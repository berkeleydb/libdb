-module(munge).
-include("rep_literals.hrl").
-export([v1_handshake/2, v2_handshake/2, v3_handshake/2]).
-export([versions/1,real_to_spoofed/2, maybe_spoofed_to_real/2]).

versions(B) ->
    Host = lists:takewhile(fun (X) -> X /= 0 end, binary_to_list(B)),
    Skip = length(Host) + 1,
    case B of 
        %%
        %% version negotiation proposal
        %% 
        <<_H:Skip/binary, Vmin:32/big, Vmax:32/big,
         _Pad/binary>> ->
            {Host, [Vmin, Vmax]};

        %%
        %% version confirmation
        %% 
        <<_H:Skip/binary, V:32/big, _Pad/binary>> ->
            {Host, V};

        %%
        %% legacy V1 handshake, with no extra version info
        %% 
        _ ->
            Host
    end.

%%% Map might be Me (a tuple) or Config (a list of tuples)
%%%
v1_handshake(Msg, Map) ->
    {?HANDSHAKE, ControlLength, RecLength, Control, Rec} = Msg,
    <<Version:32/native, Port:16/native, _:16, Prio:32/big>> = Control,
    NewPort = real_to_spoofed(Port, Map),
    NewControl = <<Version:32/native, NewPort:16/native, 0:16, Prio:32/big>>,
    NewMsg = {?HANDSHAKE, ControlLength, RecLength, NewControl, Rec},
    {Port, NewMsg}.

v2_handshake(Msg, Map) ->
    {?HANDSHAKE, ControlLength, RecLength, Control, Rec} = Msg,
    <<Port:16/big, Prio:32/big>> = Control,
    NewPort = real_to_spoofed(Port, Map),
    NewControl = <<NewPort:16/big, Prio:32/big>>,
    NewMsg = {?HANDSHAKE, ControlLength, RecLength, NewControl, Rec},
    {Port, NewMsg}.

v3_handshake(Msg, Map) ->
    {?HANDSHAKE, ControlLength, RecLength, Control, Rec} = Msg,
    <<Port:16/big, Prio:32/big, Flags:32/big>> = Control,
    NewPort = real_to_spoofed(Port, Map),
    NewControl = <<NewPort:16/big, Prio:32/big, Flags:32/big>>,
    NewMsg = {?HANDSHAKE, ControlLength, RecLength, NewControl, Rec},
    {Port, NewMsg}.


%%% rcv handshake: real_to_spoofed(Config)
%%% send handshake: real_to_spoofed(Me)
%%%
%%% rcv NEWCLIENT: real_to_spoofed(Him)
%%% send NEWCLIENT: real_to_spoofed(Me)
%%%
%%% rcv NEWSITE: maybe_spoofed_to_real(Me)
%%% snd NEWSITE: maybe_spoofed_to_real(Him)

maybe_spoofed_to_real(Spoofed, {Spoofed,Real}) ->
    Real;

maybe_spoofed_to_real(X, _) ->
    X.

real_to_spoofed(Real, {Spoofed, Real}) ->
    Spoofed;

real_to_spoofed(Real, Config) when is_list(Config) ->
    {value, {Spoofed, _Real}} = lists:keysearch(Real, 2, Config),
    Spoofed.

