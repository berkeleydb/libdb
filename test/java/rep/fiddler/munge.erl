-module(munge).
-include("rep_literals.hrl").
-export([v1_handshake/1, v2_handshake/1, v3_handshake/1, v4_handshake/1]).
-export([versions/1]).

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

v1_handshake(Msg) ->
    {?HANDSHAKE, _ControlLength, _RecLength, Control, _Rec} = Msg,
    <<_Version:32/native, Port:16/native, _:16, _Prio:32/big>> = Control,
    Port.

v2_handshake(Msg) ->
    {?HANDSHAKE, _ControlLength, _RecLength, Control, _Rec} = Msg,
    <<Port:16/big, _Prio:32/big>> = Control,
    Port.

v3_handshake(Msg) ->
    {?HANDSHAKE, _ControlLength, _RecLength, Control, _Rec} = Msg,
    <<Port:16/big, _Prio:32/big, _Flags:32/big>> = Control,
    Port.

v4_handshake(Msg) ->
    {?HANDSHAKE, _ControlLength, _RecLength, Control, _Rec} = Msg,
    <<Port:16/big, _Alignment:16/big, _AckPolicy:32/big, _Flags:32/big>> = Control,
    Port.

