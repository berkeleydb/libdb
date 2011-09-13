%%% Split the rec part of a possible version negotiation message into
%%% the host name and a list of version integers.

-module(util).
-export([versions/1, rep_msg_type/1]).
-include("rep_literals.hrl").

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

%%% In release 4.7 of Berkeley DB the replication msg protocol was
%%% changed.  Previously it sent binary data in native format; now it
%%% sends it in standard NBO.  Each message is tagged with a version
%%% number, identifying its format.  Note that this presents a bit of
%%% a "chicken-and-egg" problem: you need to know the version before
%%% you can figure out how to read any of the data, *INCLUDING THE
%%% VERSION NUMBER ITSELF*!  However, we can get around this issue by
%%% assuming native format.  If we're on a big endian architecture, it
%%% doesn't make any difference.  If we're on a little endian, then if
%%% we're wrong to assume native, then the number we see will look
%%% like some huge number, with bits turned on in the high-order
%%% byte.  This should only happen with formats >= 4.7, so we'll take
%%% the correct branch.  (See also the comment in
%%% __rep_process_message, rep/rep_record.c)
%%%
rep_msg_type(Control) ->
    <<MsgVersion:32/native, Remainder/binary>> = Control,
    if
        MsgVersion >= ?REPVERSION_47 ->
            <<_:3/unit:32, RecType:32/big, _/binary>> = Remainder;
        true ->
            <<_:3/unit:32, RecType:32/native, _/binary>> = Remainder
    end,
    RecType.
