-module(adhoc).
-export([munge/2]).

-include("rep_literals.hrl").

munge(Name, Msg) ->
    case Name of
        toss_all ->
            toss_all(Msg);
        page_clog ->
            page_clog(Msg);
        _ ->
            Msg
    end.

toss_all(_Msg) ->
    nil.

wedge() ->
    receive
        no_such_msg ->
            this_will_never_happen
    end.

page_clog(Msg) ->
    case Msg of
        {?REP_MESSAGE, _, _, Control, _} ->
            case util:rep_msg_type(Control) of
                ?PAGE ->
                    wedge();
                _ ->
                    Msg
            end;
        _ ->
            Msg
    end.
