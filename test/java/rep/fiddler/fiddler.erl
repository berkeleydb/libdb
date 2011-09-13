%%% TODO: need to figure out this 'inet6' setting in the tcp options:
%%% It (and *not* 'inet') seems to be necessary when I run on Mac.
%%% Things work with neither setting on Linux and Windows -- not sure
%%% if things work there with the setting though.

%%% When the manager receives a command to install a munger, it can
%%% only affect an existing connection, because the way it works is to
%%% send the info to the registered path_mgr processes.  It doesn't
%%% save up a record of active mungers to bequeath to any connections
%%% that become established after that.
%%%
%%% I think that's a shortcoming that should be fixed.
%%%
%%% We might also need a way to remove a munger, although not
%%% currently.

%%% Hmm, is there any reason why the various path pairs need be
%%% related to each other?  Would it make sense to start [{7001,6001},
%%% {7000,6000}] first, and then later add {7002,6002} to the chaos?
%%% I think that might be fine, but only if the site at 6002 truly
%%% isn't running yet, because if it is, it could send a message that
%%% would need munging.  Similarly, it's important that no running
%%% site have that in its configuration yet, even if the new site
%%% isn't running yet.
%%%
%%% It's not needed yet, but I could imagine a need to target a
%%% command to an as yet unestablished connection/path.  Perhaps it
%%% could be directed to 'new'(?), or {6001,6000,new} (?).  Somewhere
%%% we'd need to hold onto that list, and give it (or some part of it)
%%% to new connections as they're getting established.  The point is,
%%% if you wait until after the connection is established and the
%%% sites have talked to each other, it might be too late to do what
%%% you need to do.


%%% Need to be able to:
%%%   2. remove previously installed munges (shall we give each one a
%%%      serial number or something?)
%%%   3. sometimes it's a little more complicated, and we need a way
%%%      to send a message or signal to an existing munge and/or even
%%%      the recv/send-er (in the case of socket congestion blockage,
%%%      when we want to free it up again).

%%% An interesting feature, that could be useful, maybe even
%%% essential: we could keep a record of all connections, even after
%%% they've been closed, for the purpose of analysis by the test.
%%% Obviously they would be keyed by site-pair (as expressed by port
%%% numbers, similarly to how we addressing path-specific munger
%%% commands).  But they could also include establishment timestamp,
%%% so that a test could distinguish between possibly multiple paths
%%% between the same endpoints.
%%%
%%% With that, a command could request that a path count various
%%% statistics about the messages that pass through (e.g., did we see
%%% any heartbeats on this connection?  We shouldn't, if we're in
%%% mixed-version mode.  Although I suppose an old site would simply
%%% crash if we made that mistake.)  A later command could then come
%%% back and request these statistics.

%% Also, it may be that the forward port isn't listening yet, in which
%% case we could get econnrefused.  Again, it's not terribly
%% disastrous to just let the error report close the connection.  But
%% we should handle it.  Besides, better than closing the initiating
%% connection might be to not even start listening until we see that
%% our forward port is listening.  That's more difficult, and could
%% conceivably bother the target site, but it's more realistic in
%% terms of what the connecting site should see.  Hmm, hard to know
%% which way is better, and whether it's even worth worrying about.
%%
%% Well, here's another thing to think about: it probably doesn't work
%% merely to avoid listening in the beginning, because the site could
%% die in the middle.  We certainly want ultimately to be able to test
%% those kinds of situations.  Hmm, at least for now, perhaps it's all
%% right just to close the incoming connection when we can't make the
%% outgoing connection, because repmgr doesn't really make much
%% difference between an EOF and an error.  But someday it might make
%% a difference.
%%
%% Test functions:
%% - stop reading (to block progress)
%% - discard everything (heartbeats)
%% - discard acks
%% - delay acks
%%
%% Each pair of sites (each link we could care about controlling) has
%% two possible ways it might get set up (in the most general case,
%% though often it's easy enough to control it more strictly).
%%
%% A munge function can be specified as applying only to a specified
%% path, or to all paths.  For example,
%%
%%     {{6000,6001},page_clog}
%%
%% says to apply the page_clog function to the path going from the
%% site listening on 6000 to the site listening on 6001 (both expressed
%% as real port numbers, not the spoofed port numbers).
%%
%%
%% There's another, rather different way of looking at how this gets
%% configured: instead of each site having one fiddler "sheilding" its
%% incoming connections, you could have fiddlers at just one site,
%% completely "wrapping" it, so that it takes not only all incoming
%% connections, but outgoing connections as well.  Consequences: (1)
%% it really focuses on that one site, making it the site under test
%% -- all the others are just going along for the ride.  You could
%% even imagine making them fake.  (2) Other sites talk directly to
%% each other, so we have no control over traffic between them.
%%
%% Site A:
%%     local:  6000
%% Site B:
%%     local:  6001
%%     remote: 7000
%% Site C:
%%     local:  6002
%%     remote: 6000
%% [{7001,6001},{7000,6000},{7002,6002}
%%
%% However, this might be confusing, and is probably counter to some
%% of the higher-level assumptions I've made in setting up
%% transformations.


-module(fiddler).

-export([start/1, start/2, main/2, do_accept/3, slave/4]).
-import(lists,[keysearch/3,foreach/2]).
-import(gen_tcp,[listen/2,accept/1,recv/2,connect/3,send/2]).

-define(MANAGER_PORT, 8000).

-include("rep_literals.hrl").

%% Config is a list of {spoofed,real} port numbers.  (For now
%% everything's on localhost.)
%%
%% TODO: shouldn't we use records for those tuples?

start(Config) ->
    registry:start(),
    manager:start(?MANAGER_PORT, Config),
    start_up(Config, Config).

start(MgrPort, Config) ->
    registry:start(),
    manager:start(MgrPort, Config),
    start_up(Config, Config).

%% For each pair specified in the config, spawn off a listener to
%% spoof the given pair, passing it its own pair, plus the total list
%% of all pairs.
%% 
start_up([H|T], Config) ->
    spawn(fiddler, main, [H, Config]),
    start_up(T, Config);
start_up([], _Config) ->
    ok.

main(Me, Config) ->
    {Spoofed, _Real} = Me,
    {ok, LSock} = listen(Spoofed, 
                         [binary, inet, inet6, {packet,raw}, {active, false}, {reuseaddr, true}]),
    do_accept(Me, Config, LSock).

do_accept(Me, Config, LSock) ->
    {_Spoofed, Real} = Me,
    {ok, Sock} = accept(LSock),
    {ok, TargetSock} = connect("localhost", Real, 
                               [binary, inet, inet6, {packet, raw}, {active, false}]),
    Mgr = path_mgr:start(Me, Config, Sock, TargetSock),
    spawn(fiddler, slave, [Sock, TargetSock, Mgr, receiving]),
    spawn(fiddler, slave, [TargetSock, Sock, Mgr, sending]),
    do_accept(Me, Config, LSock).


slave(Sock, Fwd, Manager, Direction) ->
    MsgResult = (catch get_one(Sock)),
    %% If we only have a 4-part, or 2-part, msg result, it's one of
    %% the new types, and so there's no fiddling to be done with it.
    MsgToSend = case MsgResult of
                    closed -> path_mgr:msg(Manager, Direction, MsgResult);
                    {_,_,_,_,_} -> path_mgr:msg(Manager, Direction, MsgResult);
                    {_,_,_,_} -> MsgResult;
                    {_,_,_} -> MsgResult
                end,
    case MsgToSend of
        quit ->
            ok;
        Msg ->                                  % or should I construct {ok,Msg}??
            send_msg(Fwd, Msg),
            slave(Sock, Fwd, Manager, Direction)
    end.

send_msg(Sock, {MsgType, ControlLength, RecLength, Control, Rec}) ->
    Header = <<MsgType:8, ControlLength:32/big, RecLength:32/big>>,
    send(Sock, Header),
    send_piece(Sock, Control),
    send_piece(Sock, Rec);
send_msg(Sock, {MsgType, Length, Other, Data}) ->
    Header = <<MsgType:8, Length:32/big, Other:32/big>>,
    send(Sock, Header),
    send_piece(Sock, Data);
send_msg(Sock, {MsgType, Length, Other}) ->
    Header = <<MsgType:8, Length:32/big, Other:32/big>>,
    send(Sock, Header);
send_msg(_Sock, nil) ->
    ok.

send_piece(_Sock, nil) ->
    ok;
send_piece(Sock, Piece) ->
    send(Sock, Piece).

get_one(Sock) ->
    case recv(Sock, 9) of
        {ok, B} ->
            <<MsgType, Word1:32, Word2:32>> = B,
            case MsgType of
                ?ACK -> tradition_msg(Sock, MsgType, Word1, Word2);
                ?HANDSHAKE -> tradition_msg(Sock, MsgType, Word1, Word2);
                ?REP_MESSAGE -> tradition_msg(Sock, MsgType, Word1, Word2);
                ?HEARTBEAT -> tradition_msg(Sock, MsgType, Word1, Word2);
                ?APP_MSG -> simple_msg(Sock, MsgType, Word1, Word2);
                ?APP_RESPONSE -> simple_msg(Sock, MsgType, Word1, Word2);
                ?RESP_ERROR -> {Sock, MsgType, Word1, Word2};
                ?OWN_MSG -> simple_msg(Sock, MsgType, Word1, Word2)
            end;
        {error, closed} ->
            throw(closed);
        {error,enotconn} ->
            throw(closed)
    end.

tradition_msg(Sock, MsgType, ControlLength, RecLength) ->
    Control = get_piece(Sock, ControlLength),
    Rec = get_piece(Sock, RecLength),
    {MsgType, ControlLength, RecLength, Control, Rec}.

simple_msg(Sock, MsgType, Length, Word2) ->
    Msg = get_piece(Sock, Length),
    {MsgType, Length, Word2, Msg}.

get_piece(Sock, Length) ->    
    if
        Length == 0 ->
            nil;
        Length > 0 ->
            case recv(Sock, Length) of
                {ok, Piece} ->
                    Piece;
                {error, closed} ->
                    throw(closed);
                {error,enotconn} ->
                    throw(closed)
            end
    end.
