-module(path_mgr).
-behaviour(gen_fsm).
-include("rep_literals.hrl").

-export([start/4]).
-export([msg/3, update/3]).
-export([init/1, first/3, version_pending/3, port_pending/3, wait_parms/3, known/3]). % call-backs
-export([handle_event/3,handle_sync_event/4]).
-export([code_change/4,handle_info/3,terminate/3]).

-record(state_data, {me, config, version, sock, 
                     fwd, him, sending_mungers=[], receiving_mungers=[]}).

start(Me, Config, Sock, Fwd) ->
    {ok, Pid} = gen_fsm:start(path_mgr, #state_data{me=Me, config=Config, sock=Sock, fwd=Fwd}, []),
    Pid.

msg(Fsm, Direction, closed) ->
    gen_fsm:sync_send_all_state_event(Fsm, {Direction, closed});

msg(Fsm, Direction, Msg) ->
    gen_fsm:sync_send_event(Fsm, {Direction, Msg}).

update(Fsm, Path, Munger) ->
    gen_fsm:send_all_state_event(Fsm, {Path, Munger}).


init(State) ->
    {ok, first, State}.

%%% First thing we see is coming from the connector, so it could be
%%% either a legacy V1 handshake, or a version proposal (which looks a
%%% lot like a V1 handshake, but has extra version info embedded in
%%% the Rec part).
%%%
first({receiving,Msg}, _From, State) ->
    RemotePort = munge:v1_handshake(Msg),
    Rec = element(5, Msg),
    case munge:versions(Rec) of
        {_Hostname, _List} when is_list(_List) ->
            NewState = State;
        _Hostname ->
            NewState = stash_port_version(State, RemotePort, 1),
            register(NewState)
    end,
    {reply, Msg, version_pending, NewState};

%%% The first thing we're seeing is coming from the acceptor of the
%%% connection.  It must be a V1 handshake, and the connection will be
%%% operated at version 1, because if the acceptor were version-aware
%%% it would wait for a version proposal from the connector.
%%% 
first({sending, V1HS}, _From, State) ->
    {reply, V1HS, port_pending, State#state_data{version=1}}.

version_pending({sending, Msg}, _From, State) ->
    {_Type, _Clen, _Rlen, _Control, Rec} = Msg,
    case munge:versions(Rec) of
        {_Hostname, 2} ->
            Version = 2,
            Next_State = wait_parms;
        {_Hostname, 3}  ->
            Version = 3,
            Next_State = wait_parms;
        {_Hostname, 4}  ->
            Version = 4,
            Next_State = wait_parms;
        _Hostname ->                             % v1
            Version = 1,
            Next_State = known
    end,
    {reply, Msg, Next_State, State#state_data{version=Version}}.

wait_parms({receiving, Msg}, _From, State) ->
    case State#state_data.version of
        2 ->
            RemotePort = munge:v2_handshake(Msg);
        3 ->
            RemotePort = munge:v3_handshake(Msg);
        4 ->
            RemotePort = munge:v4_handshake(Msg)
    end,
    NewState = stash_his_port(State, RemotePort),
    register(NewState),
    {reply, Msg, known, NewState}.

port_pending({receiving, Msg}, _From, State) ->
    RemotePort = munge:v1_handshake(Msg),
    NewState = stash_his_port(State, RemotePort),
    register(NewState),
    {reply, Msg, known, NewState}.

known({Direction, Msg}, _From, State) ->
    {reply, apply_adhocs(Msg, Direction, State), known, State}.

handle_sync_event({Direction, closed}, _From, CurrentState, State) ->
    case Direction of
        receiving ->
            Target = State#state_data.fwd,
            Adhocs = State#state_data.receiving_mungers;
        sending ->
            Target = State#state_data.sock,
            Adhocs = State#state_data.sending_mungers
    end,
    case lists:member(toss_all, Adhocs) of
        true ->
            %% If we're "tossing all", then we don't even want the
            %% other side to be able to detect a closed connection.
            %%
            %% (By the way, the old code called wedge() in this case,
            %% but that doesn't really seem right; does it?)
            %% 
            ok;
        false ->
            gen_tcp:close(Target)
    end,
    {reply, quit, CurrentState, State}.    

handle_event({Path, Munger}, CurrentState, State) ->
    {_,LocalPort} = State#state_data.me,
    {_,RemotePort} = State#state_data.him,
    case Path of
        {LocalPort,RemotePort} ->
            NewState = State#state_data{
                         sending_mungers=[Munger|
                                          State#state_data.sending_mungers]};
        {RemotePort,LocalPort} ->
            NewState = State#state_data{
                         receiving_mungers=[Munger|
                                          State#state_data.receiving_mungers]}
    end,
    {next_state, CurrentState, NewState}.


stash_port_version(State, RemotePort, Version) ->
    PartialNewState = stash_his_port(State, RemotePort),
    PartialNewState#state_data{version=Version}.

stash_his_port(State, RemotePort) ->
    {value,ConfigTuple} = 
        lists:keysearch(RemotePort, 2, State#state_data.config), % 2 == #map.real
    State#state_data{config=done, him=ConfigTuple}.

register(State) ->
    LocalPort = element(2, State#state_data.me),
    RemotePort = element(2, State#state_data.him),
    registry:register({RemotePort,LocalPort},
                      State#state_data.sock, State#state_data.fwd,
                      self()),
    registry:register({LocalPort,RemotePort},
                      State#state_data.fwd, State#state_data.sock,
                      self()).
    
apply_adhocs(Msg, Direction, State) ->
    AdhocMungers = case Direction of
                       receiving ->
                           State#state_data.receiving_mungers;
                       sending ->
                           State#state_data.sending_mungers
                   end,
    lists:foldl(fun (X, Y) -> adhoc:munge(X, Y) end, Msg, AdhocMungers).

%%%
%%% The following functions defined in the gen_fsm behavior are not
%%% used.  But we include them in order to avoid distracting
%%% compilation warnings.
%%% 
code_change(_,_,_,_) ->
    ok.

handle_info(_,_,_) ->
    ok.

terminate(_,_,_) ->
    ok.
