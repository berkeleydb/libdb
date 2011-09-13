%%% The Registry is a gen_server process that keeps track of the
%%% currently active set of connection-path processes.  The listeners
%%% add the processes to the list as they're created.  The manager
%%% looks stuff up in order to send commands to the proper place.
%%%
%%% For each path we know the two sockets involved, and the
%%% process(es) reading from them.
%%% 
%%% A munger looks like either a simple atom (if the rule applies to
%%% any and all connections paths), or an atom tagged with a path ID
%%% (like {{6001,6000},page_clog}), which means the rule only applies
%%% to that path.
%%% 
%%% Note that "Count" is a count of the number of paths we've *ever*
%%% registered, not the current length of the list.  (Otherwise I
%%% would have simply called lists:length/1 when needed.)

-module(registry).
-behaviour(gen_server).
-export([start/0,lookup/1,register/4,unregister/1,all/0]).
-export([init/1, handle_call/3]).
-export([code_change/3,handle_cast/2,handle_info/2,terminate/2]).

start() ->
    {ok,_Pid} = gen_server:start({local,registry}, registry, [], []),
    ok.

init(_) ->
    {ok,{[],0}}.


%%% TODO: see manager.erl - I think we should get rid of sock and fwd
%%% from here.

handle_call({register,Path,Sock,Fwd, Pid}, _From, State) ->
    {Paths,Count} = State,
    {reply, ok, {[{Count,Path,Sock,Fwd,Pid}|Paths],Count+1}};

handle_call({lookup,Query}, _, State) ->
    {Paths,_Count} = State,
    Index = if
                is_tuple(Query) ->
                    2;                          % Path desc, like {6001,6000}
                true ->
                    1                           % unique serial ID number
            end,
    case lists:keysearch(Query, Index, Paths) of
        {value, Tpl} ->
            {reply, {ok,Tpl}, State};
        false ->
            {reply, notfound, State}
    end;

handle_call({unregister,Path}, _, State) ->
    {Paths,Count} = State,
    {reply, ok, {lists:keydelete(Path, 2, Paths),Count}};

handle_call(all, _, State) ->
    {Paths,_Count} = State,
    {reply, Paths, State}.

lookup(Query) ->
    gen_server:call(registry, {lookup, Query}).

register(Path, Sock, Fwd, Pid) ->
    gen_server:call(registry, {register, Path, Sock, Fwd, Pid}).

unregister(Path) ->
    gen_server:call(registry, {unregister, Path}).

all() ->
    gen_server:call(registry, all).

%%%
%%% The following functions defined in the gen_server behavior are not
%%% used.  But we include them in order to avoid distracting
%%% compilation warnings.
%%% 
code_change(_,_,_) ->
    ok.

handle_cast(_,_) ->
    ok.

handle_info(_,_) ->
    ok.

terminate(_,_) ->
    ok.
