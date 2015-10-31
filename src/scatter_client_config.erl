-module(scatter_client_config).
-behaviour(gen_server).

%% API
-export([start/0, get/1, set/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec set(Key, Value) -> {Key, Value}.
set(Key, Value) -> 
  gen_server:call(?SERVER, {set, Key, Value}).

-spec get(term()) -> {ok, term()} | error.
get(Key) -> 
  gen_server:call(?SERVER, {get, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #{}}.

handle_call({set, Key, Value}, _From, State) ->
  NewState = maps:put(Key, Value, State),
  {reply, {Key, Value}, NewState};
handle_call({get, Key}, _From, State) ->
  {reply, maps:find(Key, State), State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
