-module('scatter_client').
-include_lib("eunit/include/eunit.hrl").


%% Housekeeping.
-export([start/0, start/1]).

%% API exports.
-export([ 
          version/0
        , compile/5
        ]).

%% Defaults.
-define(DEFAULT_URL, "http://localhost:9911").
-define(DEFAULT_LOGLEVEL, info).


%%====================================================================
%% Housekeeping functions
%%====================================================================

-spec start() -> ok.
start() -> 
  start(#{}).

%% @doc Start and configure the scatter_client.  The only useful
%% configuration values are:
%%   url (default http://localhost:9911/)
%%   loglevel (default info)
-spec start(map()) -> ok.
start(Options) -> 
  %% configure logging
  LogLevel = maps:get(loglevel, Options, ?DEFAULT_LOGLEVEL),
  lager:start(),
  lager:set_loglevel(lager_console_backend, LogLevel),
  %% configure client url
  URL = maps:get(url, Options, ?DEFAULT_URL),
  scatter_client_config:start(),
  scatter_client_config:set(url, URL),
  lager:info("Communicating with server ~s", [URL]),
  %%
  application:ensure_started(ibrowse).
  
  
%%====================================================================
%% API functions
%%====================================================================

%% @doc Query scatter for its API version.
-spec version() -> term().
version() -> 
  URL = api_url("/info/version"),
  parse_result(ibrowse:send_req(URL, [], get)).

-spec compile(string(), string(), atom(), string(), map()) -> term().
compile(RequestID, UserID, Language, Code, Options) -> 
  Request = #{ user_id    => UserID,
               language   => Language,
               code       => Code,
               options    => Options },
  RequestBody = jsx:encode(Request),
  Headers = [{accept, "application/json"}, 
             {"content-type", "application/json"}],
  URL = api_url("/compile/" ++ RequestID),
  parse_result(ibrowse:send_req(URL, Headers, put, RequestBody)).
  


%%====================================================================
%% Internal functions
%%====================================================================
api_url(Path) -> 
  {ok, URL} = scatter_client_config:get(url),
  lists:flatten(URL ++ Path).

parse_result({ok, [$2|_]=StatusCode, Headers, Body}) ->
  BinaryBody = list_to_binary(Body),
  case jsx:is_json(BinaryBody) of
    true ->  {ok,             StatusCode, jsx:decode(BinaryBody), Headers};
    false -> {{ok, not_json}, StatusCode, Body,                   Headers}
  end;
parse_result({ok, StatusCode, Headers, Body}) -> 
  {server_error, StatusCode, Body, Headers}.
  
