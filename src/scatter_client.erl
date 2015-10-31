-module('scatter_client').

%% Housekeeping.
-export([start/0, start/1]).

%% API exports.
-export([ 
          version/0
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
-spec version() -> jsx:json_text().
version() -> 
  URL = api_url("/info/version"),
  lager:info(URL),
  parse_result(ibrowse:send_req(URL, [], get)).


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
  
