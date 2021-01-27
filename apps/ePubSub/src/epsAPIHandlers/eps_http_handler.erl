%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Cowboy HTTP Handler.
%%% @end
%%% Created : 26. Jan 2021 10:14 PM
%%%-------------------------------------------------------------------
-module(eps_http_handler).
-author("kirankurup@gmail.com").

-include_lib("kernel/include/logger.hrl").

%% Cowboy Callback API
-export([init/2,
  allowed_methods/2,
  options/2,
  content_types_provided/2]).

%% API
-export([to_json/2]).

%%===================================================================
%% Cowboy callbacks
%%===================================================================
%%% @doc Initialize HTTP Handler.
init(Req, []) ->
  {cowboy_rest, Req, []}.

%% @doc
%%  Which HTTP methods are allowed.
%%  Right now support only GET / HEAD / OPTIONS
%% @end
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

%% @doc OPTIONS Handler for access-control-allow-headers.
options (Req, State) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"HEAD, GET, OPTIONS">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
    <<"Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With, X-Auth-Token">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req2),
  {ok, Req3, State}.

%% @doc
%%  Which content types we handle for GET/HEAD requests
%%  Right now support only JSON requests.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}
  ], Req, State}.

%% @doc Return Aggregator values as json for the given subscriber.
to_json(Req, State) ->
  Resp = case cowboy_req:binding(sub_name, Req) of
           undefined ->
             <<"No value mentioned for subscriber">>;
           SubName ->
             case cowboy_req:binding(get_type, Req) of
               undefined ->
                 <<"Specify either aggr_result or aggr_list">>;
               GetType ->
                 case eps_config:get_sub_pid_for(SubName) of
                   {ok, SubPid} ->
                     case GetType of
                       <<"aggr_result">> ->
                         eps_sub_subscriber:get_aggr_result(SubPid);
                       <<"aggr_list">> ->
                         eps_sub_subscriber:get_aggr_msg_list(SubPid);
                       _ ->
                         <<"Unidentified value for get_type">>
                     end;
                   {error, _} -> <<"This subscriber is not present">>
                 end
             end
         end,
  {jsone:encode(Resp), Req, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
