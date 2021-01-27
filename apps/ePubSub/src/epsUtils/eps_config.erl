%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Handler for all configuration parameters.
%%%   Implemented as a gen_server, so that lookup can be performed
%%%   faster irrespective of which process is invoking it.
%%% @end
%%% Created : 24. Jan 2021 9:30 PM
%%%-------------------------------------------------------------------
-module(eps_config).
-author("kirankurup@gmail.com").

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("eps_defaults.hrl").
-include("eps_macros.hrl").

%% API
-export([start_link/0, lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eps_config_state, {conf_data}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Lookup with Key.
%%    Returns value associated with Key.
%%    If Key not found default value is returned.
%% @end
lookup(Key, DefValue) ->
  gen_server:call(?MODULE, {lookup, Key, DefValue}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
  process_flag(trap_exit, true),
  Conf = load_conf_data (),
  ?LOG_INFO("All default configuration loaded ~p~n", [Conf]),
  {ok, #eps_config_state{conf_data = Conf}}.

%% @private
%% @doc Handling call messages

%% @doc Handle all lookup invocations to retrieve the date from our record.
handle_call ({lookup, Key, DefValue}, _From, State = #eps_config_state{}) ->
  ?LOG_DEBUG ("lookup for key ~p~n", [Key]),
  Reply = case lists:keyfind(Key, 1, State#eps_config_state.conf_data) of
            {Key, Val} ->
              Val;
            false ->
              DefValue
          end,
  {reply, Reply, State};
%% @doc Handle all other call messages.
handle_call(_Request, _From, State = #eps_config_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Request, State = #eps_config_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State = #eps_config_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
terminate(_Reason, _State = #eps_config_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #eps_config_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Loads all configuration data from sys.config / application env variables.
%% @end
load_conf_data () ->
  [ {?APPL_RUN_AS,            ?GET_ENV(?APPLICATION, ?APPL_RUN_AS, ?DEFAULT_APPL_RUN_AS)},
    {?AMQSERVER,              ?GET_ENV(?APPLICATION, ?AMQSERVER, ?DEFAULT_AMQSERVER)},
    {?AMQSERVER_PORT,         ?GET_ENV(?APPLICATION, ?AMQSERVER_PORT, ?DEFAULT_AMQSERVER_PORT)},
    {?PUBLISHER_RATE,         ?GET_ENV(?APPLICATION, ?PUBLISHER_RATE, ?DEFAULT_PUBLISHER_RATE)},
    {?PUBLISHER_RANGE,        ?GET_ENV(?APPLICATION, ?PUBLISHER_RANGE, ?DEFAULT_PUBLISHER_RANGE)},
    {?EPS_TOPIC,              ?GET_ENV(?APPLICATION, ?EPS_TOPIC, ?DEFAULT_EPS_TOPIC)},
    {?SUBSCRIBER_SLEEP_INTVL, ?GET_ENV(?APPLICATION, ?SUBSCRIBER_SLEEP_INTVL, ?DEFAULT_SUBSCRIBER_SLEEP_INTVL)}].