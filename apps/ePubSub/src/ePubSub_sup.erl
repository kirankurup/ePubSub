%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%  ePubSub top level supervisor.
%%% @end
%%% Created : 23. Jan 2021 3:50 PM
%%%-------------------------------------------------------------------
-module(ePubSub_sup).
-author("kirankurup@gmail.com").

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").
-include("eps_defaults.hrl").

-export([start_link/0, start_subscribers/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc Spawns the Supervisor and other dependencies.
start_link() ->
  %% Start EPS Config gen_server.
  eps_config:start_link(),
  RunMode = eps_config:lookup(?APPL_RUN_AS, ?DEFAULT_APPL_RUN_AS),
  case RunMode of
    "subscriber" ->
      %% Start subscribers after a small interval.
      timer:apply_after(1000, ?SERVER, start_subscribers, []);
    "both" ->
      %% Start subscribers after a small interval.
      timer:apply_after(1000, ?SERVER, start_subscribers, []);
    "publisher" ->
      ok
  end,
  %% Start the supervisor module.
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
%% @private
%% @doc Initializes the supervisor / temporary workers
init([Module]) ->
  %% Supervisor flags.
  SupFlags = #{strategy => simple_one_for_one,
    intensity => ?SUPERVISOR_MAX_RESTART,
    period => ?SUPERVISOR_MAX_TIME},
  %% Subscriber spec
  SubSpec = #{id => undefined,
    start => {Module, start_link, []},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modules => []},
  ChildSpecs = [SubSpec],
  {ok, {SupFlags, ChildSpecs}};
init([]) ->
  %% Constants needed to load the processes.
  RunMode = eps_config:lookup(?APPL_RUN_AS, ?DEFAULT_APPL_RUN_AS),
  AMQServer = eps_config:lookup(?AMQSERVER, ?DEFAULT_AMQSERVER),
  AMQPort = eps_config:lookup(?AMQSERVER_PORT, ?DEFAULT_AMQSERVER_PORT),
  TopicName = eps_config:lookup(?EPS_TOPIC, ?DEFAULT_EPS_TOPIC),
  PubRate = eps_config:lookup(?PUBLISHER_RATE, ?DEFAULT_PUBLISHER_RATE),
  PubRange = eps_config:lookup(?PUBLISHER_RANGE, ?DEFAULT_PUBLISHER_RANGE),

  %% Supervisor flags.
  SupFlags = #{strategy => one_for_one,
    intensity => ?SUPERVISOR_MAX_RESTART,
    period => ?SUPERVISOR_MAX_TIME},
  %% --- Specifications
  %% Publisher Spec.
  PubSpec = #{id => eps_pub_publisher,
    start => {eps_pub_publisher, start_link, [[AMQServer, AMQPort, TopicName, PubRate, PubRange]]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [eps_pub_publisher]},
  %% Subscriber Supervisor Spec.
  SubSupSpec = #{id => eps_sub_supervisor,
    start => {supervisor, start_link, [{local, eps_sub_supervisor}, ?MODULE, [eps_sub_subscriber]]},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => []},
  % Select the Specs based on the run_mode configured.
  ChildSpecs = case RunMode of
                 "subscriber" -> [SubSupSpec];
                 "publisher"  -> [PubSpec];
                 "both"       -> [SubSupSpec, PubSpec]
               end,
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
%%--------------------------------------------------------------------
%% @doc
%%  Start the subscribers here.
%% @end
%%--------------------------------------------------------------------
start_subscribers() ->
  %% Constants needed to load subscribers.
  Dispatch = cowboy_router:compile([
    {'_', [{"/[:sub_name]/[:get_type]", eps_http_handler, []}]}
  ]),

  {ok, _} = cowboy:start_clear(eps_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ),

  AMQServer = eps_config:lookup(?AMQSERVER, ?DEFAULT_AMQSERVER),
  AMQPort = eps_config:lookup(?AMQSERVER_PORT, ?DEFAULT_AMQSERVER_PORT),
  TopicName = eps_config:lookup(?EPS_TOPIC, ?DEFAULT_EPS_TOPIC),
  SleepInterval = eps_config:lookup(?SUBSCRIBER_SLEEP_INTVL, ?DEFAULT_SUBSCRIBER_SLEEP_INTVL),
  SubNames = ["subscriber_1", "subscriber_2"],
  AggrFns = [calculate_sum, calculate_median],
  lists:map(fun({SubName, AggrFn}) ->
    Res = supervisor:start_child(eps_sub_supervisor, [[AMQServer, AMQPort, SubName, TopicName, SleepInterval, AggrFn]]),
    case Res of
      {ok, Pid} -> eps_config:add_sub_pid_for(list_to_binary(SubName), Pid);
      {error, Err} -> ?LOG_ERROR("Error (~p) while creating subscriber process for ~p~n", [Err, SubName])
    end
            end,
    lists:zip(SubNames, AggrFns)).
