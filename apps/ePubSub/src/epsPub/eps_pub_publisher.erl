%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2021 4:27 PM
%%%-------------------------------------------------------------------
-module(eps_pub_publisher).
-author("kirankurup@gmail.com").

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("eps_defaults.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eps_pub_publisher_state, {
  hostname,               %% Host to which we need to connect (where ActiveMQ is running).
  port,                   %% Port where connection to ActiveMQ server has to be made.
  topic_name,             %% Topic to which this process will subscribe to.
  conn,                   %% Connection object.
  rate,                   %% Rate @ which messages need to be published.
  pub_range,              %% Publisher data range. Integers will randomly drawn from this range.
  sleep_interval,         %% Sleep time before next message is sent out (1000ms/rate).
  pid,                    %% Process Id of the current subscriber.
  timer_ref               %% Timer Reference
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([AMQServer, AMQPort, TopicName, PubRate, PubRange]) ->
  %% To receive a message when this process dies.
  process_flag(trap_exit, true),
  Pid = self(),
  ?LOG_INFO("Connecting Publisher to ~ts:~tp with Rate: ~tp", [AMQServer, AMQPort, PubRate]),
  %% Connect to ActiveMQ Server:Port
  case stomp:connect(AMQServer, AMQPort, "", "") of
    {error, Err} ->
      ?LOG_ERROR("Unable (~tp) to connect Publisher to ActiveMQ Server (~ts:~tp)", [Err, AMQServer, AMQPort]),
      {stop, Err};
    Conn ->
      %% Start a timer with initial delay, so that publisher will start sending data only after this cushion period.
      timer:send_after(?INITIAL_DELAY, Pid, initial_delay),
      {ok, #eps_pub_publisher_state{hostname = AMQServer, port = AMQPort, topic_name = TopicName,
        conn = Conn, rate = PubRate, pub_range = PubRange, sleep_interval = 1000/PubRate,
        pid = Pid, timer_ref = ''}}
  end.

%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State = #eps_pub_publisher_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Request, State = #eps_pub_publisher_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages

%% @private
%%--------------------------------------------------------------------
%% @doc
%%  Timeout handler for initial delay.
%%    --> Publishes the message to broker and sleep to achieve the rate
%%    --> Store the new timer reference.
%% @end
%%--------------------------------------------------------------------
handle_info(initial_delay, State = #eps_pub_publisher_state{}) ->
  % Start publishing data @ the rate configured.
  TimerRef = publish_message_and_sleep(State),
  {noreply, State#eps_pub_publisher_state{timer_ref = TimerRef}};
%%--------------------------------------------------------------------
%% @doc
%%  Timeout handler for publish trigger.
%%    --> Cancels the existing timer reference.
%%    --> Publishes the message to broker and sleep to achieve the rate
%%    --> Store the new timer reference.
%% @end
%%--------------------------------------------------------------------
handle_info(publish_trigger, State = #eps_pub_publisher_state{}) ->
  % Start publishing data @ the rate configured.
  TimerRef = publish_message_and_sleep(State),
  {noreply, State#eps_pub_publisher_state{timer_ref = TimerRef}};
%% @private
%% @doc Handling other non call/cast messages
handle_info(_Info, State = #eps_pub_publisher_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
terminate(_Reason, State = #eps_pub_publisher_state{}) ->
  % Cancels the timer reference.
  timer:cancel(State#eps_pub_publisher_state.timer_ref),
  % Disconnect the connection to ActiveMQ Server.
  stomp:disconnect(State#eps_pub_publisher_state.conn),
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #eps_pub_publisher_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Publish message to Topic.
%%  Sleeps for the stipulated time, so that the rate is maintained.
%%  Returns timer reference to be stored in the state variable.
%% @end
%%--------------------------------------------------------------------
publish_message_and_sleep(State) ->
  Bef = trunc(epsUtils:get_timestamp() / 1000),
  % Cancels the timer reference.
  timer:cancel(State#eps_pub_publisher_state.timer_ref),
  % Create the message to be published.
  Msg = integer_to_list(rand:uniform(State#eps_pub_publisher_state.pub_range)),
  ?LOG_INFO("Sending message...~ts", [Msg]),
  % Send out the message.
  stomp:send(State#eps_pub_publisher_state.conn, State#eps_pub_publisher_state.topic_name, [], Msg),
  TimeInMS = trunc(epsUtils:get_timestamp() / 1000) - Bef,
  % Recalculate the new sleep time based on the time spend in sending the message.
  NewSleepTime = trunc(State#eps_pub_publisher_state.sleep_interval - TimeInMS),
  % Start new timer and return the timerRef
  case timer:send_after(NewSleepTime, State#eps_pub_publisher_state.pid, publish_trigger) of
    {ok, TRef} -> TRef;
    {error, _} -> ''
  end.