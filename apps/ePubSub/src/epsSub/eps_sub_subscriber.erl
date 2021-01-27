%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   All Subscriber related handler functions.
%%%   Connects to ActiveMQ broker, retrieves the messages, stores them
%%%   and at regular intervals run an aggregator function on the stored
%%%   messages.
%%% @end
%%% Created : 24. Jan 2021 12:43 AM
%%%-------------------------------------------------------------------
-module(eps_sub_subscriber).
-author("kirankurup@gmail.com").

-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include("eps_defaults.hrl").
%% API
-export([start_link/1,
  get_aggr_msg_list/1,
  get_aggr_result/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eps_sub_subscriber_state, {
  hostname,               %% Host to which we need to connect (where ActiveMQ is running).
  port,                   %% Port where connection to ActiveMQ server has to be made.
  sub_name,               %% Subscriber Name to be used while creating the connection.
  topic_name,             %% Topic to which this process will subscribe to.
  conn,                   %% Connection object.
  msg_list,               %% Storage for all the incoming messages to this subscriber.
  pid,                    %% Process Id of the current subscriber.
  aggr_fn,                %% Aggregator Function.
  agg_msg_list,           %% Messages list on which aggregator function was run recently.
  aggr_result,            %% Current result of aggregator function.
  sleep_interval,         %% Sleep time before aggregated function need to be run.
  timer_ref               %% Timer Reference
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% @doc
%%  Retrieves the Message list on which aggregator function was run recently
%%  Returns list.
%% @end
get_aggr_msg_list(Pid) ->
  gen_server:call(Pid, retrieve_aggr_msg_list).

%% @doc
%%  Retrieves the aggregator function result.
%%  Returns integer / float result.
%% @end
get_aggr_result(Pid) ->
  gen_server:call(Pid, retrieve_aggr_result).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([AMQServer, AMQPort, SubName, TopicName, SleepInterval, AggrFn]) ->
  Pid = self(),
  %% Start a timer with initial delay.
  timer:send_after(?INITIAL_SUB_DELAY, Pid, initial_sub_delay),
  ?LOG_INFO("Starting Subscriber ~p with sleep interval ~p... Connecting to ~s:~p, ~n",
    [SubName, SleepInterval, AMQServer, AMQPort]),
  {ok, #eps_sub_subscriber_state{hostname = AMQServer, port = AMQPort, sub_name = SubName,
    topic_name = TopicName, conn = '', msg_list = [], pid = Pid, aggr_fn = AggrFn,
    agg_msg_list = [], aggr_result = '', sleep_interval = SleepInterval, timer_ref = ''}}.

%% @private
%% @doc Handling call messages

%% @doc Handler for retrieving aggr_msg_list
handle_call(retrieve_aggr_msg_list, _From, State = #eps_sub_subscriber_state{}) ->
  {reply, State#eps_sub_subscriber_state.agg_msg_list, State};

%% @doc Handler for retrieving aggr_result
handle_call(retrieve_aggr_result, _From, State = #eps_sub_subscriber_state{}) ->
  {reply, State#eps_sub_subscriber_state.aggr_result, State};

%% @doc Handling all other call messages
handle_call(_Request, _From, State = #eps_sub_subscriber_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages

%% @private
%% @doc
%%  Handle incoming messages to this subscriber.
%%    --> All the incoming messages are added to the list.
%%    --> Wait again for incoming messages.
%% @end
handle_cast({inc_subs_msg, Messages}, State = #eps_sub_subscriber_state{}) ->
  %% Handle incoming messages by storing in the list variable
  NewState = handle_inc_messages(Messages, State),
  %% Subscribe for new messages.
  subs_for_inc_msgs(State#eps_sub_subscriber_state.conn,
    State#eps_sub_subscriber_state.pid, fun handle_subs_msg/2),
  {noreply, NewState};
%% @private
%% @doc Handling all other cast messages
handle_cast(_Request, State = #eps_sub_subscriber_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages

%% @private
%% @doc
%%  Handle initial timeout message.
%%    --> Start the connection.
%%    --> Subscribe to the topic configured.
%%    --> Start a new timer for performing aggregate function.
%%    --> Start waiting for any new message.
%% @end
handle_info(initial_sub_delay, State = #eps_sub_subscriber_state{}) ->
  %% Create the connection.
  Conn = stomp:connect(State#eps_sub_subscriber_state.hostname,
    State#eps_sub_subscriber_state.port, "", "", [{"client-id", State#eps_sub_subscriber_state.sub_name}]),
  %% Subscribe to the topic configured.
  stomp:subscribe(State#eps_sub_subscriber_state.topic_name,
    Conn, [{"activemq.subscriptionName", "SampleSubscription"}]),
  % Start timer to calculate perform the aggregate function.
  TimerRef = case timer:send_after(State#eps_sub_subscriber_state.sleep_interval,
    State#eps_sub_subscriber_state.pid, subscribe_aggr_timeout) of
               {ok, TRef} -> TRef;
               {error, _} -> ''
             end,
  % Wait for any new message.
  subs_for_inc_msgs(Conn, State#eps_sub_subscriber_state.pid, fun handle_subs_msg/2),
  {noreply, State#eps_sub_subscriber_state{conn = Conn, timer_ref = TimerRef}};
%% @private
%% @doc
%%  Handle timeout message to perform aggregate function.
%% @end
handle_info(subscribe_aggr_timeout, State = #eps_sub_subscriber_state{}) ->
  NewState = handle_messages_and_sleep(State),
  {noreply, NewState};
%% @private
%% @doc Handling other non call/cast messages
handle_info(_Info, State = #eps_sub_subscriber_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
terminate(_Reason, State = #eps_sub_subscriber_state{}) ->
  %% Cancel the timer reference.
  timer:cancel(State#eps_sub_subscriber_state.timer_ref),
  %% Disconnect the STOMP Connection.
  stomp:disconnect(State#eps_sub_subscriber_state.conn),
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #eps_sub_subscriber_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
%%--------------------------------------------------------------------
%% @doc
%%  Retrieve the messages, and pass it to the subscriber process.
%% @end
%%--------------------------------------------------------------------
subs_for_inc_msgs(Conn, Pid, HandlerFn) ->
  Messages = stomp:get_messages(Conn),
  HandlerFn(Pid, Messages).

%% @private
%%--------------------------------------------------------------------
%% @doc
%%  Handler Function for incoming message.
%% @end
%%--------------------------------------------------------------------
handle_subs_msg(Pid, Messages) ->
  gen_server:cast(Pid, {inc_subs_msg, Messages}).

%% @private
%%--------------------------------------------------------------------
%% @doc
%%  Cancel the existing timer Reference,
%%  Apply the aggregator function on the stored list of messages (integers).
%%  Recalculate new sleep time and start a new timer with the calculated value.
%% @end
%%--------------------------------------------------------------------
handle_messages_and_sleep(State) ->
  Bef = trunc(epsUtils:get_timestamp() / 1000),
  % Cancels the timer reference.
  timer:cancel(State#eps_sub_subscriber_state.timer_ref),
  % Apply aggregator function as configured.
  Agg = apply(epsUtils, State#eps_sub_subscriber_state.aggr_fn, [State#eps_sub_subscriber_state.msg_list]),
  ?LOG_DEBUG("Incoming messages for ~s over the subscription time: ~p ",
    [State#eps_sub_subscriber_state.sub_name, State#eps_sub_subscriber_state.msg_list]),
  ?LOG_DEBUG("Output from ~s Aggregated Function(~p) is: ~p", [State#eps_sub_subscriber_state.sub_name,
    State#eps_sub_subscriber_state.aggr_fn, Agg]),
  TimeInMS = trunc(epsUtils:get_timestamp() / 1000) - Bef,
  % Recalculate the new sleep time based on the time spend in aggregator function.
  NewSleepTime = trunc(State#eps_sub_subscriber_state.sleep_interval - TimeInMS),
  % Start new timer
  TimerRef = case timer:send_after(NewSleepTime,
    State#eps_sub_subscriber_state.pid, subscribe_aggr_timeout) of
               {ok, TRef} -> TRef;
               {error, _} -> ''
             end,
  LastMsgList = State#eps_sub_subscriber_state.msg_list,
  State#eps_sub_subscriber_state{timer_ref = TimerRef, msg_list = [], agg_msg_list = LastMsgList, aggr_result = Agg}.

%% @private
%%--------------------------------------------------------------------
%% @doc
%%  Handle incoming messages. Here we just store to list.
%%  Store only messages with TYPE "MESSAGE"
%% @end
%%--------------------------------------------------------------------
handle_inc_messages([], State) ->
  State;
handle_inc_messages([[{type, "MESSAGE"}, {headers, _Headers}, {body, MessageBody}]|T], State) ->
  NewMsgList = [list_to_integer(MessageBody)] ++ State#eps_sub_subscriber_state.msg_list,
  handle_inc_messages(T, State#eps_sub_subscriber_state{msg_list = NewMsgList});
handle_inc_messages([[{type, _}, _, _]|T], State) ->
  handle_inc_messages(T, State).