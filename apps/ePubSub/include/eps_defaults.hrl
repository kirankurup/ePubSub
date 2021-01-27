%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%  Default values mentioned here.
%%% @end
%%% Created : 23. Jan 2021 3:56 PM
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-define(APPLICATION,                        ePubSub).

%% Supervisor Defaults
-define(SUPERVISOR_MAX_RESTART,             5).
-define(SUPERVISOR_MAX_TIME,                60).

%% Initial Delay in milli seconds before the publisher starts sending messages.
-define(INITIAL_DELAY,                      5000).
%% Initial Delay in milli seconds before the subscriber starts accepting messages.
-define(INITIAL_SUB_DELAY,                  1000).

%% Configuration related.
-define(APPL_RUN_AS,                        run_as).                % Run application as publisher / subscriber / both.
-define(DEFAULT_APPL_RUN_AS,                "both").                % Can be "publisher" / "subscriber" / "both".

-define(AMQSERVER,                          amq_server).            % ActiveMQ Server Key
-define(DEFAULT_AMQSERVER,                  "localhost").           % ActiveMQ Server Default value
-define(AMQSERVER_PORT,                     amq_server_port).       % ActiveMQ Server Connection Port Key
-define(DEFAULT_AMQSERVER_PORT,             61613).                 % ActiveMQ Server Connection Port Default Value.
-define(PUBLISHER_RATE,                     publish_rate).          % Publisher rate Key.
-define(DEFAULT_PUBLISHER_RATE,             20).                    % Publisher rate per second.
-define(PUBLISHER_RANGE,                    publish_range).         % Publisher range Key.
-define(DEFAULT_PUBLISHER_RANGE,            100).                   % Publisher can send integers up to this level.
-define(EPS_TOPIC,                          eps_topic).             % EPS Topic Key.
-define(DEFAULT_EPS_TOPIC,                  "/topic/SampleTopic").  % Default EPS Topic.
-define(SUBSCRIBER_SLEEP_INTVL,             subs_sleep_intvl).      % Subscriber sleep interval Key.
-define(DEFAULT_SUBSCRIBER_SLEEP_INTVL,     5000).                  % Default Subscriber sleep interval in millisec.