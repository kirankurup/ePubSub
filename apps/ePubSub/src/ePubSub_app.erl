%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%  ePubSub public API.
%%% @end
%%% Created : 23. Jan 2021 3:50 PM
%%%-------------------------------------------------------------------
-module(ePubSub_app).
-author("kirankurup@gmail.com").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ePubSub_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
