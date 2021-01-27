%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   Utility functions
%%% @end
%%% Created : 23. Jan 2021 10:11 PM
%%%-------------------------------------------------------------------
-module(epsUtils).
-author("kirankurup@gmail.com").

%% API
-export([get_timestamp/0, calculate_sum/1,
  calculate_median/1]).

%%--------------------------------------------------------------------
%% @doc
%%  Retrieve the current timestamp in micro seconds.
%% @end
%%--------------------------------------------------------------------
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.

%%--------------------------------------------------------------------
%% @doc
%%  Calculate sum of all the entries in list.
%% @end
%%--------------------------------------------------------------------
calculate_sum(List) ->
  calculate_sum(List, 0).

calculate_sum([], Sum) -> Sum;
calculate_sum([H|T], Sum) ->
  calculate_sum(T, Sum + H).

%%--------------------------------------------------------------------
%% @doc
%%  Calculate the median of the entries in the list.
%% @end
%%--------------------------------------------------------------------
calculate_median([]) ->
  0;
calculate_median(Unsorted) ->
  Sorted = lists:sort(Unsorted),
  Length = length(Sorted),
  Mid = Length div 2,
  Rem = Length rem 2,
  (lists:nth(Mid+Rem, Sorted) + lists:nth(Mid+1, Sorted)) / 2.