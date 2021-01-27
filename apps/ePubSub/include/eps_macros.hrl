%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%   MACRO definitions used across the modules.
%%% @end
%%% Created : 24. Jan 2021 9:27 PM
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-ifndef (_EPS_MACROS_HRL).
-define (_EPS_MACROS_HRL, true).

-define (GET_ENV(K),            application:get_env(K)).
-define (GET_ENV(A, K),         application:get_env(A, K)).
-define (GET_ENV(A, K, D),      application:get_env(A, K, D)).

-endif.