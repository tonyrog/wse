%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Handle data from input fields
%%% @end
%%% Created : 26 Apr 2024 by Tony Rogvall <tony@rogvall.se>

-module(wse_input_demo).

-export([run/2]).

%% Button demo
run(Ws, Where) ->
    io:format("wse_input_demo: called\n"),
    run_loop(Ws, Where).

run_loop(Ws, Where) ->
    receive
	Msg = {notify,_ID,_Local,_Data} ->
	    io:format("wse_input_demo: ~p\n", [Msg]),
	    run_loop(Ws, Where)
    end.
