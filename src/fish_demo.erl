%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%      Show how to do remote animation with png
%%%      Fishes from: http://www.squaregoldfish.co.uk
%%% @end
%%% Created :  24 Apr 2014 by Tony Rogvall <tony@rogvall.se>

-module(fish_demo).
-export([run/2]).

-compile(export_all).

run(Ws, Where) ->
    io:format("fish_demo: called\n"),
    %% "preload" fish images
    Fishes =
	[ begin
	      FNo = tl(integer_to_list(10000+I)),
	      File = "fish/fish"++FNo++".png",
	      {ok,Image} = wse:load_image(Ws, File),
	      %% io:format("loaded: ~s\n", [File]),
	      Image
	  end ||  I <- lists:seq(1,23)],
    Image = wse:createElement(Ws, "img"),
    Parent = wse:id(Where),
    wse:appendChild(Ws, Parent, Image),
    update_loop(Ws, Image, Fishes, Fishes).

update_loop(Ws, Image, [F|Fs],  Fishes) ->
    {ok,Src} = wse:get(Ws, F, "src"),
    %% io:format("Src=~p\n", [Src]),
    wse:set(Ws, Image, "src", Src),
    timer:sleep(100),
    update_loop(Ws, Image, Fs, Fishes);
update_loop(Ws, Image, [],  Fishes) ->
    update_loop(Ws, Image, Fishes, Fishes).

