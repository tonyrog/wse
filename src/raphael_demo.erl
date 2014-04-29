%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%      This demo is called directly from web page
%%% @end
%%% Created :  9 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(raphael_demo).

-export([run/2]).

%%
%% Small demo
%%
run(Ws, Where) ->
    io:format("raphael_demo: called\n"),
    ok = wse:load(Ws, "raphael-min.js"),
    {ok,Paper} = raphael:new(Ws, Where, 640, 480),

    %% must create the image element here!
%%    {ok,_Image1} = wse:load_image(Ws, "bd.jpg"),
%%    {ok,IMG1} = raphael:image(Ws, Paper, "bd.jpg", 0, 50, 320, 240),
%%    {ok,IMG2} = raphael:image(Ws, Paper, "bd.jpg", 0, 100, 320, 240),
%%    raphael:attr(Ws, IMG2, "transform", "s1-1"),
%%    raphael:attr(Ws, IMG2, "opacity", ".5"),
%%    {ok,R5} = raphael:rect(Ws, Paper, 10, 50, 120, 100),
%%    raphael:attr(Ws, R5, "fill", "90-#fff-#000"),
%%    raphael:attr(Ws, R5, "fill", "#FF0000"),    
%%    raphael:attr(Ws, R5, "stroke", "none"),    
%%    raphael:attr(Ws, R5, "opacity", "none"),

    {ok,C1} = raphael:circle(Ws, Paper, 10, 10, 10),
    raphael:attr(Ws, C1, "fill", "#FF0000"),
    {ok,C2} = raphael:circle(Ws, Paper, 40, 10, 10),
    raphael:attr(Ws, C2, "fill", "#00FF00"),
    {ok,C3} = raphael:circle(Ws, Paper, 70, 10, 10),
    raphael:attr(Ws, C3, "fill", "#0000FF"),
    {ok,C4} = raphael:circle(Ws, Paper, 100, 10, 10),
    raphael:attr(Ws, C4, "fill", "90-#fff-#000"),
    
    {ok,R1} = raphael:rect(Ws, Paper, 10, 30, 10, 10),
    raphael:attr(Ws, R1, "fill", "#FF0000"),    
    {ok,R2} = raphael:rect(Ws, Paper, 40, 30, 10, 10),
    raphael:attr(Ws, R2, "fill", "#00FF00"),
    
    {ok,R3} = raphael:rect(Ws, Paper, 70, 30, 10, 10),
    raphael:attr(Ws, R3, "fill", "#0000FF"),
    {ok,R4} = raphael:rect(Ws, Paper, 100, 30, 10, 10),
    raphael:attr(Ws, R4, "fill", "90-#fff-#000"),


    ok.

    
    
    
