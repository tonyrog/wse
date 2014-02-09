%%% File    : raphael.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : Raphael interface towards browser
%%% Created : 21 Dec 2009 by Tony Rogvall <tony@rogvall.se>

-module(raphael).

-compile(export_all).

%% New returns a reference to the canvase object
new(Ws, Array) ->
    wse:call(Ws, wse:window(), "Raphael", [Array]).

new(Ws, Object, W, H) ->
    wse:call(Ws, wse:window(), "Raphael", [Object,W,H]).

new(Ws, X, Y, W, H) ->
    wse:call(Ws, wse:window(), "Raphael", [X,Y,W,H]).

setSize(Ws,Paper,W,H) ->
    wse:call(Ws, Paper, setSize, [W, H]).

close(Ws,Paper) ->
    wse:call(Ws, Paper, clone, []).

%% get attribute value
attr(Ws,Object,Name) ->
    wse:call(Ws,Object,attr,[Name]).

%% set attribute value
attr(Ws,Object,Name,Value) ->
    wse:call(Ws,Object,attr,[Name,Value]).


circle(Ws,Paper, X, Y, R) ->
    wse:call(Ws, Paper, circle, [X, Y, R]).

rect(Ws,Paper, X, Y, W, H) ->
    wse:call(Ws, Paper, rect, [X, Y, W, H]).

rect(Ws,Paper, X, Y, W, H, R) ->
    wse:call(Ws, Paper, rect, [X, Y, W, H, R]).

ellipse(Ws,Paper,X,Y,Rx,Ry) ->
    wse:call(Ws,Paper,ellipse,[X,Y,Rx,Ry]).

image(Ws,Paper,Src,X,Y,Width,Height) ->
    wse:call(Ws,Paper,image,[Src,X,Y,Width,Height]).

set(Ws,Paper) ->
    wse:call(Ws,Paper,set,[]).

push(Ws,Set,Objects) when is_list(Objects) ->
    wse:call(Ws,Set,push,Objects);
push(Ws, Set, Object) ->
    wse:call(Ws,Set,push,[Object]).

%% FIXME: utf8!
text(Ws,Paper,X,Y,Text) ->
    wse:call(Ws,Paper,text,[X,Y,Text]).

%% return a font object
getFont(Ws,Paper,Family) ->
    wse:call(Ws,Paper,getFont,[Family]).

print(Ws,Paper,X,Y,Text,Font,FontSize) ->
    wse:call(Ws,Paper,print,[X,Y,Text,Font,FontSize]).

path(Ws,Paper,SVG) ->
    wse:call(Ws,Paper,path,[SVG]).

path(Ws,Paper) ->
    wse:call(Ws,Paper,path,[]).

%%
%% Small demo
%%
demo(Ws, Where) ->
    %% FIXME: only load once per document!
    ok = wse:load(Ws, "raphael-min.js"),
    {ok,Paper} = new(Ws, Where, 120, 50),
    {ok,C1} = circle(Ws, Paper, 10, 10, 10),
    attr(Ws, C1, "fill", "#FF0000"),
    {ok,C2} = circle(Ws, Paper, 40, 10, 10),
    attr(Ws, C2, "fill", "#00FF00"),
    {ok,C3} = circle(Ws, Paper, 70, 10, 10),
    attr(Ws, C3, "fill", "#0000FF"),
    {ok,C4} = circle(Ws, Paper, 100, 10, 10),
    attr(Ws, C4, "fill", "90-#fff-#000"),    
    
    {ok,R1} = rect(Ws, Paper, 10, 30, 10, 10),
    attr(Ws, R1, "fill", "#FF0000"),    
    {ok,R2} = rect(Ws, Paper, 40, 30, 10, 10),
    attr(Ws, R2, "fill", "#00FF00"),
    
    {ok,R3} = rect(Ws, Paper, 70, 30, 10, 10),
    attr(Ws, R3, "fill", "#0000FF"),
    {ok,R4} = rect(Ws, Paper, 100, 30, 10, 10),
    attr(Ws, R4, "fill", "90-#fff-#000").




    
    


    





	    
    


