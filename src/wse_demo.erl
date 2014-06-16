-module(wse_demo).
-export([run/2]).

run(Ws, Where) ->
    ElemNode = wse:createElement(Ws, "p"),
    TextNode = wse:createTextNode(Ws, "Hello world"),
    wse:appendChild(Ws, ElemNode, TextNode),
    wse:appendChild(Ws, wse:id(Where), ElemNode).
