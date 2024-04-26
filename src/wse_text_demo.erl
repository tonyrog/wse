-module(wse_text_demo).
-export([run/2]).

run(Ws, [A,B]) ->
    {ok,Ap} = wse:getElementById(Ws, A),
    {ok,Bp} = wse:getElementById(Ws, B),
    loop(Ws, Ap, Bp).

loop(Ws, Ap, Bp) ->
    timer:sleep(3000),
    set_p_text(Ws, Ap, "en", "Good bye!"),
    set_p_text(Ws, Bp, "en", "Sigur Rós is playing"),
    timer:sleep(3000),
    set_p_text(Ws, Ap, "en", "Film time!"),
    set_p_text(Ws, Bp, "jap", "宮崎駿 is playing"),
    timer:sleep(3000),
    set_p_text(Ws, Ap, "en", "Film time2!"),
    set_p_text(Ws, Bp, "ja", [229,174,174,229,180,142,233,167,191]++" is playing"),
    timer:sleep(3000),
    set_p_text(Ws, Ap, "en", "Hello, world!"),
    set_p_text(Ws, Bp, "en", "Röyksopp is playing"),
    timer:sleep(3000),
    set_p_text(Ws, Ap, "en", "Film time3"),
    set_p_text(Ws, Bp, "jap", <<"宮崎駿 is playing"/utf8>>),
    loop(Ws, Ap, Bp).

set_p_text(Ws, P, Lang, Text) ->
    io:format("set_text: ~p\n", [Text]),
    io:format("        : ~w\n", [Text]),
    wse:set(Ws, P, "lang", Lang),
    {ok, TextNode} = wse:firstChild(Ws, P),  %% Text node!?
    wse:set(Ws, TextNode, "nodeValue", Text).

     
