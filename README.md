wse - Websockets for Erlang
===========================

WSE is a simple, standalone, websocket server which execute
request in the browser instead of in the web server. There is a bit
of boot strapping that may need help from a web server. But a simple
web page and a browser is all that it takes to get running.

To get control of part of a web page (or all of it) use the following 
HTML snippet. Also make sure you have ej.js and wse.js in the same directory.

    <html><head>
    <title>wse demo page</title>
    <script src='ej.js'></script>
    <script src='wse.js'></script>
    <script>
        window.onload = function() {
          if (Wse.open("ws://localhost:1234/websession"))
             Wse.start('wse_demo', 'run', ["myid"]);
        };
    </script></head>
    <body>
      <div id="myid"></div>
    </body></html>

Next thing is to start erlang wse server (default to port 1234):

    $ erl
    > wse_server:start().

Then have the erlang module wse_demo in the path somewhere:

    -module(wse_demo).
    -export([run/2]).

    run(Ws, Where) ->
        ElemNode = wse:createElement(Ws, "p"),
        TextNode = wse:createTextNode(Ws, "Hello world"),
        wse:appendChild(Ws, ElemNode, TextNode),
        wse:appendChild(Ws, wse:id(Where), ElemNode).

The browser will call wse_demo:run (via the websocket) with the web socket proxy process as a the first argument and the "myid" as the second argument. From thereon the web page can be manipulated at will.

