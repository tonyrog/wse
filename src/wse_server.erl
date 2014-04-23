%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Web socket server (RFC 6455)
%%% @end
%%% Created :  9 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(wse_server).
-compile(export_all).

-define(WS_UUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
-define(WS_OP_TEXT,   1).
-define(WS_OP_BINARY, 2).

-record(event,
	{
	  iref,      %% global integer reference
	  from,      %% [owner local reference | event owner pid]
	  how=once,  %% once | all | none
	  data       %% local data for events
	}).

-record(ws_header,
	{
	  host,         %% 'Host'
	  upgrade,      %% 'Upgrade'
	  connection,   %% 'Connection'
	  key,          %% "Sec-WebSocket-Key"
	  protocol,     %% "Sec-WebSocket-Protocol"
	  origin,       %% 
	  version,      %% "Sec-WebSocket-Version"
	  hs = []
	}).

-record(s,
	{
	  iref = 1,
	  proto,       %% from handshake
	  type,        %% .. text right now
	  fs   = [],  %% fragments
	  wait = []   %% #event
	}).


-define(log(F,W,As),
	io:format("~s:~w: " ++ (W)++" "++(F)++"\n", [?MODULE, ?LINE | (As)])).	
-define(debug(F,As), ?log(F,"debug",As)).
%% -define(debug(F,A), ok).
-define(warn(F,As),  ?log(F,"warn", As)).
-define(error(F,As), ?log(F,"error", As)).

%% start()
%%  This should be in another module for clarity
%%  but is included here to make the example self-contained

start() ->
    start(1234).

start(Port) ->
    spawn(fun() -> init(Port) end).

init(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, 
				  [{packet,http},{reuseaddr,true},
				   {mode, binary}, {active, once}]),
    listen_loop(Listen).

listen_loop(Listen) ->
    Parent = self(),
    {Pid,Mon} = spawn_monitor(fun() -> accept(Parent, Listen) end),
    receive
	{Pid,ok} ->
	    erlang:demonitor(Mon, [flush]),
	    ?MODULE:listen_loop(Listen);
	{'DOWN',Mon,process,Pid,Reason} ->
	    io:format("process crashed: ~p\n", [Reason]),
	    listen_loop(Listen)
    end.
    
accept(Parent, Listen) ->
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    io:format("Connected to ~p\n", [inet:peername(Socket)]),
	    Parent ! {self(), ok},
	    ?MODULE:ws_handshake(Socket);
	Error ->
	    Parent ! {self(), Error}
    end.
%%
%% Simple BERT
%%
bert_encode(Term) ->
    term_to_binary(Term).

bert_decode(Bin) ->
    binary_to_term(Bin).

%%
%% Stupid WebSocket (where is the length mode?)
%% The spec says that you can send length indicator 
%% 0x80+L1,0x80+L2,..Ln where Li is 7 bit data from length
%% and the last length byte has high bit clear
%%
ws_encode(Term) ->
%%    base64:encode(bert_encode(Term)).
    bert_encode(Term).

ws_decode(Data) ->
%%    bert_decode(base64:decode(Data)).
    bert_decode(Data).

ws_handshake(Socket) ->
    receive
	{http, Socket, _Req={http_request,'GET',Uri,_Version}} ->
	    ?debug("got ws request ~p", [_Req]),
	    ws_handshake(Socket, Uri);
	{http, _Socket, Req={http_request, _, _, _}} ->
	    ?warn("reject ws request ~p", [Req]),
	    %% send error reply!
	    ws_error({error, bad_request});
	Any ->
	    ?warn("reject ws data ~p", [Any]),
	    ws_error({error, no_data})
    end.

ws_handshake(Socket, _Uri) ->
    inet:setopts(Socket, [{active, once}]),
    case ws_recv_headers(Socket, #ws_header{}, 1000) of
	Err ={error,_} ->
	    ws_error(Err);
	F when is_list(F#ws_header.key) ->
	    ?debug("got request data: uri=~p, header=~p", [_Uri, F]),
	    %% fixme: check base64! (just crash now)
	    %% ?debug("Random = ~w", [base64:decode(F#ws_header.key)]),
	    Accept1 = [F#ws_header.key, ?WS_UUID],
	    Accept2 = crypto:hash(sha, Accept1),
	    Accept  = base64:encode(Accept2),
	    WsAccept = ["Sec-Websocket-Accept:",Accept,"\r\n"],
	    %% ?debug("Accept = ~w", [Accept]),
	    WsProto = if is_list(F#ws_header.protocol) ->
			      ["Sec-Websocket-Protocol:",
			       hd(string:tokens(F#ws_header.protocol, ",")),
			       "\n\n"];
			 true -> []
		      end,
	    Handshake =
		[
		 "HTTP/1.1 101 Switching Protocols\r\n",
		 "Upgrade: websocket\r\n",
		 "Connection: Upgrade\r\n",
		 WsAccept,
		 WsProto,
		 "\r\n"],
	    gen_tcp:send(Socket, Handshake),
	    ?debug("ws_server: sent: ~p", [Handshake]),
	    inet:setopts(Socket, [{packet, 0},{active,once}]),
	    ws_loop(<<>>, Socket, #s {proto=WsProto, type=?WS_OP_BINARY});
	true ->
	    ws_error({error, missing_key})
    end.

ws_error(Error) ->
    ?error("~w", [Error]),
    Error.

ws_recv_headers(S, F, Timeout) ->
    receive
	{http, S, http_eoh} ->
	    F;
	{http, S, {http_header, _, K, _, V}} ->
	    inet:setopts(S, [{active, once}]),
	    case K of
		'Host' ->
		    ws_recv_headers(S, F#ws_header { host = V}, Timeout);
		'Upgrade' ->
		    ws_recv_headers(S, F#ws_header { upgrade = V}, Timeout);
		'Connection' ->
		    ws_recv_headers(S, F#ws_header { connection = V}, Timeout);
		"Sec-Websocket-Key" ->
		    ws_recv_headers(S, F#ws_header { key = V}, Timeout);
		"Sec-Websocket-Protocol" ->
		    ws_recv_headers(S, F#ws_header { protocol = V}, Timeout);
		"Sec-Websocket-Version" ->
		    ws_recv_headers(S, F#ws_header { version = V}, Timeout);
		_ ->
		    F1 = F#ws_header { hs = [{K,V}|F#ws_header.hs]},
		    ws_recv_headers(S, F1, Timeout)
	    end
    after Timeout ->
	    {error, timeout}
    end.

%%
%% Reply on event
%% if reply returns true then the event should stay
%% otherwise the event should be deleted
%%
reply(E, Reply) ->
    if E#event.how == none ->
	    false;
       true ->
	    [Ref|Pid] = E#event.from,
	    Pid ! {reply,Ref,Reply},
	    E#event.how == all
    end.


next_ref(Ref) ->
    Ref1 = (Ref+1) band 16#ffffffff,
    if Ref1 == 0 ->
	    1;
       true ->
	    Ref1
    end.

ws_loop(Buf, Socket, S) ->
    receive
	%% WebSocket stuff
	{tcp, Socket, Data} ->
	    ?debug("tcp ~w: ~p", [Socket, Data]),
	    ws_data(Buf, Data, Socket, S);

	{tcp_closed, Socket} ->
	    ?debug("tcp_closed ~w", [Socket]),
	    %% reply to all remaining callers
	    lists:foreach(fun(E) -> reply(E, {error,closed}) end, S#s.wait),
	    ok;
	
	Message ->
	    ?debug("handle_local: ~p", [Message]),
	    case handle_local(Message, Socket, S) of
		{noreply,S1} ->
		    ws_loop(Buf, Socket, S1);
		{stop,normal} ->
		    ok;
		{stop,Reason} ->
		    exit(Reason)
	    end
    end.

ws_data(Buf, Data, Socket, S) ->
    case <<Buf/binary, Data/binary>> of
	%% masked data
	<<Fin:1,_Rsv:3,Op:4,1:1,126:7,L:16,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	    ?debug("unmask fragment: mask=~p, frag=~p", [M, Frag]),
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket, Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,1:1,127:7,L:64,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	     ?debug("unmask fragment: mask=~p, frag=~p", [M, Frag]),
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket,Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,1:1,L:7,M:4/binary,Frag:L/binary,Buf1/binary>> ->
	    ?debug("unmask fragment: mask=~p, frag=~p", [M, Frag]),
	    Frag1 = ws_mask(M, Frag),
	    S1 = ws_fragment(Socket,Fin, Op, Frag1, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	%% non masked data
	<<Fin:1,_Rsv:3,Op:4,0:1,126:7,L:16,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,0:1,127:7,L:64,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	<<Fin:1,_Rsv:3,Op:4,0:1,L:7,Frag:L/binary,Buf1/binary>> ->
	    S1 = ws_fragment(Socket,Fin, Op, Frag, S),
	    ws_data(Buf1, <<>>, Socket, S1);
	Buf1 -> %% handle to large messages and mal formed
	    inet:setopts(Socket, [{active, once}]),
	    ws_loop(Buf1, Socket, S)
    end.

ws_mask(<<M0,M1,M2,M3>>, Frag) ->
    ws_mask(Frag,M0,M1,M2,M3).
    
ws_mask(<<X0,X1,X2,X3,Xs/binary>>, M0, M1, M2, M3) ->
    <<(X0 bxor M0), (X1 bxor M1), (X2 bxor M2), (X3 bxor M3),
      (ws_mask(Xs, M0, M1, M2, M3))/binary>>;
ws_mask(<<X0,X1,X2>>, M0, M1, M2, _M3) ->
    <<(X0 bxor M0), (X1 bxor M1), (X2 bxor M2)>>;
ws_mask(<<X0,X1>>, M0, M1, _M2, _M3) ->
    <<(X0 bxor M0), (X1 bxor M1)>>;
ws_mask(<<X0>>, M0, _M1, _M2, _M3) ->
    <<(X0 bxor M0)>>;
ws_mask(<<>>, _M0, _M1, _M2, _M3) ->
    <<>>.

ws_fragment(Socket,1, _Op, Frag, S) ->
    Payload = iolist_to_binary(lists:reverse([Frag|S#s.fs])),
    ?debug("op=~w, unmasked payload = ~p", [ws_opcode(_Op),Payload]),
    Message = ws_decode(Payload),
    ?debug("handle_remote: ~p", [Message]),
    handle_remote(Message, Socket, S#s { fs=[] });
ws_fragment(_Socket, 0, _Op, Frag, S) ->
    ?debug("collect fragment: Op=~w, Frag=~p", [_Op,Frag]),
    S#s { fs = [Frag|S#s.fs ]}.


ws_opcode(0) -> continuation;
ws_opcode(?WS_OP_TEXT) -> text;
ws_opcode(?WS_OP_BINARY) -> binary;
ws_opcode(8) -> close;
ws_opcode(9) -> ping;
ws_opcode(10) -> pong;
ws_opcode(Op) -> Op.

ws_make_server_frame(Payload0,Type) ->
    Fin = 1,
    ws_make_frame(Fin,Type,<<>>, Payload0).

ws_make_client_frame(Payload0,Type) ->
    Fin = 1,
    M = crypto:rand_bytes(4),
    Payload = ws_mask(M, Payload0),
    ws_make_frame(Fin,Type,M,Payload).


ws_make_frame(Fin, Op, Mask, Data) ->
    L = byte_size(Data),
    M = if Mask =:= <<>> -> 0; true -> 1 end,
    ?debug("payload size = ~w, mask=~w\n", [L,M]),
    if L < 126 ->
	    <<Fin:1,0:3,Op:4,M:1,L:7,Mask/binary,Data/binary>>;
       L < 65536 ->
	    <<Fin:1,0:3,Op:4,M:1,126:7,L:16,Mask/binary,Data/binary>>;
       true ->
	    <<Fin:1,0:3,Op:4,M:1,127:7,L:64,Mask/binary,Data/binary>>
    end.

    
handle_local({rsync,From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({rsync,IRef,Request}),
    gen_tcp:send(Socket, ws_make_server_frame(Bin,S0#s.type)),
    Event = #event{iref=IRef,from=From},
    Wait1 = [Event|S0#s.wait],
    {noreply,S0#s { iref=next_ref(IRef), wait=Wait1 }};
handle_local({nsync,From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({nsync,IRef,Request}),
    gen_tcp:send(Socket,  ws_make_server_frame(Bin,S0#s.type)),
    Event = #event{iref=IRef,from=From,how=none},
    Wait1 = [Event|S0#s.wait],
    {noreply,S0#s { iref=next_ref(IRef), wait=Wait1 }};
handle_local({async,_From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({async,IRef,Request}),
    gen_tcp:send(Socket,  ws_make_server_frame(Bin,S0#s.type)),
    {noreply,S0#s { iref=next_ref(IRef) }};
handle_local({close,From,Reason},Socket,S0) ->
    reply(#event { from=From} , ok),
    lists:foreach(fun(E) -> reply(E, {error, closed}) end, S0#s.wait),
    gen_tcp:close(Socket),
    {stop,Reason};
handle_local({create_event,From,How,Data},_Socket,S0) ->
    IRef = S0#s.iref,
    Event = #event { iref=IRef, from=From, how=How, data=Data},
    Wait1 = [Event|S0#s.wait],    
    reply(Event, {ok, IRef}),
    {noreply,S0#s { iref=next_ref(IRef), wait=Wait1 }};
handle_local(Other,_Socket,S0) ->
    io:format("handle_local: got ~p~n",[Other]),
    {noreply,S0}.

%%
%% Handle remote operations and replies
%%
handle_remote({reply,IRef,Reply}, _Socket, S0) ->
    case lists:keytake(IRef, #event.iref, S0#s.wait) of
	false ->
	    S0;
	{value,Event,Wait1} ->
	    reply(Event, Reply),
	    S0#s { wait=Wait1}
    end;
handle_remote({noreply,IRef},_Socket,S0) ->
    case lists:keytake(IRef, #event.iref, S0#s.wait) of
	false ->
	    S0;
	{value,_Event,Wait1} ->
	    S0#s { wait=Wait1}
    end;
handle_remote({notify,IRef,RemoteData},_Socket,S0) ->
    io:format("NOTIFY: ~w ~p\n", [IRef,RemoteData]),
    case lists:keytake(IRef,#event.iref, S0#s.wait) of
	false ->
	    S0;
	{value,E,Wait1} ->
	    [_Ref|Pid] = E#event.from,
	    Pid ! {notify,IRef,E#event.data,RemoteData},
	    if E#event.how == all ->
		    S0;
	       true ->
		    S0#s { wait=Wait1}
	    end
    end;
handle_remote({info,Data},_Socket,S0) ->
    io:format("INFO: ~p\n", [Data]),
    S0;
handle_remote({start,M,F,As},_Socket,S0) ->
    spawn(M,F,[self()|As]),
    S0;
handle_remote({call,IRef,M,F,As},Socket,S0) ->
    try apply(M,F,As) of
	Value ->
	    Bin = ws_encode({reply,IRef,{ok,Value}}),
	    gen_tcp:send(Socket,  ws_make_server_frame(Bin,S0#s.type)),
	    S0
    catch
	error:Reason ->
	    Bin = ws_encode({reply,IRef,{error,Reason}}),
	    gen_tcp:send(Socket,  ws_make_server_frame(Bin,S0#s.type)),
	    S0
    end;
handle_remote({cast,_IRef,M,F,As},_Socket,S0) ->
    catch (apply(M,F,As)),
    S0;
handle_remote(Other, _Socket, S0) ->
    io:format("handle_remote: got ~p\n", [Other]),
    S0.
