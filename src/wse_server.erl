-module(wse_server).
-compile(export_all).

-record(event,
	{
	  iref,      %% global integer reference
	  from,      %% [owner local reference | event owner pid]
	  how=once,  %% once | all | none
	  data       %% local data for events
	}).

-record(s,
	{
	  iref = 1,
	  wait = []   %% #event
	}).

%% start()
%%  This should be in another module for clarity
%%  but is included here to make the example self-contained

start() ->
    start(1234).

start(Port) ->
    spawn(fun() -> init(Port) end).

init(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, 
				  [{packet,0},{reuseaddr,true},
				   {active, true}]),
    listen_loop(Listen).

listen_loop(Listen) ->
    Parent = self(),
    Pid = spawn(fun() -> accept(Parent, Listen) end),
    receive
	{Pid,ok} ->
	    ?MODULE:listen_loop(Listen);
	{Pid,Error} ->
	    Error
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
    base64:encode(bert_encode(Term)).

ws_decode(Data) ->
    bert_decode(base64:decode(Data)).

ws_handshake(Socket) ->
    receive
	{tcp, Socket, _Data} ->
	    Handshake =
		[
		 "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
		 "Upgrade: WebSocket\r\n",
		 "Connection: Upgrade\r\n",
		 "WebSocket-Origin: http://localhost\r\n",
		 "WebSocket-Location: ",
		 "  ws://localhost:1234/websession\r\n\r\n"
		],
	    gen_tcp:send(Socket, Handshake),
	    io:format("ws_server: sent: ~p\n", [Handshake]),
	    ws_loop(zero, Socket, #s {});
	Any ->
	    io:format("Received:~p~n",[Any]),
	    ?MODULE:ws_handshake(Socket)
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

ws_loop(Buf, Socket, S0) ->
    receive
	%% WebSocket stuff
	{tcp, Socket, Data} ->
	    ws_data(Buf, Data, Socket, S0);

	{tcp_closed, Socket} ->
	    %% reply to all remaining callers
	    lists:foreach(fun(E) -> reply(E, {error,closed}) end, S0#s.wait),
	    ok;
	
	Message ->
	    io:format("handle_local: ~p\n", [Message]),
	    case handle_local(Message, Socket, S0) of
		{noreply,S1} ->
		    ws_loop(Buf, Socket, S1);
		{stop,normal} ->
		    ok;
		{stop,Reason} ->
		    exit(Reason)
	    end
    end.


ws_data(zero, [0|T], Socket, S0) -> 
    ws_data([], T, Socket, S0);
ws_data(zero, [], Socket, S0) ->
    ws_loop(zero, Socket, S0);
ws_data(L, [255|T], Socket, S0) ->
    Message = ws_decode(lists:reverse(L)),
    io:format("handle_remote: ~p\n", [Message]),
    S1 = handle_remote(Message, Socket, S0),
    ws_data(zero,T, Socket, S1);
ws_data(L, [H|T], Socket, S0) ->
    ws_data([H|L], T, Socket, S0);
ws_data([], L, Socket, S0) ->
    ws_loop(L, Socket, S0).


handle_local({rsync,From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({rsync,IRef,Request}),
    gen_tcp:send(Socket, [0,Bin,255]),
    Event = #event{iref=IRef,from=From},
    Wait1 = [Event|S0#s.wait],
    {noreply,S0#s { iref=next_ref(IRef), wait=Wait1 }};
handle_local({nsync,From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({nsync,IRef,Request}),
    gen_tcp:send(Socket, [0,Bin,255]),
    Event = #event{iref=IRef,from=From,how=none},
    Wait1 = [Event|S0#s.wait],
    {noreply,S0#s { iref=next_ref(IRef), wait=Wait1 }};
handle_local({async,_From,Request},Socket,S0) ->
    IRef = S0#s.iref,
    Bin = ws_encode({async,IRef,Request}),
    gen_tcp:send(Socket, [0,Bin,255]),
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
	    gen_tcp:send(Socket, [0,Bin,255]),
	    S0
    catch
	error:Reason ->
	    Bin = ws_encode({reply,IRef,{error,Reason}}),
	    gen_tcp:send(Socket, [0,Bin,255]),
	    S0
    end;
handle_remote({cast,_IRef,M,F,As},_Socket,S0) ->
    catch (apply(M,F,As)),
    S0;
handle_remote(Other, _Socket, S0) ->
    io:format("handle_remote: got ~p\n", [Other]),
    S0.
