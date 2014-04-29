%%% File    : wse.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : WebSocket Erlang interface
%%% Created : 21 Dec 2009 by Tony Rogvall <tony@rogvall.se>

-module(wse).
-compile(export_all).

id(ID) when is_atom(ID) ->
    {object, ID};
id(ID) when is_list(ID) ->
    {object, ID}.

document()  -> id(document).
window()    -> id(window).
screen()    -> id(screen).
navigator() -> id(navigator).

%% force list to be encoded as a list when using term_to_binary!!!
%% this is handled in the BERT decoder
array(Elements) when is_list(Elements) ->
    ['array'|Elements].

send(Ws, Tag, Data) ->
    nsync(Ws, {send,Tag,Data}).

call(Ws, Object, Method, Args) ->
    call(Ws, Object, Method, Object, Args).

call(Ws, Object, Method, This, Args) when is_list(Args) ->
    rsync(Ws, {call, Object, Method, This, array(Args)}).

rcall(Ws, Object, Method, Args) ->
    rcall(Ws, Object, Method, Object, Args).

rcall(Ws, Object, Method, This, Args) when is_list(Args) ->
    dsync(Ws, {call, Object, Method, This, array(Args)}).


%% get attribute or value at index
get(Ws, Object, Attribute) ->
    rsync(Ws, {get, Object, Attribute}).

%% set attibute or array at index
set(Ws, Object, Attribute, Value) ->
    rsync(Ws, {set, Object, Attribute,Value}).

cast(Ws, Object, Method, Args) ->
    cast(Ws, Object, Method, Object, Args).

cast(Ws, Object, Method, This, Args) when is_list(Args) ->
    async(Ws, {call, Object, Method, This, array(Args)}).

new(Ws, Class, Args) when is_list(Args) ->
    rsync(Ws, {new,Class,array(Args)}).

%% newf(Ws, Args::string(), Body::string()) 
%% newf(Ws, "e,f", "'e.pageX'").
newf(Ws, Args, Body) when is_list(Args), is_list(Body) ->
    rsync(Ws, {newf,Args,Body}).

delete(Ws, {object,ID}) ->
    rsync(Ws, {delete,ID});
delete(_Ws, _) ->
    ok.

close(Ws) ->
    close(Ws, normal).

close(Ws,Reason) ->
    Ref = make_ref(),
    Ws ! {close,[Ref|self()],Reason},
    receive
	{reply, Ref, Reply} ->
	    Reply
    end.
%%
%% create event listener How=once,all
%% return an integer ID 
%%
create_event(Ws) ->
    create_event(Ws,all,[]).

create_event(Ws,How,Data) ->
    Ref = make_ref(),    
    Ws ! {create_event,[Ref|self()],How,Data},
    receive
	{reply, Ref, Reply} ->
	    Reply
    end.

wait_event(ID,Timeout) ->
    receive
	{notify,ID,Local,Remote} ->
	    {ok,{Local,Remote}}
    after Timeout ->
	    {error,timeout}
    end.
			
    

%% Short cuts to DOM access
createElement(Ws, Name) ->
    %% io:format("createElement: ~p\n", [Name]),
    {ok,E} = call(Ws, document(), createElement, [Name]),
    E.

createTextNode(Ws, Text) ->
    {ok,E} = call(Ws, document(), createTextNode, [Text]),
    E.

appendChild(Ws, Element, Child) ->
    {ok,_} = call(Ws, Element, appendChild, [Child]),
    ok.


load_image(Ws, Src) ->
    Image = createElement(Ws, "img"),
    set(Ws, Image, "src", Src),
    set(Ws, Image, "style.display", "none"),
    %% set(Ws, Image, "type", "image/jpeg");
    {ok,Head} = call(Ws, document(), getElementsByTagName, ["head"]),
    {ok,Elem} = get(Ws, Head, 0),
    appendChild(Ws, Elem, Image),
    %% wait for image to load?
    {ok,Image}.


%% Short cut to dynamically load java script library
load(Ws, Library) ->
    Script1 = createElement(Ws, "script"),
    set(Ws, Script1, "type", "text/javascript"),
    set(Ws, Script1, "src", Library),

    {ok,ID} = create_event(Ws),
    Script2 = createElement(Ws, "script"),
    Text=createTextNode(Ws, "Wse.notify("++integer_to_list(ID)++",'loaded');"),
    appendChild(Ws, Script2, Text),

    %% Append script's in head element
    {ok,Head} = call(Ws, document(), getElementsByTagName, ["head"]),
    {ok,Elem} = get(Ws, Head, 0),
    appendChild(Ws, Elem, Script1),
    appendChild(Ws, Elem, Script2),
    Result = wait_event(ID, 5000),
    io:format("wait event = ~p\n", [Result]),
    ok.


%% Sync and Async primitives	
rsync(Ws, Command) ->
    Ref = make_ref(),
    Ws ! {rsync,[Ref|self()],Command},
    receive
	{reply, Ref, Reply} ->
	    Reply
    end.

dsync(Ws, Command) ->
    Ref = make_ref(),
    Ws ! {dsync,[Ref|self()],Command},
    receive
	{reply, Ref, Reply} ->
	    Reply
    end.

nsync(Ws, Command) ->
    Ref = make_ref(),
    Ws ! {nsync,[Ref|self()],Command}.

async(Ws, Command) ->
    Ref = make_ref(),
    Ws ! {async,[Ref|self()],Command}.
