%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% File    : wse.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : WebSocket Erlang interface
%%% Created : 21 Dec 2009 by Tony Rogvall <tony@rogvall.se>

-module(wse).

-export([start/0]).
-export([start_link/1]).

-export([call/4, call/5]).
-export([rcall/4, rcall/5]).
-export([get/3, set/4]).
-export([cast/4, cast/5]).
-export([send/3]).
-export([new/3]).
-export([delete/2]).
-export([close/1, close/2]).
%% standard objects
-export([document/0, window/0, screen/0, navigator/0]).
%% object wrapper when passing string based id
-export([id/1]).
-export([array/1]).
-export([create_event/1, create_event/3]).
-export([wait_event/2]).
-export([wait_events/2]).
-export([createElement/2]).
-export([createElementNS/3]).
-export([createTextNode/2]).
-export([remove/2]).
-export([getElementsByTagName/2]).
-export([getElementById/2]).
-export([getAttribute/3]).
-export([appendChild/3]).
-export([setAttribute/4]).
-export([setStyle/3]).
-export([load/2]).
-export([load_image_sync/2]).
-export([load_image/2, load_image/3]).
-export([load_images/2, load_images/3]).
-export([load_video/4, load_video/5]).

%% header items
-export([header/1]).
-export([header/2]).
-export([session_header/0]).
-export([session_header/1]).

-compile(export_all).

%% -define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).

-type void() :: ok.
-type wse_object() :: {object, atom()|string()|integer()}.
-type wse_event() :: integer().
-type wse() :: pid().
-type url() :: string().

-type dom_id() :: atom()|string().
-type html_tag() :: atom().
-type attr_name() :: atom()|string()|list().
-type attr_value() :: atom()|string()|integer().
-type html_attr() :: {attr_name(),attr_value()}.
-type ehtml() :: atom() | string() | integer() |
		 {Tag::html_tag(),[html_attr()]} |
		 {Tag::html_tag(),[html_attr()],[ehtml()]}.

start() ->
    application:ensure_all_started(wse).

start_link(Args) ->
    wse_server:start_link(Args).

id(ID) when is_atom(ID) ->
    {object, ID};
id(ID) when is_list(ID) ->
    {object, ID}.

-spec document() -> wse_object().
document()  -> id(document).

-spec window() -> wse_object().
window()    -> id(window).

-spec screen() -> wse_object().
screen()    -> id(screen).

-spec navigator() -> wse_object().
navigator() -> id(navigator).

%% @doc
%%   Send (e)HTML to a DOM element sowmehere in the document.
%% @end
-spec send(Ws::wse(), Tag::dom_id(), Data::ehtml()) -> void().
		  
send(Ws, Tag, Data) ->
    nsync(Ws, {send,Tag,Data}).

%% @doc
%%   Call a Javascript method
%% @end
-spec call(Ws::wse(), Object::wse_object(), Method::atom(), Args::[term()]) ->
		  {ok, Value::term()}.
call(Ws, Object, Method, Args) ->
    call(Ws, Object, Method, Object, Args).

%% @doc
%%   Call a Javascript method setting this
%% @end
-spec call(Ws::wse(), Object::wse_object(),
	   This::wse_object(), Method::atom(), Args::[term()]) ->
		  {ok, Value::term()}.
call(Ws, Object, Method, This, Args) when is_list(Args) ->
    rsync(Ws, {call, Object, Method, This, array(Args)}).

%% @doc
%%   Remote call
%% @end

-spec rcall(Ws::wse(), Object::wse_object(), Method::atom(), Args::[term()]) ->
		   {ok, Value::term()}.
rcall(Ws, Object, Method, Args) ->
    rcall(Ws, Object, Method, Object, Args).

-spec rcall(Ws::wse(), Object::wse_object(),
	    This::wse_object(), Method::atom(), Args::[term()]) ->
		   {ok, Value::term()}.
rcall(Ws, Object, Method, This, Args) when is_list(Args) ->
    dsync(Ws, {call, Object, Method, This, array(Args)}).

%% @doc
%%  Get attribute or value at index
%% @end

-spec get(Ws::wse(), Object::wse_object(), 
	  Attribute::integer()|atom()|string()|list())
	 -> {ok,Value::term()} | {error,Reason::term()}.

get(Ws, Object, [LeafAttribute]) when is_list(LeafAttribute);
				      is_atom(LeafAttribute) ->
    get(Ws, Object, LeafAttribute);
get(Ws, Object, [Attribute | SubAttributes]) when is_list(Attribute);
						  is_atom(Attribute) ->
    {ok, AttributeObject} = get(Ws, Object, Attribute),
    get(Ws, AttributeObject, SubAttributes);
get(Ws, Object, Attribute) ->
    rsync(Ws, {get, Object, Attribute}).

%% @doc 
%%   Set attibute or array at index
%% @end
-spec set(Ws::wse(), Object::wse_object(), 
	  Attribute::attr_name(),
	  Value::term()) -> ok | {error,Reason::term()}.

set(Ws, Object, [LeafAttribute], Value) when is_list(LeafAttribute);
					     is_atom(LeafAttribute) ->
    rsync(Ws, {set, Object, LeafAttribute, Value});
set(Ws, Object, [Attribute | SubAttributes], Value) when is_list(Attribute);
							 is_atom(Attribute) ->
    {ok, AttributeObject} = get(Ws, Object, Attribute),
    set(Ws, AttributeObject, SubAttributes, Value);
set(Ws, Object, Attribute, Value) ->
    rsync(Ws, {set, Object, Attribute, Value}).

%% @doc
%%   Cast like call, but with no return value
%% @end
-spec cast(Ws::wse(), Object::wse_object(), Method::atom(), Args::[term()]) ->
		  ok.
cast(Ws, Object, Method, Args) ->
    cast(Ws, Object, Method, Object, Args).

-spec cast(Ws::wse(), Object::wse_object(), Method::atom(), 
	   Thus::wse_object(), Args::[term()]) ->
		  ok.
cast(Ws, Object, Method, This, Args) when is_list(Args) ->
    async(Ws, {call, Object, Method, This, array(Args)}).

%% @doc
%%   Create a new Java script object
%% @end
new(Ws, Class, Args) when is_list(Args) ->
    rsync(Ws, {new,Class,array(Args)}).

%% @doc
%%    Create a new function.
%%    <pre>
%%       newf(Ws, "e,f", "'e.pageX'").
%%    </pre>
%% @end
-spec newf(Wse::wse(), Agrs::string(), Body::string()) ->
		  wse_object().

newf(Ws, Args, Body) when is_list(Args), is_list(Body) ->
    rsync(Ws, {newf,Args,Body}).

%% @doc
%%   Release an object from the encoding object array.
%%   use with care
%% @end

-spec delete(Ws::wse(), Object::wse_object()) ->
		    void().

delete(Ws, {object,ID,_Ref}) ->
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

%% @doc
%%   Force list to be encoded as a list (instead of a string) 
%%   when using term_to_binary, this is handled in the wse decoder
%% @end

-spec array([term()]) -> list().

array(Elements) when is_list(Elements) ->
    ['array'|Elements].

%% @doc
%%   Create event listener that can be used to wait
%%   for events from java script.
%%   Same as create_event(Ws, all, []).
%% @end

-spec create_event(Ws::wse()) ->
			  {ok, wse_event()}.
create_event(Ws) ->
    create_event(Ws,all,[]).

%% @doc
%%   Create event listener with two flavours
%% @end
-spec create_event(Ws::wse(), How::once|all, Data::term()) ->
			  {ok, wse_event()}.

create_event(Ws,How,Data) ->
    Ref = make_ref(),    
    Ws ! {create_event,[Ref|self()],How,Data},
    receive
	{reply, Ref, Reply} ->
	    Reply
    end.

%% @doc
%%  Wait for an event sent from java script.
%%  Reply will contain both the local data installed with the
%%  event it self, and the data sent by java script, as a pair.
%% @end
-spec wait_event(ID::wse_event(), Timeout::timeout()) ->
			{ok,{Local::term(), Remote::term()}}.

wait_event(ID,Timeout) ->
    receive
	{notify,ID,Local,Remote} ->
	    {ok,{Local,Remote}}
    after Timeout ->
	    {error,timeout}
    end.

%% @doc
%%  Wait for multiple events sent from java script.
%%  Reply will contain both the local data installed with the
%%  event it self, and the data sent by java script, as a pair.
%% @end
-spec wait_events(ID::[wse_event()], Timeout::timeout()) ->
	  {ok,[{ID::wse_event(),{Local::term(), Remote::term()}}]}.

wait_events(IDs,Timeout) ->
    wait_events_(IDs,Timeout,[]).

wait_events_([ID|IDs],Timeout,Acc) ->
    receive
	{notify,ID,Local,Remote} ->
	    wait_events_(IDs,Timeout,[{ID,{Local,Remote}}|Acc])
    after Timeout ->
	    {error,timeout}
    end;
wait_events_([],_Timeout,Acc) ->
    {ok, lists:reverse(Acc)}.
			
%% Short cuts to DOM access

%% @doc
%%   Short cut for
%%   <pre>
%%   {ok,E} = call(Ws, document(), createElement, [Name]), E
%%   </pre>
%% @end
-spec createElement(Ws::wse(), Name::string()) -> wse_object().
createElement(Ws, Name) ->
    {ok,E} = call(Ws, document(), createElement, [Name]),
    E.


-spec createElementNS(Ws::wse(), Ns::string(), Name::string()) -> wse_object().
createElementNS(Ws, Ns, Name) ->
    {ok,E} = call(Ws, document(), createElementNS, [Ns, Name]),
    E.

%% @doc
%%   Short cut for
%%   <pre>
%%   {ok,E} = call(Ws, document(), createTextNode, [Text]), E
%%   </pre>
%% @end
-spec createTextNode(Ws::wse(), Text::string()) -> wse_object().
createTextNode(Ws, Text) ->
    {ok,E} = call(Ws, document(), createTextNode, [Text]),
    E.

-spec remove(Ws::wse(), Element::wse_object()) ->
		    void().

remove(Ws, Element) ->
    call(Ws, Element, remove, []).

%% 
%% Set attribute value
%%
-spec setAttribute(Ws::wse(), Obj::wse_object(), 
		   Attribute::attr_name(), Value::attr_value()) -> ok.
setAttribute(Ws, Obj, Attribute, Value) ->
    {ok,_E} = call(Ws, Obj, setAttribute, [Attribute, Value]),
    ok.

-spec getAttribute(Ws::wse(), Obj::wse_object(), 
		   Attribute::attr_name()) -> {ok,Value::attr_value()}.
getAttribute(Ws, Obj, Attribute) ->
    call(Ws, Obj, getAttribute, [Attribute]).

%% 
%% Set attribute values (FIXME, need special call)
%%
-spec setStyle(Ws::wse(), Obj::wse_object(), Value::string()) -> ok.
setStyle(Ws, Obj, Value) ->
    setAttribute(Ws, Obj, style, Value).

%% @doc
%%   Short cut for
%%   <pre>
%%   {ok,_} = call(Ws, Element, appendChild, [Child]), ok
%%   </pre>
%% @end
-spec appendChild(Ws::wse(), Element::wse_object(), Child::wse_object()) ->
			 ok.
appendChild(Ws, Element, Child) ->
    {ok,_} = call(Ws, Element, appendChild, [Child]),
    ok.

%% @doc
%%    Retrive an array of DOM objects by tag name
%%    Short cut for
%%    <ptr>
%%         call(Ws, document(), getElementsByTagName, [Name])
%%    </ptr>
%% @end
%%
-spec getElementsByTagName(Ws::wse(), Name::string()) ->
				  {ok,Array::wse_object()} | 
				  {error,Reason::string()}.
getElementsByTagName(Ws, Name) ->
    call(Ws, document(), getElementsByTagName, [Name]).


%% @doc
%%    Retrive a DOM object by its id
%%    Short cut for
%%    <ptr>
%%         call(Ws, document(), getElementById, [ID]).
%%    </ptr>
%% @end
%%
-spec getElementById(Ws::wse(), ID::string()) ->
			    {ok,Elem::wse_object()} | 
			    {error,Reason::string()}.
getElementById(Ws, ID) ->
    call(Ws, document(), getElementById, [ID]).

%% @doc 
%%    Get first child
%% @end
firstChild(Ws, Object) ->
    get(Ws, Object, firstChild).

%% @doc 
%%    Get last child
%% @end
lastChild(Ws, Object) ->
    get(Ws, Object, lastChild).

%% @doc
%%    Get next sibling
%% @end
nextSibling(Ws, Object) ->
    get(Ws, Object, nextSibling).

%% @doc
%%   Load an image into the document head and return 
%%   image the object.
%% @end
-spec load_image_sync(Ws::wse(), Src::url()) ->
    {ok,Image::wse_object()}.

load_image_sync(Ws, Src) ->
    Image = createElement(Ws, "img"),
    wait_complete(Ws, Image, Src),
    setStyle(Ws, Image, "display:none"),
    {ok,Heads} = getElementsByTagName(Ws, "head"),
    {ok,Head} = get(Ws, Heads, 0),
    appendChild(Ws, Head, Image),
    %% wait for image to load?
    {ok,Image}.


%% load multiple images and wait for them all
load_images(Ws, Files) ->
    {ok,Heads} = getElementsByTagName(Ws, "head"),
    {ok,Head} = get(Ws, Heads, 0),
    load_images(Ws, Head, Files).

load_images(Ws, Parent, Files) ->
    IDList =
	[begin
	     Image = createElement(Ws, "img"),
	     setStyle(Ws, Image, "display:none"),	 
	     ID = init_image_onload(Ws, Parent, Image, Src),
	     {ID, Image} 
	 end || Src <- Files],
    {ok,_} = wait_events([ID || {ID,_} <- IDList], 5000),
    lists:foreach(
      fun({_ID, Image}) ->
	      appendChild(Ws, Parent, Image)
      end, IDList),
    {ok, [Image || {_,Image} <- IDList]}.
    
%% @doc
%%   Load an image into the document head and return 
%%   image the object. Using event handler and onload
%% @end
-spec load_image(Ws::wse(), Src::url()) ->
	  {ok,Image::wse_object()}.

load_image(Ws, Src) ->
    {ok,Heads} = getElementsByTagName(Ws, "head"),
    {ok,Head} = get(Ws, Heads, 0),
    load_image(Ws, Head, Src).

-spec load_image(Ws::wse(), Parent::wse_object(), Src::url()) ->
	  {ok,Image::wse_object()}.
%% Load image into a child (like a div tag)
load_image(Ws, Parent, Src) ->
    Image = createElement(Ws, "img"),
    setStyle(Ws, Image, "display:none"),
    case wait_image_loaded(Ws, Parent, Image, Src, 5000) of
	{ok, _} ->
	    appendChild(Ws, Parent, Image),
	    {ok, Image};
	Error -> 
	    Error
    end.


%% @doc
%%   Load an image into the document head and return 
%%   image the object. Using event handler and onload
%% @end
-spec load_video(Ws::wse(), Src::url(), Width::integer(), Height::integer()) ->
	  {ok,Video::wse_object()}.

load_video(Ws, Src, Width, Height) ->
    {ok,Heads} = getElementsByTagName(Ws, "head"),
    {ok,Head} = get(Ws, Heads, 0),
    load_video(Ws, Head, Src, Width, Height).

-spec load_video(Ws::wse(), Parent::wse_object(), Src::url(), 
		 Width::integer(), Height::integer()) ->
	  {ok,Video::wse_object()}.
%% Load image into a child (like a div tag)
load_video(Ws, Parent, Src, Width, Height) ->
    Video = createElement(Ws, "video"),
    setStyle(Ws, Video, "display:none"),
    set(Ws, Video, width, Width),
    set(Ws, Video, height, Height),
    set(Ws, Video, controls, true),
    Source = createElement(Ws, "source"),
    appendChild(Ws, Video, Source),
    set(Ws, Source, "src", Src),
    set(Ws, Source, "type", "video/mp4"),
    appendChild(Ws, Parent, Video),
    {ok, Video}.


wait_image_loaded(Ws, Parent, Image, Src, Timeout) ->
    ID = init_image_onload(Ws, Parent, Image, Src),
    wait_event(ID, Timeout).

init_image_onload(Ws, Parent, Image, Src) ->
    {ok,ID} = create_event(Ws),
    {ok,LoadedScript} = make_notify_script(Ws,Parent,ID),
    set(Ws, Image, onload, LoadedScript),
    set(Ws, Image, "src", Src),
    ID.

%% Create Wse.notify loaded script
make_notify_script(Ws,Parent,ID) ->
    LoadedScript = createElement(Ws, "script"),
    Text = createTextNode(Ws,"Wse.notify("++integer_to_list(ID)++",'loaded');"),
    appendChild(Ws, LoadedScript, Text),
    %% Append script's in head element
    appendChild(Ws, Parent, LoadedScript),
    {ok,LoadedScript}.


wait_complete(Ws, Image, Src) ->
    wait_complete(Ws, Image, Src, 100, 10).

wait_complete(Ws, Image, Src, CheckTmo) ->
    wait_complete(Ws, Image, Src, CheckTmo, 3).

wait_complete(Ws, Image, Src, CheckTmo, NumberOfChecks) ->
    set(Ws, Image, "src", Src),
    wait_complete_(Ws, Image, CheckTmo, NumberOfChecks).

wait_complete_(_Ws, _Image, _CheckTmo, 0) ->
    {error, timeout};
wait_complete_(Ws, Image, CheckTmo, I) ->
    %% io:format("complete: ~w\n", [I]),
    case get(Ws, Image, complete) of
	{ok,true} ->
	    case get(Ws, Image, naturalWidth) of
		{ok,0} -> 
		    io:format("width=0 (wait)\n", []),
	    	    wait_complete_(Ws, Image, CheckTmo, I-1);
		{ok,_Width} ->
		    %% io:format("width=~w\n", [_Width]),
		    {ok, Image}
	    end;
	{ok,false} ->
	    timer:sleep(CheckTmo),
	    wait_complete_(Ws, Image, CheckTmo, I-1)
    end.

%% @doc
%%   Load a java script library, and wait for it to load.
%% @end
-spec load(Ws::wse(), Library::url()) -> ok.

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
    _Result = wait_event(ID, 5000),
    %% io:format("wait event = ~p\n", [_Result]),
    ok.

header(Ws) when is_pid(Ws) ->
    Ref = make_ref(),
    Ws ! {header,[Ref|self()]},
    receive
	{reply, Ref, Reply} ->
	    Reply
    after 5000 ->
	    {error,timeout}
    end.
    
header(Ws, ItemName) when is_pid(Ws) ->
    Ref = make_ref(),
    Ws ! {header,ItemName, [Ref|self()]},
    receive
	{reply, Ref, Reply} ->
	    Reply
    after 5000 ->
	    {error,timeout}
    end.

session_header() ->
    %% Header stored in process dictionary
    get(header).

session_header(ItemName) ->
    case get(header) of
	undefined ->
	    %% or crash??
	    {error, no_header};
	Header ->
	    case lists:keyfind(ItemName, 1, Header) of
		{ItemName, ItemValue} ->
		    {ok, ItemValue};
		false ->
		    {error, unknown_header_item}
	    end
    end.

%% Sync and Async primitives	
rsync(Ws, Command) ->
    Ref = make_ref(),
    ?dbg("rsync command ~p\n", [Command]),
    Ws ! {rsync,[Ref|self()],Command},
    receive
	{reply, Ref, Reply} ->
	    ?dbg("rsync reply ~p\n", [Reply]),
	    Reply
    end.

dsync(Ws, Command) ->
    Ref = make_ref(),
    ?dbg("dsync command ~p\n", [Command]),
    Ws ! {dsync,[Ref|self()],Command},
    receive
	{reply, Ref, Reply} ->
	    ?dbg("dsync reply ~p\n", [Reply]),
	    Reply
    end.

nsync(Ws, Command) ->
    Ref = make_ref(),
    ?dbg("nsync command ~p\n", [Command]),
    Ws ! {nsync,[Ref|self()],Command}.

async(Ws, Command) ->
    Ref = make_ref(),
    ?dbg("async command ~p\n", [Command]),
    Ws ! {async,[Ref|self()],Command}.
