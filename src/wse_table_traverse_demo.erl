%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%   Demonstration of table traversal using wse_table_traverse
%%% @end
%%% Created : 24 Apr 2024 by Tony Rogvall <tony@rogvall.se>

-module(wse_table_traverse_demo).
-export([run/2]).

-define(NBSP, [16#A0]).

queue_info() ->
    [#{ <<"track_name">> => <<"Track 1">>,
	<<"artist_name">> => <<"Artist 1">>,
	<<"album_name">> => <<"Album 1">>,
	<<"duration">> => 1234},
     #{ <<"track_name">> => <<"Track 2">>,
	<<"artist_name">> => <<"Artist 2">>,
	<<"album_name">> => <<"Album 2">>,
	<<"duration">> => 2345},
     #{ <<"track_name">> => <<"Track 3">>,
	<<"artist_name">> => <<"Artist 3">>,
	<<"album_name">> => <<"Album 3">>,
	<<"duration">> => 3456},
     #{ <<"track_name">> => <<"Track 4">>,
	<<"artist_name">> => <<"Artist 4">>,
	<<"album_name">> => <<"Album 4">>,
	<<"duration">> => 4567},
     #{ <<"track_name">> => <<"Track 5">>,
	<<"artist_name">> => <<"Artist 5">>,
	<<"album_name">> => <<"Album 5">>,
	<<"duration">> => 5678}].

run(Ws, ID) ->
    QueueInfo = queue_info(),
    N = length(QueueInfo),
    QueueFollow = 13,
    case ID of
	[$r|_] ->
	    {ok,TR} = wse:getElementById(Ws, ID),
	    {ok,ID1} = wse:get(Ws, TR, id),
	    io:format("ID1=~p\n", [ID1]),
	    update_queue_table_(Ws, TR, 1, N, QueueInfo, QueueFollow);
	[$t|_] ->
	    {ok,Table} = wse:getElementById(Ws, ID),
	    {ok,ID1} = wse:get(Ws, Table, id),
	    io:format("ID1=~p\n", [ID1]),
	    {ok,TBody} = wse:firstElementChild(Ws, Table),
	    {ok,TR0} = wse:firstElementChild(Ws, TBody),
	    {ok,TR0_ID} = wse:get(Ws, TR0, id),
	    io:format("ID2=~p\n", [TR0_ID]),
	    {ok,TR} = wse:nextElementSibling(Ws, TR0),
	    {ok,TR_ID} = wse:get(Ws, TR, id),
	    io:format("TR_ID=~p\n", [TR_ID]),
	    update_queue_table_(Ws, TR, 1, N, QueueInfo, QueueFollow)
    end.

update_queue_table_(Ws, TR, R, 0, _, QueueFollow) ->
    io:format("eot: R=~w, = QueueFollow=~p\n", [R,QueueFollow]),
    {ok,TD} = wse:firstElementChild(Ws, TR),
    FollowString = [$+|integer_to_list(QueueFollow)],
    set_queue_row_(Ws, TD, [FollowString,?NBSP,?NBSP,?NBSP]);
update_queue_table_(Ws, TR, R, I, [Item|Queue], QueueFollow) ->
    io:format("format row ~w: ~p\n", [R, Item]),
    {ok,TR_ID} = wse:get(Ws, TR, id),
    io:format("TR.id=~p\n", [TR_ID]),

    {ok,TD} = wse:firstElementChild(Ws, TR),
    {ok,TD_ID} = wse:get(Ws, TD, id),
    io:format("TD.id=~p\n", [TD_ID]),

    update_queue_row_(Ws, TD, Item),
    {ok,NextTR} = wse:nextElementSibling(Ws, TR),
    update_queue_table_(Ws, NextTR, R+1, I-1, Queue, QueueFollow);
update_queue_table_(Ws, TR, R, I, [], QueueFollow) ->
    io:format("format empty row ~w\n", [R]),
    {ok,TD} = wse:firstElementChild(Ws, TR),
    set_queue_row_(Ws, TD, [?NBSP,?NBSP,?NBSP,?NBSP]),
    {ok,NextTR} = wse:nextElementSibling(Ws, TR),
    update_queue_table_(Ws, NextTR, R+1, I-1, [], QueueFollow).


update_queue_row_(Ws, TD, Item) ->
    io:format("update_queue_row_ td=~p\n", [TD]),
    TrackName = maps:get(<<"track_name">>, Item, <<"Unknown">>),
    ArtistName = maps:get(<<"artist_name">>, Item, <<"Unknown">>),
    AlbumName = maps:get(<<"album_name">>, Item, <<"Unknown">>),
    Duration = maps:get(<<"duration">>, Item, 0),
    set_queue_row_(Ws, TD, [binary_to_list(TrackName),
			     binary_to_list(ArtistName),
			     binary_to_list(AlbumName), 
			     format_duration(Duration)]).

set_queue_row_(Ws, TD0, [Track,Artist,Album,Duration]) ->
    %% Note the use of firstChild to get the TextNode
    {ok,TrackNode} = wse:firstChild(Ws, TD0),  
    wse:set(Ws, TrackNode, "nodeValue", Track),
    io:format("TrackNode=~p\n", [TrackNode]),
    
    {ok,TD1} = wse:nextElementSibling(Ws, TD0),
    io:format("TD1=~p\n", [TD1]),
    {ok,ArtistNode} = wse:firstChild(Ws, TD1),
    wse:set(Ws, ArtistNode, "nodeValue", Artist),
    io:format("ArtistNode=~p\n", [ArtistNode]),

    {ok,TD2} = wse:nextElementSibling(Ws, TD1),
    io:format("TD2=~p\n", [TD2]),
    {ok,AlbumNode} = wse:firstChild(Ws, TD2),
    wse:set(Ws, AlbumNode, "nodeValue", Album),
    io:format("AlbumNode=~p\n", [AlbumNode]),

    {ok,TD3} = wse:nextElementSibling(Ws, TD2),
    io:format("TD3=~p\n", [TD3]),
    {ok,DurationNode} = wse:firstChild(Ws, TD3),
    wse:set(Ws, DurationNode, "nodeValue", Duration),
    io:format("DurationNode=~p\n", [DurationNode]),

    io:format("set row: tracknode=~p, ~p ~p ~p ~p\n",
	      [TrackNode, Track, Artist, Album, Duration]).


format_duration(DurationMs) ->
    DurationS = (DurationMs+500) div 1000,
    Min0 = DurationS div 60,
    Sec = DurationS rem 60,
    Hours = Min0 div 60,
    Min = Min0 rem 60,
    if Hours > 0 ->
	    integer_to_list(Hours)++":"++
		tl(integer_to_list(100+Min)) ++ ":" ++ 
		tl(integer_to_list(100+Sec));
       true ->
	    integer_to_list(Min) ++ ":" ++ tl(integer_to_list(100+Sec))
    end.
