%%desc 一个聊天室demo，用来测试html5 websocket 和 erlang 的通信
%%@auth tissot.cai@gmail.com
%%@date 2013-09-28

-module(chatroom_server).

-export([start/0, start/1]).

-export([
    loop_listen/1,
    loop_server/0,
    loop_client/1,
    handle_client/1
]).

-define(SERVER_PROC, server_proc).


start () ->
    start(9000).

%% 注意6666等一些端口不能使用，chrome把这些端口列为危险端口
start (Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [{active, true}, {packet, 0}, {mode, binary}]),

    register(?SERVER_PROC, spawn(?MODULE, loop_server, [])),
    spawn(?MODULE, loop_listen, [Listen]),
    io:format("start on port : ~p~n", [Port]).
   

loop_server () ->
    loop_server([]).
loop_server (ClientList) ->
    receive
        {new_client, ClientPid} ->
            loop_server([ClientPid | ClientList]);
        {close_client, ClientPid} ->
            loop_server(lists:delete(ClientPid, ClientList));
        {new_msg, SendPid, Msg} ->
            [Pid ! {send, Msg} || Pid <- ClientList, Pid /= SendPid],
            loop_server(ClientList);
        UnKnowMsg ->
            io:format("loop_server:~p~n", [UnKnowMsg])
    end. 


loop_listen (Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Pid = spawn(?MODULE, handle_client, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            loop_listen(Listen);
        _ ->
            gen_tcp:close(Listen)
    end.


handle_client (Socket) ->
    receive 
        {tcp, Socket, Data} ->
            SecData = html5:get_seccure_data(Data),
            gen_tcp:send(Socket, SecData),
            ?SERVER_PROC ! {new_client, self()},
            loop_client(Socket);
        UnKnowMsg ->
            exit(UnKnowMsg)
    end.

loop_client (Socket) ->
    receive
        {tcp, Socket, Data} ->
            RecData = html5:unpack_data(Data),
            SendData = html5:pack_data(RecData),
            ?SERVER_PROC ! {new_msg, self(), SendData},
            loop_client(Socket);
        {tcp_closed, Socket} ->
            ?SERVER_PROC ! {close_client, self()};
        {send, Data} ->
            gen_tcp:send(Socket, Data),
            loop_client(Socket);
        UnKnowMsg ->
            exit(UnKnowMsg)
    end.