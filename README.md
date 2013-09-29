erlang-html5-chatroom
=====================

一个聊天室demo，用来测试html5 websocket 和 erlang 的通信

服务端
编译:
cd server
erlc *.erl

运行:
erl
1>chatroom:start(9000).
