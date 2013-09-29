%% @desc html5 websocket 协议，协议格式参考 http://tools.ietf.org/html/rfc6455#section-1.2
%% @auth tissot.cai@gmail.com
%% @date 20130928

-module(html5).

-export([get_seccure_data/1, unpack_data/1, pack_data/1]).

%% 获取握手数据
get_seccure_data (BrowserSecData) ->
    Param = get_http_params(BrowserSecData),
    Key = <<"Sec-WebSocket-Key">>,
    {Key, ClientKey} = lists:keyfind(Key, 1, Param),

    ServerKey = base64:encode(crypto:sha([ClientKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>])),
    <<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ", ServerKey/binary, "\r\n\r\n">>.

%% 解析客户端发过来的数据
unpack_data (InData) ->
    <<_FIN:1, _RSV:3, _Opcode:4, Mask:1, Len:7, Ret/binary>> = InData,
    if
        Mask == 1 ->
            get_pay_load_data(Len, Ret);
        true ->
            <<>>
    end.

%% 打包下发客户端数据
pack_data (Data) ->
    Size = byte_size(Data),
    if
        Size < 126 ->
            <<1:1, 0:3, 2:4, 0:1, Size:7, Data/binary>>;
        true ->
            <<1:1, 0:3, 2:4, 0:1, 126:7, Size:16, Data/binary>>
    end.


get_http_params (HttpData) ->
    [
        {Key, Value}
        ||
        [Key, Value] <- [
            binary:split(Obj, <<": ">>)
            ||
            Obj <- binary:split(HttpData, <<"\r\n">>, [global]), Obj /= <<>>
        ]
    ].


get_pay_load_data (Len, Data) ->
    MaskingData = if
        Len < 126 ->
            Data;
        Len == 126 ->
            <<_PayLen:16, R/binary>> = Data,
            R;
        Len == 127 ->
            <<_PayLen:64, R/binary>> = Data,
            R
    end,
    unmark(MaskingData).

unmark (MaskingData) ->
    <<Marking:4/binary, PayLoadData/binary>> = MaskingData,
    unmark(Marking, PayLoadData, <<>>).
unmark (Marking = <<Mk1:8, Mk2:8, Mk3:8, Mk4:8>>, PayLoadData, Result) ->
    Size = size(PayLoadData),
    if
        Size == 0 ->
            Result;
        Size == 1 ->
            <<D:8>> = PayLoadData,
            <<Result/binary, (Mk1 bxor D)>>;
        Size == 2 ->
            <<D1:8, D2:8>> = PayLoadData,
            <<Result/binary, (Mk1 bxor D1), (Mk2 bxor D2)>>;
        Size == 3 ->
            <<D1:8, D2:8, D3:8>> = PayLoadData,
            <<Result/binary, (Mk1 bxor D1), (Mk2 bxor D2), (Mk3 bxor D3)>>;
        Size == 4 ->
            <<D1:8, D2:8, D3:8, D4:8>> = PayLoadData,
            <<Result/binary, (Mk1 bxor D1), (Mk2 bxor D2), (Mk3 bxor D3), (Mk4 bxor D4)>>;
        true ->
            <<D1:8, D2:8, D3:8, D4:8, Next/binary>> = PayLoadData,
            unmark(Marking, Next, <<Result/binary, (Mk1 bxor D1), (Mk2 bxor D2), (Mk3 bxor D3), (Mk4 bxor D4)>>)
    end.