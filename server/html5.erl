%% @desc html5 websocket 协议，协议格式参考 http://tools.ietf.org/html/rfc6455#section-1.2
%% @auth tissot.cai@gmail.com
%% @date 20130928

-module(html5).
-include("inc.hrl").

-export([
    get_seccure_data/1,
    unpack_data/2,
    pack_data/1,
    mask/1,
    unmask/1
]).

%% 获取握手数据
get_seccure_data (BrowserSecData) ->
    Param = get_http_params(BrowserSecData),
    Key = <<"Sec-WebSocket-Key">>,
    {Key, ClientKey} = lists:keyfind(Key, 1, Param),

    %%ServerKey = base64:encode(crypto:sha([ClientKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>])),
    ServerKey = base64:encode(crypto:hash(sha, [ClientKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>])),
    <<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ", ServerKey/binary, "\r\n\r\n">>.

%% 解析客户端发过来的数据
unpack_data (InData, Buff) ->
    NewBuff = if
        Buff #buff.fin == undefined ->
            <<FIN:1, RSV1:1, RSV2:1, RSV3:1, Opcode:4, Mask:1, Len:7, PayloadData/binary>> = InData,

            {PayloadLen, Data} = if
                Len < 126 ->
                    {Len, PayloadData};
                Len == 126 ->
                    <<PayLen:16, R/binary>> = PayloadData,
                    {PayLen, R};
                Len == 127 ->
                    <<PayLen:64, R/binary>> = PayloadData,
                    {PayLen, R}
            end,

            MaskLen = if
                Mask == 1 ->
                    4;
                true ->
                    0
            end,

            Buff #buff{
                fin      = FIN,
                rsv1     = RSV1,
                rsv2     = RSV2,
                rsv3     = RSV3,
                opcode   = Opcode,
                mask_len = MaskLen,
                is_mask  = Mask == 1,
                payload_len  = PayloadLen,
                payload_data = Data,
                is_end       = PayloadLen == (byte_size(Data) - MaskLen)
            };
        true ->
            NewPayloadData = <<(Buff #buff.payload_data)/binary, InData/binary>>,
            Buff #buff{
                payload_data = NewPayloadData,
                is_end = Buff #buff.payload_len == (byte_size(NewPayloadData) - Buff #buff.mask_len)
            }
    end,

    if
        NewBuff #buff.is_end ->
            if
                NewBuff #buff.is_mask ->
                    NewBuff #buff{
                        payload_data = unmask(NewBuff #buff.payload_data)
                    };
                true ->
                    NewBuff
            end;
        true ->
            NewBuff
    end.


%% 打包下发客户端数据
pack_data (Data) ->
    Size = byte_size(Data),
    Mask = 0,   %todo issue when send by mask

    Head = if
        Size < 126 ->
            <<1:1, 0:1, 0:1, 0:1, 2:4, Mask:1, Size:7>>;
        Size > 125, Size =< 4294967295 ->
            <<1:1, 0:1, 0:1, 0:1, 2:4, Mask:1, 126:7, Size:16/unsigned>>;
        true ->
            <<1:1, 0:1, 0:1, 0:1, 2:4, Mask:1, 127:7, Size:64/unsigned>>
    end,

    NData = if
        Mask == 1 ->
            mask(Data);
        true ->
            Data
    end,

    <<Head/binary, NData/binary>>.

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

unmask (MaskingData) ->
    <<Marking:4/binary, PayLoadData/binary>> = MaskingData,
    unmask(Marking, PayLoadData, <<>>).
unmask (Marking = <<Mk1:8, Mk2:8, Mk3:8, Mk4:8>>, PayLoadData, Result) ->
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
            unmask(Marking, Next, <<Result/binary, (Mk1 bxor D1), (Mk2 bxor D2), (Mk3 bxor D3), (Mk4 bxor D4)>>)
    end.

mask (Data) ->
    {_, _, MaskKey} = erlang:now(),
    MaskData = unmask(<<MaskKey:32, Data/binary>>),
    <<MaskKey:32, MaskData/binary>>.
