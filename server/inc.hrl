-record(buff, {
    fin = undefined,
    rsv1,
    rsv2,
    rsv3,
    opcode,
    mask_len = 0,
    is_mask = false,
    payload_len = 0,
    payload_data,
    is_end = false
}).