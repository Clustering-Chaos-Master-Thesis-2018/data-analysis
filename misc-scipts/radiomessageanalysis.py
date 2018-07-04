file = open("radio_messages_log_competition-radius-1_resync-2_2018-05-24_3")


def validate_ch_list_item(ch_list_item):
    return ch_list_item["node_id"] in range(0,50+1)


def validate_parsed(parsed):
    return parsed["cluster_id"] in range(0,50+1) and all([validate_ch_list_item(item) for item in parsed["ch_list"]])


def int_to_ch_status(i):
    return ["NOT_INITIALIZED",
    "NOT_CH",
    "TENTATIVE",
    "FINAL"][i]


def parse_ch_list_item(str_item):
    node_id = str_item[2:4] + str_item[0:2]
    node_id = int(node_id, 16)

    combined_field = str_item[4:6]
    combined_field = int(combined_field, 16)
    hop_count = combined_field & 0b00111111
    status = int_to_ch_status((combined_field & 0b11000000) >> 6)
    return {
        "node_id": node_id,
        "hop_count": hop_count,
        "status": status
    }


#print(parse_ch_list_item("2100C1"))


def get_byte(packet_body, col_index, byte_index):
    read_pos = col_index * 8 + byte_index * 2
    byte = packet_body[read_pos:read_pos + 2]
    return int(byte, 16)


def parse_ch_list(chlist_str):
    pass


def parse_line(line):
    parsed = {}
    data = line.split(":")
    metadata = data[0]


    packagedata = data[1]

    packagedata = packagedata[3:]
    packagedata = packagedata.split(" ")

    #packetheader = packagedata[:2]
    #packetbody = packagedata[3:]

    packagedata = "".join(packagedata)


    parsed["cluster_id"] = get_byte(packagedata, 2, 2)
    parsed["cluster_head_count"] = get_byte(packagedata, 2, 3)
    parsed["source_id"] = get_byte(packagedata, 3, 0)
    parsed["consecutive_cluster_round_count"] = get_byte(packagedata, 3, 1)



    start = 3 * 8 + 2 * 2
    if parsed["cluster_head_count"] > 0:
        ch_list_content = packagedata[start : start+parsed["cluster_head_count"]*6]
        parts = [parse_ch_list_item(ch_list_content[i:i + 6]) for i in range(0, len(ch_list_content), 6)]
        parsed["ch_list"] = parts
    else:
        parsed["ch_list"] = []
    return parsed

outfile = open("radio_messages_log_competition-radius-1_resync-2_2018-05-24_3-parsed.txt", "w")

for i, line in enumerate(file.readlines()):
    #if (i in [7816, 7817, 7818]):
    #print(f'{i} {parse_line(line)}')
    a = parse_line(line)
    if not validate_parsed(a):
        print(f'{i+1} {a}')
    outfile.write(f'{i+1} {a}\n')
    #print(line)


