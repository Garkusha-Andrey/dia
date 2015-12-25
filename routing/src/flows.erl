-module(flows).

-export([make/5]).

-define(FLOW_BASIC_PRIO, 5).
-define(FLOW_EX_PRIO, 10).

-define(FLOW_BASIC_PREFIX, "dia-b-").
-define(FLOW_EX_PREFIX, "dia-ex-").

get_flow_id(basic, Ip) ->
    %% TODO3 add support for arbitrary chunk number
    {ok, {_,_,_,Id}} = inet:parse_ipv4_address(Ip),
    Id;
get_flow_id(exception, Ip) ->
    %% TODO implement properly
    100 + get_flow_id(basic, Ip).

make(Type, Ip, Mask, InstanceId, Gateway) ->

    case Type of
        exception ->
            Prio = ?FLOW_EX_PRIO,
            Name = ?FLOW_EX_PREFIX ++ Ip;
        basic ->
            Prio = ?FLOW_BASIC_PRIO,
            Name = ?FLOW_BASIC_PREFIX ++ Ip
    end,

    FlowId = get_flow_id(Type, Ip),

    io:format("making flow id(~w) prio(~w) name(~s) ip(~s/~s) instance(~w) gw(~w)~n",
              [FlowId, Prio, Name, Ip, Mask, InstanceId, Gateway]),

    {FlowId,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>" ++ integer_to_list(Prio) ++ "</priority>
    <flow-name>" ++ Name ++ "</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2048</type>
            </ethernet-type>
        </ethernet-match>
        <ipv4-destination>" ++ Ip ++ "/" ++ Mask ++ "</ipv4-destination>
    </match>
    <id>" ++ integer_to_list(FlowId) ++"</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
        </instruction>
    </instructions>
</flow>
"}.
