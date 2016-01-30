-module(flows).

-export([make/4, defaults/2]).

-define(FLOW_BASIC_PRIO, 5).
-define(FLOW_EX_PRIO, 10).

-define(FLOW_BASIC_PREFIX, "dia-b-").
-define(FLOW_EX_PREFIX, "dia-ex-").

-define(PORT_INTERNAL, 2).
-define(PORT_EXTERNAL, 1).

get_flow_id(basic, Ip) ->
    %% TODO3 add support for arbitrary chunk number
    {ok, {_,_,_,Id}} = inet:parse_ipv4_address(Ip),
    Id;
get_flow_id(exception, Ip) ->
    %% TODO implement properly
    100 + get_flow_id(basic, Ip).

make(Type, Ip, Mask, Gateway) ->

    case Type of
        exception ->
            Prio = ?FLOW_EX_PRIO,
            Name = ?FLOW_EX_PREFIX ++ Ip;
        basic ->
            Prio = ?FLOW_BASIC_PRIO,
            Name = ?FLOW_BASIC_PREFIX ++ Ip
    end,

    FlowId = get_flow_id(Type, Ip),

    io:format("making flow id(~w) prio(~w) name(~s) ip(~s/~s) sendto(~s)~n",
              [FlowId, Prio, Name, Ip, Mask, Gateway]),

    case Gateway of
	drop ->
	    Instructions = "
    <instructions>
        <instruction>
            <order>0</order>
        </instruction>
    </instructions>";

	_ ->
	    Instructions = "
    <instructions>
        <instruction>
            <order>0</order>
            <dst_mac>" ++ Gateway ++ "</dst_mac>
        </instruction>
        <instruction>
            <order>1</order>
            <output>1</output>
        </instruction>
    </instructions>"
    end,

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
    <table_id>0</table_id>"
++ Instructions ++ "
</flow>
"}.


defaults(arp, [_IpInternal,_MaskInternal,
	       _IpExternal,_MaskExternal]) ->
[{1,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40002</priority>
    <flow-name>arp-req</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2054</type>
            </ethernet-type>
        </ethernet-match>
        <in-port>LOCAL</in-port>
"
%%        <nw-destination>"
%%            ++ IpInternal ++ "/" ++ MaskInternal ++
%%        "</nw-destination>
"
    </match>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <output>" ++ integer_to_list(?PORT_INTERNAL) ++ "</output>
        </instruction>
        <instruction>
            <order>1</order>
            <output>" ++ integer_to_list(?PORT_EXTERNAL) ++ "</output>
        </instruction>
    </instructions>
</flow>
"},
{2,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40001</priority>
    <flow-name>arp-reply</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2054</type>
            </ethernet-type>
        </ethernet-match>
    </match>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <output>LOCAL</output>
        </instruction>
    </instructions>
</flow>
"}];

defaults(ip,
	 [IpInternal,IpExternal,MacSwitch,MacGateway])
->
[
{3,"
<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>50</priority>
    <flow-name>ip2bplane</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2048</type>
            </ethernet-type>
        </ethernet-match>
        <ipv4-source>" ++ IpInternal ++ "/255.255.255.255</ipv4-source>
    </match>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <output>" ++ integer_to_list(?PORT_INTERNAL) ++ "</output>
        </instruction>
    </instructions>
</flow>
"},
{4,"
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>50</priority>
    <flow-name>ip-from-bplane</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2048</type>
            </ethernet-type>
        </ethernet-match>
        <ipv4-destination>" ++ IpInternal ++ "/255.255.255.255</ipv4-destination>
    </match>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <output>LOCAL</output>
        </instruction>
    </instructions>
</flow>
"},



{5,"
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40</priority>
    <flow-name>ip2external</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2048</type>
            </ethernet-type>
        </ethernet-match>
        <ipv4-source>" ++ IpExternal ++ "/255.255.255.255</ipv4-source>
    </match>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <src_mac>" ++ MacSwitch ++ "</src_mac>
        </instruction>
        <instruction>
            <order>1</order>
            <dst_mac>" ++ MacGateway ++ "</dst_mac>
        </instruction>
        <instruction>
            <order>1</order>
            <output>" ++ integer_to_list(?PORT_EXTERNAL) ++ "</output>
        </instruction>
    </instructions>
</flow>
"}].
