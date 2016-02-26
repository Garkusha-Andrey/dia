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
    10 + Id;
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
            <apply-actions>
                <action>
                    <order>0</order>
                    <set-dl-dst-action>
                        <address>" ++ Gateway ++ "</address>
                    </set-dl-dst-action>
                </action>
                <action>
                    <order>1</order>
                    <output-action>
                        <output-node-connector>"
                            ++ integer_to_list(?PORT_INTERNAL) ++
                        "</output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
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


defaults(arp, [IpInternal,MaskInternal,
	       IpExternal,MaskExternal]) ->
[{1,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40002</priority>
    <flow-name>arp2bplane</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2054</type>
            </ethernet-type>
        </ethernet-match>
        <in-port>LOCAL</in-port>
        <arp-target-transport-address>"
        ++ IpInternal ++ "/" ++ MaskInternal ++
       "</arp-target-transport-address>
    </match>
    <id>1</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <output-action>
                        <output-node-connector>"
                            ++ integer_to_list(?PORT_INTERNAL) ++
                       "</output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"},
{2,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40002</priority>
    <flow-name>arp2ext</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2054</type>
            </ethernet-type>
        </ethernet-match>
        <in-port>LOCAL</in-port>
        <arp-target-transport-address>"
        ++ IpExternal ++ "/" ++ MaskExternal ++
       "</arp-target-transport-address>
    </match>
    <id>2</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <output-action>
                        <output-node-connector>"
                            ++ integer_to_list(?PORT_EXTERNAL) ++
                       "</output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"},
{3,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<flow xmlns=\"urn:opendaylight:flow:inventory\">
    <priority>40001</priority>
    <flow-name>arp2ovs</flow-name>
    <match>
        <ethernet-match>
            <ethernet-type>
                <type>2054</type>
            </ethernet-type>
        </ethernet-match>
    </match>
    <id>3</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <output-action>
                        <output-node-connector>
                            LOCAL
                        </output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"}];

defaults(ip,
	 [IpInternal,IpExternal,MacSwitch,MacGateway])
->
[
{4,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
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
    <id>4</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <output-action>
                        <output-node-connector>"
                            ++ integer_to_list(?PORT_INTERNAL) ++
                       "</output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"},
{5,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
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
    <id>5</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <output-action>
                        <output-node-connector>
                            LOCAL
                        </output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"},



{6,
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
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
    <id>6</id>
    <table_id>0</table_id>
    <instructions>
        <instruction>
            <order>0</order>
            <apply-actions>
                <action>
                    <order>0</order>
                    <set-dl-src-action>
                        <address>" ++ MacSwitch ++ "</address>
                    </set-dl-src-action>
                </action>
                <action>
                    <order>1</order>
                    <set-dl-dst-action>
                        <address>" ++ MacGateway ++ "</address>
                    </set-dl-dst-action>
                </action>
                <action>
                    <order>2</order>
                    <output-action>
                        <output-node-connector>"
                            ++ integer_to_list(?PORT_EXTERNAL) ++
                       "</output-node-connector>
                    </output-action>
                </action>
            </apply-actions>
        </instruction>
    </instructions>
</flow>
"}].
