-module(restconf).

-export([flow_send/1,
	 table_delete/1]).

-define(CONTROLLER_ADDR, "127.0.0.1:8080").


make_flow_url(TableId, FlowId) ->
    make_table_url(TableId) ++ "/flow/" ++ integer_to_list(FlowId).

make_table_url(TableId) ->
    "http://" ++ ?CONTROLLER_ADDR ++
    "/restconf/config/opendaylight-inventory:nodes/node/openflow:1/table/"
    ++ integer_to_list(TableId).

auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

request(Method, Body) ->
%%    {ok,{{"HTTP/1.1",200,"OK"},
%%                         [{"server","Jetty(8.1.15.v20140411)"},
%%                          {"content-length","0"}],
%%                         []}}

    {Res,Details} = httpc:request(Method, Body, [], []),
    case Res of
        ok ->
            {{_Protocol,Code,CodeText},
             _,
             Answer} = Details,
            io:format("httpc:request(): ret(~w) code(~w), answer(~s), body(~s)~n",
                          [Res, Code, CodeText, Answer]);
        error ->
            {Error, _} = Details,
            io:format("httpc:request(): error: ~w~n", [Error])
    end,
    Res.

put_method(URL, ContentType, Body) -> request(put,
                                              {URL,
                                               [auth_header("admin", "admin"),
                                                {"Content-Type", ContentType}],
                                               ContentType, Body}).

%%get(URL)                     -> request(get,  {URL, []}).

flow_send(noflow) ->
    ok;
flow_send({FlowId, Flow}) ->
    flow_send({0, FlowId, Flow});
flow_send({TableId, FlowId, Flow}) ->
    %%io:format("Sending:~n~s~n",[Flow]),
    put_method(make_flow_url(TableId, FlowId), "application/xml", Flow).

table_delete(TableId) ->
    request(delete, {make_table_url(TableId), [auth_header("admin", "admin")],
		     [], []}).