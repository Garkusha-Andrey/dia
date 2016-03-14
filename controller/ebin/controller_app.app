{application, controller_app,
[{vsn, "1.0.0"},
{description, ""},
{modules, [controller_app, controller_sup, controller_server, contrroler_lib, flows, restconf, routing, boot]},
{applications, [stdlib, kernel]},
{registered, [controller_app, controller_sup, controller_server]},
{mod, {controller_app, []}}

]}.
