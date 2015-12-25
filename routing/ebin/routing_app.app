{application,routing_app,
             [{description,"Routing controller of diameter instances"},
              {vsn,"0.0.1"},
              {modules,[dia_stubs,flows,restconf,routing,routing_app,
                        routing_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,inets]},
              {mod,{routing_app,[]}},
              {env,[]}]}.
