{application,controller_app,
             [{description,"Controller of diameter instances"},
              {vsn,"0.0.1"},
              {modules,[controller_app,controller_server,controller_sub,
                        controller_sup]},
              {registered,[controller_app,controller_sup,controller_server]},
              {applications,[kernel,stdlib]},
              {mod,{controller_app,[]}},
              {env,[]}]}.
