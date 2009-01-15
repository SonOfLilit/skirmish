{application, skirmish-server,
 [{description, "Skirmish Server"},
  {vsn, "0"},
  {modules, [skirmish_server, skirmish_server_sup, skirmish_server_listener]},
  {registered, []}, % TODO: figure out what processes we register
  {applications, [kernel, stdlib]},
  {mod, {skirmish_server_sup,[]}}
 ]}.
