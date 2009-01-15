{application, skirmish_server,
 [{description, "Skirmish Server"},
  {vsn, "0"},
  {modules, [skirmish_server, skirmish_server_sup, skirmish_server_listener]},
  {registered, [skirmish_server_sup]},
  {applications, [kernel, stdlib]},
  {mod,
   {skirmish_server, []}
  }
 ]}.
