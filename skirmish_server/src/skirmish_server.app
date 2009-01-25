{application, skirmish_server,
 [{description, "Skirmish Server"},
  {vsn, "0"},
  {modules, [skirmish_server, listener, client_handler]},
  {registered, [skirmish_server]},
  {applications, [kernel, stdlib]},
  {mod,
   {skirmish_server, []}
  }
 ]}.
