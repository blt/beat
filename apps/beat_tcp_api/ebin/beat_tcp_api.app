{application, beat_tcp_api,
 [
  {description, "The TCP API of the 'beat' project"},
  {vsn, "2013.1"},
  {modules,
   [
    beat_tcp_api_app,
    beat_tcp_api_sup,
    beat_tcp_api,
    bta_protocol
   ]
  },
  {registered, []},
  {included_applications, [ranch]},
  {applications, [kernel, stdlib, beat_core]},
  {mod, {beat_tcp_api_app, []}},
  {env, []}
 ]
}.
