{application, beat_core,
 [
  {description, "The core application of the 'beat' project'"},
  {vsn, "2013.2"},
  {modules,
   [
    beat_core_app,
    beat_core_sup,
    beat_core,
    beat_core_beater
   ]
  },
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {beat_core_app, []}},
  {env, []}
 ]
}.
