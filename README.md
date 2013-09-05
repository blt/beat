# beat - A small, textual TCP chat server.

The aim of this project is pedagogy. In particular, it's meant to provide a base
for intermediate Erlang developers at the Rackspace Hackday a chance to work on
and dissect an OTP project.

## Quick Start

To run `beat` ensure that you have [`relx`](https://github.com/erlware/relx)
installed on your system and do the following:

```
> make && relx
...
> _rel/bin/beat console
```
Now connect to localhost port 27182 and enjoy the sweet flow of integers.

## The relx problem.

The tool relx is supposed to make generating relups rather more simple. This
repository contains two branches of interest to follow along in reproducing.
First, checkout `v1` and create a fresh release.

```
> git checkout v1
Switched to branch 'v1'

> make
...

> relx
...
Resolved beat-2013.1
release successfully created!
```

You may now start up a `beat` instance. Confirm that the release layout looks
like:

```
_rel/lib:
beat_core-2013.1    kernel-2.15.3       stdlib-1.18.3
beat_tcp_api-2013.1 ranch-0.8.5

_rel/releases:
beat-2013.1
```

Cool. Checkout `v2` and build a relup:

```
> git checkout v2
Switched to branch 'v2'

> make
...

> relx relup
Starting relx build process ...
Resolving OTP Applications from directories:
    /Users/blt/projects/us/troutwine/beat/apps
    /Users/blt/projects/us/troutwine/beat/deps
    /Users/blt/.kerl/installs/R15B03/lib
    /Users/blt/projects/us/troutwine/beat/_rel

Resolving available releases from directories:
    /Users/blt/projects/us/troutwine/beat/apps
    /Users/blt/projects/us/troutwine/beat/deps
    /Users/blt/.kerl/installs/R15B03/lib
    /Users/blt/projects/us/troutwine/beat/_rel

Resolved beat-2013.2
Errors generating release
    beat_core: File not found: "beat_core.app"


Usage: relx [-n <relname>] [-v <relvsn>] [-g <goal>] [-u <upfrom>]
            [-o <output_dir>] [-l <lib_dir>]
            [--disable-default-libs [<disable_default_libs>]]
            [-V [<log_level>]] [-a <override_app>] [-c [<config>]]
            [-r <root_dir>] [*release-specification-file*]

  -n, --relname           Specify the name for the release that will be
                          generated
  -v, --relvsn            Specify the version for the release
  -g, --goal              Specify a target constraint on the system. These
                          are usually the OTP
  -u, --upfrom            Only valid with relup target, specify the
                          release to upgrade from
  -o, --output-dir        The output directory for the release. This is
                          `./` by default.
  -l, --lib-dir           Additional dirs that should be searched for OTP
                          Apps
  --disable-default-libs  Disable the default system added lib dirs (means
                          you must add them all manually [default: false]
  -V, --verbose           Verbosity level, maybe between 0 and 2 [default:
                          1]
  -a, --override_app      Provide an app name and a directory to override
                          in the form <appname>:<app directory>
  -c, --config            The path to a config file [default: ]
  -r, --root              The project root directory
```

Uh oh, that doesn't look right.

```
> ls _rel/releases _rel/lib
_rel/lib:
beat_tcp_api-2013.1 kernel-2.15.3       ranch-0.8.5         stdlib-1.18.3

_rel/releases:
beat-2013.1 beat-2013.2
```

The previous version of `beat_core` has gone missing and there was no relup
generated. This probably is very not right at all.
