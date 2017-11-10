# YConf

The Yahoo! Configuration System.

## What is it?

YConf is a configuration server. It contains a collection of configuration
files (JSON or YAML) that specify multi-dimensional configuration data (like
YCB, but with some minor design differences), and respond to contextual queries
from clients in either a RESTful JSON HTTP frontend or a [message
pack](http://msgpack.org) RPC frontend (or both simultaneously).

## Features

 * Specify configuration data in JSON or YAML (format is almost like YCB).
 * File-system back-end: yconf loads configuration from files.
 * HTTP back-end: yconf loads configuration from http endpoints.
 * Live reload: the server automatically reloads if configuration back-end
   changes.
 * HTTP JSON frontend (supports jsonp)
 * [Message pack](http://msgpack.org) RPC over TCP frontend.

## Usage example

### Command line options

```console
 Usage: yconf [-p|--path PATH] [-u|--url URL] [--http [BIND:]PORT]
              [--rpc [BIND:]PORT]

 Available options:
   -h,--help                Show this help text
   --yconf-config PATH      Optional configuration file for yconf (alternative to
                            command line)
   -p,--path PATH           location of configuration files
   -u,--url URL             URL of configuration master
   --http [BIND:]PORT       bind host and port to start the HTTP server
   --rpc [BIND:]PORT        bind host and port to start the TCP message pack RPC
                            server
   -l,--log-output PATH     file to write log (default to stderr)
   --log-level [Trace|Debug|Info|Warn|Error|Fatal]
                            log level (default: Info)
```

Multiple front-ends can be used (at least one should be defined by `--http`
and/or `--rpc`), but only one back-end must be used (`--path` or `--url`),
because there must be only one source of truth for configuration data.

If using the `--yconf-config` option, one can specify a file to read the
configurations. It's an alternative to the command line interface. It must be a
YAML file defining the configurations, example:

```yaml
path        : examples/1
http        : 127.0.0.1:4080
msgpack-rpc : 3000
log-output  : /var/log/yconf.log
log-level   : Debug
```

Here's another example using HTTP back-end, just the HTTP front-end (any host,
4080 port), and using defaults for the rest:

```yaml
url              : "http://localhost:8080/MANIFEST"
http             : 4080
```

If using the HTTP back-end (with `--url`), then this URL should return a JSON or
YAML data defining the "configuration manifest", which is simply a YAML
content defining three things (see [examples](examples)):

 * The configuration hash - this is used to keep track whether or not Yconf
   should update the configuration tree.
 * The dimensions URL (relative or absolute).
 * A list of rules URLs (relative or absolute).

Example:

```yaml
hash: f84adeee0cbf2a267e00df741d7d3c66
dimensions: /dimensions.yml
rules:
    - /config1.yml
    - /config2.json
```

Whenever the hash changes, the configuration system is reloaded (yconf checks
every 10 seconds).

### Example (HTTP JSON)

```console
$ yconf --path examples/1/ --http 4080
```

Here `path` specifies where are the configuration files (see
[examples](examples) folder); and `listen` specify the TCP port serve.
Optionally, one could specify `hostname:port` ( _e.g._ `127.0.0.1:4080` to
accept only local connections) to bind to a specific host.

Then, one can access contextual configuration, like:

```HTTP
GET /?runtime=client&device=iphone&environment=development HTTP/1.1
```

```json
{
    "status": 200,
    "result": {
        "location_backend": {
            "hostname": "mock.geo.data-example.com",
            "path": "/data/fetch"
        },
        "features": {
            "use_library_x": false,
            "library_z_version": 9,
            "use_library_y": false
        }
    }
}
```

You can get a specify sub-configuration by encoding the keys in the path:

```http
GET /features?runtime=client&device=iphone&environment=development HTTP/1.1
```

```json
{
    "status": 200,
    "result": {
        "use_library_x": false,
        "library_z_version": 9,
        "use_library_y": false
    }
}
```

Or even:

```http
GET /features/library_z_version?runtime=client&device=iphone&environment=development HTTP/1.1
```

```json
{
    "status": 200,
    "result": 9
}
```

## Implementation

YConf uses a blazing fast new algorithm for contextual projection. It
pre-process the configuration and the dimension combinations into an internal
tree. That gives linear performance guarantees:

 * Memory usage is **linear** to the size of the configuration.
 * Runtime (time to lookup a configuration) is **linear** to the number of
   configuration rules that matches the context query.

### Comparison to YCB

Yconf is 100% compatible to [YCB](https://github.com/yahoo/ycb-java), but it
also adds the following benefits:

 * Platform agnostic - integration is with Yconf is through JSON over HTTP or
   message-pack over TCP.
 * Decoupling - That means two things: ability to change configuration without
   redeployments, and, ability to use the same configuration base for different
   applications (possibly distinct platforms).
 * Support more types (all JSON types), instead of only "map" and "string".

Please note that Yconf don't have integrated cache (yet).
