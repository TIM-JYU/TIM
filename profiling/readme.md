# Profiling

There are two ways to profile TIM:

* Using Werkzeug's `ProfilerMiddleware`
* Using py-spy

Using py-spy is easier and more useful because it can profile any Python process
without code changes.

Werkzeug's profiler is currently present only in TIM container.

## py-spy

* Install [py-spy](https://github.com/benfred/py-spy) in the host machine
* Start TIM normally if it's not running already
* Run `./profile <containername>`, for example `./profile tim` or `./profile csplugin`
* Perform actions in the application that you want to measure
* Stop profiling with Ctrl+C
* Open the given link in browser to see the flamegraph

Sudo access is required to use py-spy.

## Werkzeug's `ProfilerMiddleware`

Werkzeug profiling can be enabled with the configuration option `PROFILE = True`.

When enabled, each request generates a `.prof` file in the directory `<repo>/timApp/static/profiling`.
You can analyze the files with any software that accepts `.prof` files, such as
[SnakeViz](https://jiffyclub.github.io/snakeviz/).
