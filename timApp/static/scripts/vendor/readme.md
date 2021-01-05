# Vendored libraries

This directory contains vendored libraries.

Usually JS dependencies are installed from NPM, but sometimes
the dependency is broken or needs major changes, in which
case it may make sense to vendor the package (to copy it to the source tree).

## Currently vendored libraries and reasons

* [ngx-bootstrap-datetime-popup][1]: Vendored because
  the precompiled package doesn't work on newest Angular (11). The package
  doesn't need any functional changes; it just needs to be compiled
  with the newest Angular.

[1]: https://www.npmjs.com/package/ngx-bootstrap-datetime-popup
