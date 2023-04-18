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
* Mathquill: Vendored because it is a javascript library with no npm support. 
  Code is from https://github.com/digabi/mathquill and built with make. 
  Declaration file is from https://github.com/joaofogoncalves/ngx-mathquill
  and there are slight modification done to it. export default MathQuill is added 
  to the bottom of mathquill.js.

[1]: https://www.npmjs.com/package/ngx-bootstrap-datetime-popup
