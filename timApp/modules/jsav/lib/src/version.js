/**
* Version support
* Depends on core.js
*/
(function() {
  if (typeof JSAV === "undefined") { return; }
  var theVERSION = "v1.0.1-30-g2036c55";

  JSAV.version = function() {
    return theVERSION;
  };
})();
