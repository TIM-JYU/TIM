/* globals angular, $ */

var timApp = angular.module('timApp');

timApp.defineMath = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users, ParCompiler) {
    "use strict";

    // for compatibility, we make the math processing functions available from scope too
    sc.processAllMathDelayed = ParCompiler.processAllMathDelayed;
    sc.processAllMath = ParCompiler.processAllMath;
    sc.processMath = ParCompiler.processMath;
    sc.processAllMathDelayed($('body'), 1500);
};
