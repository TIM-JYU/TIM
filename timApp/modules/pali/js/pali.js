"use strict";
var paliApp = angular.module('paliApp', ['ngSanitize']);
paliApp.TESTWITHOUTPLUGINS = false; // if one wants to test without pali plugins

paliApp.directive('paliRunner',['$sanitize','$compile',
                  function ($sanitize,$compile1) {"use strict";
                      timHelper.sanitize = paliApp.sanitize = $sanitize;
                      paliApp.compile = $compile1;
                      return paliApp.directiveFunction(); }]
);


paliApp.directiveTemplate = function () {
    if ( paliApp.TESTWITHOUTPLUGINS ) return '';
	return  '<div class="csRunDiv no-popup-menu">' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
				  '<div><label>{{inputstem}} <span><input type ="text" class="paliInput" ng-model="userword" ng-trim="false" placeholder="{{inputplaceholder}}" size="{{cols}}"></span></label>' +
				  ' <span class="unitTestGreen"  ng-show="runTestGreen" >&nbsp;ok&nbsp;</span>' +
				  ' <span class="unitTestRed"  ng-show="!runTestGreen">&nbsp;not&nbsp;</span>' +
                  '</div>' +
				  '<button ng-if="button"  ng-disabled="isRunning" ng-click="paliScope.runCode">{{button}}</button>&nbsp&nbsp' +
				  '<a href="" ng-if="muokattu" ng-click="paliScope.initCode">{{resetText}}</a>' +
                  '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +
				  '<pre class="" ng-if="error">{{error}}</pre>' +
				  '<pre  class="" ng-show="result">{{result}}</pre>' +
    		      '<p class="plgfooter">Here comes footer</p>' +
              '</div>';
};


paliApp.directiveFunction = function() {
"use strict";
    return {
		link: paliApp.initScope,
		scope: {},
		controller: paliApp.Controller,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: paliApp.directiveTemplate()
		// templateUrl: 'html/paliTempl.html'
	};
};


paliApp.Controller = function($scope, $http, $transclude, $interval) {
"use strict";
    if (paliApp.TESTWITHOUTPLUGINS) return;
    $scope.paliScope = new PaliScope($scope);
    $scope.attrs = {};
    $scope.http = $http;
    $scope.interval = $interval;

    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope);  });

    $scope.errors = [];
	$scope.muokattu = false;
    $scope.result = "";

    $scope.$watch('userword', $scope.paliScope.watchWord, true);
};


paliApp.initScope = function (scope, element, attrs) {
"use strict";
    scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
    scope.plugin = element.parent().attr("data-plugin");
    scope.taskId = element.parent().attr("id");

    timHelper.set(scope, attrs, "stem");
    timHelper.set(scope, attrs, "inputstem");
    timHelper.set(scope, attrs, "inputplaceholder", "Write your input here");
    timHelper.set(scope, attrs, "user_id");
    timHelper.set(scope, attrs, "initword", "");
    timHelper.set(scope, attrs, "state.userword", scope.initword);
    timHelper.set(scope, attrs, "button", "Save");
    timHelper.set(scope, attrs, "resetText", "Reset");
    timHelper.set(scope, attrs, "state.tries", 0);
    timHelper.set(scope, attrs, "max_tries");
    timHelper.set(scope, attrs, "cols", 20);
    timHelper.set(scope, attrs, "autoupdate", 500);
    timHelper.setn(scope, "tid", attrs, ".taskID"); // vain kokeilu että "juuresta" ottaminen toimii
    element[0].childNodes[0].outerHTML = timHelper.getHeading(attrs, "header", scope, "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(attrs, "footer", scope, 'p class="footer"');
    scope.paliScope.checkPalindrome();
    scope.attrs = {}; // not needed any more
};


function PaliScope(scope) {
    this.scope = scope;
}


PaliScope.prototype.watchWord = function() {
"use strict";
    this.scope.interval.cancel(this.scope.runTimer);
    this.scope.runTimer = this.scope.interval(this.checkPalindrome, this.scope.autoupdate);
    this.scope.muokattu = ( this.scope.initword !== this.scope.userword );
};


function isPalindrome(s) {
"use strict";
    var sc = s.toLowerCase();
    sc = sc.replace(/[^a-zåöä]/g,'');
    for (var i1 = 0, i2 = sc.length-1; i1< i2; i1++,i2--)
        if ( sc[i1] != sc[i2] ) return false;
    return true;
}


PaliScope.prototype.checkPalindrome = function() {
"use strict";
    var $scope = this.scope;
    $scope.interval.cancel($scope.runTimer);
    var pali = isPalindrome($scope.userword);
    $scope.runTestGreen = pali;
    return pali;
};


PaliScope.prototype.initCode = function() {
"use strict";
    var $scope = this.scope;
    $scope.muokattu = false;
    $scope.userword = $scope.initword;
    $scope.error = "";
    $scope.result = "";
};


PaliScope.prototype.runCode = function() {
"use strict";
    paliScope.doRunCode(this.scope, false);
};


PaliScope.prototype.doRunCode = function($scope, nosave) {
"use strict";
    $scope.error = "... running ...";
    $scope.isRunning = true;

    $scope.result = "";

    var uword = "";

    if ($scope.userword) uword = $scope.userword;

    var params = {
        'input': {
            'userword': uword,
            'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id},
            'paliOK': $scope.paliScope.checkPalindrome()
        }
    };

    if (nosave) params.input.nosave = true;
    var url = "/pali/answer";
    if ($scope.plugin) {
        url = $scope.plugin;
        var i = url.lastIndexOf("/");
        if (i > 0) url = url.substring(i);
        url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
    }

    $scope.http({method: 'PUT', url: url, data: params, headers: {'Content-Type': 'application/json'}, timeout: 20000}
    ).success(function (data, status, headers, config) {
            $scope.isRunning = false;
            $scope.error = data.web.error;
            $scope.result = data.web.result;
            $scope.tries = data.web.tries;
        }).error(function (data, status) {
            $scope.isRunning = false;
            $scope.errors.push(status);
            $scope.error = "Ikuinen silmukka tai jokin muu vika?";
            // $scope.error = data;
        });
};
