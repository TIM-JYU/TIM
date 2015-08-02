"use strict";
var paliApp = angular.module('paliApp', ['ngSanitize']);
paliApp.TESTWITHOUTPLUGINS = false; // if one wants to test without pali plugins

paliApp.directive('paliRunner',['$sanitize','$compile',
                  function ($sanitize,$compile1) {"use strict";
                      // Tätä kutsutaan yhden kerran kun plugin otetaan käyttöön
                      timHelper.sanitize = paliApp.sanitize = $sanitize;
                      paliApp.compile = $compile1;
                      return paliApp.directiveFunction(); }]
);


paliApp.directiveFunction = function() {
"use strict";
// Koska tätä kutsutaan direktiivistä, tätä kutsutaan yhden kerran
    return {
		scope: {},
		controller: paliApp.Controller,
		link: paliApp.initScope,
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


paliApp.directiveTemplate = function () {
"use strict";
// Koska tätä kutsutaan directiveFunction-metodista, tätä kutsutaan yhden kerran
    if ( paliApp.TESTWITHOUTPLUGINS ) return '';
	return  '<div class="csRunDiv no-popup-menu">' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
				  '<div><label>{{inputstem}} <span><input type ="text" class="paliInput" ng-model="userword" ng-trim="false" placeholder="{{inputplaceholder}}" size="{{cols}}"></span></label>' +
				  ' <span class="unitTestGreen"  ng-show="runTestGreen" >&nbsp;ok&nbsp;</span>' +
				  ' <span class="unitTestRed"  ng-show="!runTestGreen">&nbsp;not&nbsp;</span>' +
                  '</div>' +
				  '<button ng-if="button"  ng-disabled="isRunning" ng-click="paliScope.saveText();">{{button}}</button>&nbsp&nbsp' +
				  '<a href="" ng-if="muokattu" ng-click="paliScope.initCode()">{{resetText}}</a>' +
                  '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +
				  '<pre class="" ng-if="error">{{error}}</pre>' +
				  '<pre  class="" ng-show="result">{{result}}</pre>' +
    		      '<p class="plgfooter">Here comes footer</p>' +
              '</div>';
};


paliApp.Controller = function($scope, $http, $transclude, $interval) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Controlleriksi.
// Tähän tullaan ensin ja sitten initScope-metodiin
// Siitä ei ole mitään hajua mistä se keksii tälle nuo parametrit???
    if (paliApp.TESTWITHOUTPLUGINS) return;
    $scope.paliScope = new PaliScope($scope);
    $scope.attrs = {};
    $scope.http = $http; 
    $scope.interval = $interval;

    // Luodaan $scope.attrs joka on avattuna sisällössä olev JSON tai HEX
    $transclude(function(clone,scope) { timHelper.initAttributes(clone,$scope);  });

    $scope.errors = [];
	$scope.muokattu = false;
    $scope.result = "";
};


paliApp.initScope = function (scope, element, attrs) {
"use strict";
// Tätä kutsutaan kerran jokaiselle pluginin esiintymälle.
// Angular kutsuu tätä koska se on sanottu direktiivifunktiossa Link-metodiksi.
    scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
    scope.plugin = element.parent().attr("data-plugin");
    scope.taskId = element.parent().attr("id");

    // Etsitään kullekin attribuutille arvo joko scope.attrs tai attrs-parametrista. Jos ei ole, käytetään oletusta.
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

    // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
    element[0].childNodes[0].outerHTML = timHelper.getHeading(scope, attrs, "header", "h4");
    var n = element[0].childNodes.length;
    if (n > 1) element[0].childNodes[n - 1].outerHTML = timHelper.getHeading(scope, attrs, "footer", 'p class="footer"');
    scope.paliScope.checkPalindrome();
    scope.attrs = {}; // not needed any more

    // seurataan userword-muuttujan muuttumista.  Angular myös päivittää näyttöä
    // automaattisesti koska on sanottu että ng-model="userword"
    scope.$watch('userword', function() { scope.paliScope.watchWord(); }, true);
};


// Tehdään kaikista toiminnallisista funktoista oma luokka, jotta
// niitä ei erikseen lisätä jokaisen pluginin esiintymän kohdalla uudelleen.
function PaliScope(scope) {
"use strict";
    this.scope = scope;
}


PaliScope.prototype.watchWord = function() {
"use strict";
    var $scope = this.scope;
    $scope.interval.cancel($scope.runTimer);
    $scope.runTimer = $scope.interval(function() { $scope.paliScope.checkPalindrome(); }, $scope.autoupdate);
    $scope.muokattu = ( $scope.initword !== $scope.userword );
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
    var $scope=this.scope;
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


PaliScope.prototype.saveText = function() {
"use strict";
    this.doSaveText(false);
};


PaliScope.prototype.doSaveText = function(nosave) {
"use strict";
    var $scope = this.scope;
    $scope.error = "... saving ...";
    $scope.isRunning = true;

    $scope.result = "";

    var uword = "";

    if ($scope.userword) uword = $scope.userword;

    var params = {
        'input': {
            'userword': uword,
            'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id},
            'paliOK': this.checkPalindrome()
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
