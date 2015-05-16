"use strict";

var PALITESTWITHOUTPLUGINS = false; // if one wants to test without pali plugins

var paliApp = angular.module('paliApp', ['ngSanitize']);

paliApp.directive('paliRunner',['$sanitize','$compile', function ($sanitize,$compile1) {"use strict"; paliApp.sanitize = $sanitize; paliApp.compile = $compile1; return paliApp.directiveFunction(); }]);


paliApp.getHeading = function(a,key,$scope,defElem) {
"use strict";
	var h = paliApp.set($scope,a,key,"");
	if ( !h ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = defElem;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	var ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	try {
	  val = decodeURIComponent(encodeURI(val));
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = paliApp.sanitize(html);
	return html;
};


paliApp.directiveTemplatePali = function () {
    if ( PALITESTWITHOUTPLUGINS ) return '';
	return  '<div class="paliRunDiv no-popup-menu">' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
				  '<div></div><label>{{inputstem}} </label><span><input type ="text" class="paliInput" ng-model="userword" ng-trim="false" placeholder="{{inputplaceholder}}"></span></div>'+
				  '<button ng-if="isRun"  ng-disabled="isRunning" ng-click="runCode();">{{buttonText}}</button>&nbsp&nbsp'+
				  '<pre class="csRunError" ng-if="runError">{{error}}</pre>'+
				  '<pre  class="console" ng-show="result">{{result}}</pre>'+
    		      '<p class="plgfooter">Here comes footer</p>'+
              '</div>';
};


paliApp.set = function(scope,attrs,name,def) {
"use strict";
    scope[name] = def;
    if ( attrs && attrs[name] ) scope[name] = attrs[name];
    if ( scope.attrs && scope.attrs[name] ) scope[name] = scope.attrs[name];
    if ( scope[name] == "None" ) scope[name] = "";
    return scope[name];
};


paliApp.directiveFunction = function() {
"use strict";
    return {
		link: function (scope, element, attrs) {
            scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
            scope.plugin = element.parent().attr("data-plugin");
            scope.taskId  = element.parent().attr("id");

            paliApp.set(scope,attrs,"stem");
            paliApp.set(scope,attrs,"inputplaceholder","Write your input here");
            paliApp.set(scope,attrs,"user_id");
            paliApp.set(scope,attrs,"button","Save");
            paliApp.set(scope,attrs,"resetText","Reset");
            element[0].childNodes[0].outerHTML = paliApp.getHeading(attrs,"header",scope,"h4");
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = csApp.getHeading(attrs,"footer",scope,'p class="footer"');
		},
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
		template: paliApp.directiveTemplatePali()
		// templateUrl: 'html/paliTempl.html'
	};
};


paliApp.Hex2Str = function(s) {
"use strict";
  var result = '';
  for (var i=0; i<s.length; i+=2) {
    var c = String.fromCharCode(parseInt(s[i]+s[i+1],16));
    result += c;
  }
  return result;
};


paliApp.Controller = function($scope,$http,$transclude,$sce) {
    "use strict";
    $scope.byCode = "";
    $scope.attrs = {};
    $scope.svgImageSnippet = function () {
        var s = $sce.trustAsHtml($scope.htmlresult);
        return s;
        // return $scope.htmlresult;
    };

    $transclude(function (clone, scope) {
        if (PALITESTWITHOUTPLUGINS) return;
        if (!clone[0]) return;
        var markJSON = "xxxJSONxxx";
        var markHex = "xxxHEXJSONxxx";
        var s = clone[0].textContent;
        var chex = s.indexOf(markHex) === 0;
        var cjson = s.indexOf(markJSON) === 0;
        if (!chex && !cjson) {
            $scope.byCode = s;
            return;
        }
        if (cjson) s = s.substring(markJSON.length);
        if (chex) s = csApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
        $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
    });

    $scope.errors = [];
    $scope.result = "";
    $scope.htmlresult = "";
    $scope.resImage = "";
    $scope.imgURL = "";
    $scope.viewCode = false;
    $scope.runSuccess = false;

    $scope.$watch('userinput', function () {
        window.clearInterval($scope.runTimer);
        if ($scope.autoupdate) $scope.runTimer = setInterval($scope.runCodeAuto, $scope.autoupdate);
    }, true);


    $scope.doRunCode = function (runType, nosave) {
        // $scope.viewCode = false;
        window.clearInterval($scope.runTimer);
        // alert("moi");

        $scope.error = "... running ...";
        $scope.runError = true;
        $scope.isRunning = true;

        $scope.runSuccess = false;
        $scope.result = "";
        $scope.runTestGreen = false;
        $scope.runTestRed = false;

        var uword = "";

        if ($scope.userword) uword = $scope.userinput;

        var params = {
            'input': {
                'userword': uword,
                'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id}
            }
        };
        //		  alert($scope.usercode);
        if (nosave) params.input.nosave = true;
        var url = "/pali/answer";
        if ($scope.plugin) {
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }

        $http({method: 'PUT', url: url, data: params, headers: {'Content-Type': 'application/json'}, timeout: 20000}
        ).success(function (data, status, headers, config) {
                $scope.error = data.web.error;
                $scope.runSuccess = true;
                $scope.isRunning = false;
                $scope.result = data.web.console;
            }).error(function (data, status) {
                $scope.isRunning = false;
                $scope.errors.push(status);
                $scope.error = "Ikuinen silmukka tai jokin muu vika?";
                // $scope.error = data;
            });
    };
}
