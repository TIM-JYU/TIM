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
	return  '<div class="csRunDiv no-popup-menu">' +
				  '<p>Here comes header</p>' +
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
				  '<div><label>{{inputstem}} </label><span><input type ="text" class="paliInput" ng-model="userword" ng-trim="false" placeholder="{{inputplaceholder}}" size="{{cols}}"></span>' +
				  ' <span class="unitTestGreen"  ng-show="runTestGreen" >&nbsp;ok&nbsp;</span>' +
				  ' <span class="unitTestRed"  ng-show="!runTestGreen">&nbsp;not&nbsp;</span>' +
                  '</div>'+
				  '<button ng-if="button"  ng-disabled="isRunning" ng-click="runCode();">{{button}}</button>&nbsp&nbsp'+
				  '<a href="" ng-if="muokattu" ng-click="initCode();">{{resetText}}</a>' +
                  '<span class="tries" ng-if="max_tries"> Tries: {{tries}}/{{max_tries}}</span>' +
				  '<pre class="" ng-if="error">{{error}}</pre>'+
				  '<pre  class="" ng-show="result">{{result}}</pre>'+
    		      '<p class="plgfooter">Here comes footer</p>'+
              '</div>';
};


paliApp.get = function(jso,keys) {
"use strict";
    if ( !jso ) return undefined;
    var val = jso;
    for (var k in keys ) {
        if ( val === null ) return undefined;
        if ( !keys[k] ) continue;
        val = val[keys[k]];
        if ( val === undefined ) return undefined
    }
    return val;
}

paliApp.setk = function(scope,sname,attrs,keys,def) {
"use strict";
    scope[sname] = def;
    var val = paliApp.get(attrs,keys);
    if ( val != undefined ) scope[sname] = val;
    val = paliApp.get(scope.attrs,keys);
    if ( val != undefined ) scope[sname] = val;
    if ( scope[sname] == "None" ) scope[sname] = "";
    return scope[sname];
};


paliApp.setn = function(scope,sname,attrs,name,def) {
"use strict";
    if ( name.indexOf(".") < 0 ) name = "markup."+name;
    var keys = name.split(".");
    return paliApp.setk(scope,sname,attrs,keys,def);
};


paliApp.set = function(scope,attrs,name,def) {
"use strict";
    if ( name.indexOf(".") < 0 ) name = "markup."+name;
    var keys = name.split(".");
    var sname = keys[keys.length-1];
    return paliApp.setk(scope,sname,attrs,keys,def);
};


paliApp.directiveFunction = function() {
"use strict";
    return {
		link: function (scope, element, attrs) {
            scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
            scope.plugin = element.parent().attr("data-plugin");
            scope.taskId  = element.parent().attr("id");

            paliApp.set(scope,attrs,"stem");
            paliApp.set(scope,attrs,"inputstem");
            paliApp.set(scope,attrs,"inputplaceholder","Write your input here");
            paliApp.set(scope,attrs,"user_id");
            paliApp.set(scope,attrs,"initword","");
            paliApp.set(scope,attrs,"state.userword",scope.initword);
            paliApp.set(scope,attrs,"button","Save");
            paliApp.set(scope,attrs,"resetText","Reset");
            paliApp.set(scope,attrs,"state.tries",0);
            paliApp.set(scope,attrs,"max_tries");
            paliApp.set(scope,attrs,"cols",20);
            paliApp.set(scope,attrs,"autoupdate",500);
            paliApp.setn(scope,"tid",attrs,".taskID"); // vain kokeilu että "juuresta" ottaminen toimii
            element[0].childNodes[0].outerHTML = paliApp.getHeading(attrs,"header",scope,"h4");
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = paliApp.getHeading(attrs,"footer",scope,'p class="footer"');
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


function isPalindrome(s) {
    var sc = s.toLowerCase();
    sc = sc.replace(/[^a-z,åöä]/g,'');
    for (var i1 = 0, i2 = sc.length-1; i1< i2; i1++,i2--)
        if ( sc[i1] != sc[i2] ) return false;
    return true;
}

paliApp.Controller = function($scope,$http,$transclude,$interval) {
    "use strict";
    $scope.attrs = {};

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
        if (chex) s = paliApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
        $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
    });

    $scope.errors = [];
	$scope.muokattu = false;
    $scope.result = "";

    $scope.$watch('userword', function () {
        //window.clearInterval($scope.runTimer); // perustimerin tapahtumalla ei päivit
        //if ($scope.autoupdate) $scope.runTimer = window.setInterval($scope.runCodeAuto, $scope.autoupdate);
        $interval.cancel($scope.runTimer);
        $scope.runTimer = $interval($scope.runCodeAuto, $scope.autoupdate);
        $scope.muokattu = ( $scope.initword !== $scope.userword );
        //$scope.runCodeAuto();
    }, true);

    
    $scope.runCodeAuto = function() {
        // window.clearInterval($scope.runTimer);
        $interval.cancel($scope.runTimer);
        var pali = isPalindrome($scope.userword);
        $scope.runTestGreen = pali;
	};
	
    
	$scope.initCode = function() {
		$scope.muokattu = false;
		$scope.userword = $scope.initword;
		$scope.error = "";
		$scope.result = "";
	};


    $scope.runCode = function() {
        $scope.doRunCode(false);
    }
    
    $scope.doRunCode = function (nosave) {
        // $scope.viewCode = false;
        window.clearInterval($scope.runTimer);
        // alert("moi");

        $scope.error = "... running ...";
        $scope.isRunning = true;

        $scope.result = "";

        var uword = "";

        if ($scope.userword) uword = $scope.userword;

        var params = {
            'input': {
                'userword': uword,
                'markup': {'taskId': $scope.taskId, 'user_id': $scope.user_id}
            }
        };

        if (nosave) params.input.nosave = true;
        params.input.paliOK = $scope.runTestGreen;
        var url = "/pali/answer";
        if ($scope.plugin) {
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }

        $http({method: 'PUT', url: url, data: params, headers: {'Content-Type': 'application/json'}, timeout: 20000}
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
}
