﻿"use strict";
var csApp = angular.module('csApp', ['ngSanitize']);
csApp.directive('csRunner',['$sanitize', function ($sanitize) {	"use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('console',false); }]);
csApp.directive('csJypeliRunner', ['$sanitize', function ($sanitize) { "use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('jypeli',false); }]);
csApp.directive('csComtestRunner', ['$sanitize', function ($sanitize) { "use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('comtest',false); }]);
csApp.directive('csRunnerInput',['$sanitize', function ($sanitize) { "use strict";	csApp.sanitize = $sanitize; return csApp.directiveFunction('console',true); }]);
csApp.directive('csJypeliRunnerInput', ['$sanitize', function ($sanitize) { "use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('jypeli',true); }]);
csApp.directive('csComtestRunnerInput', ['$sanitize', function ($sanitize) { "use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('comtest',true); }]);
csApp.directive('csTaunoRunner', ['$sanitize', function ($sanitize) { "use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('tauno',false); }]);
csApp.directive('csTaunoRunnerInput', ['$sanitize', function ($sanitize) {"use strict"; csApp.sanitize = $sanitize; return csApp.directiveFunction('tauno',true); }]);
// csApp.directive('csRunner',function() {	csApp.sanitize = $sanitize; return csApp.directiveFunction('console'); }); // jos ei tarviiis sanitize

var TESTWITHOUTPLUGINS = true && false;
csApp.taunoNr = 0;

// =================================================================================================================
// Things for konwn langueges

var languageTypes = {};
// What are known language types (be carefull not to include partial word):
languageTypes.runTypes     = ["jypeli","java","graphics","cc","c++","shell","py","fs","clisp","jjs","sql","alloy","text","cs","run"];

// What are known test types (be carefull not to include partial word):
languageTypes.testTypes = ["ccomtest","jcomtest","comtest","junit"];

// If test type is comtest, how to change it for specific languages
languageTypes.impTestTypes = {console:"comtest", cc:"ccomtest", java:"jcomtest"};
languageTypes.impTestTypes["c++"] = "ccomtest";

languageTypes.whatIsIn = function (types, type, def) {
"use strict";
    if (!type) return def;
    type = type.toLowerCase();
    for (var i=0; i< types.length; i++)
        if ( type.indexOf(types[i]) >= 0 ) return types[i];
    return def;
};

languageTypes.getRunType = function(type,def) {
"use strict";
    return this.whatIsIn(this.runTypes,type,def);
};

languageTypes.getTestType = function(type,def) {
"use strict";
    var t = this.whatIsIn(this.testTypes,type,def);
    if ( t !== "comtest" ) return t;
    var lt = this.whatIsIn(this.runTypes,type,"console");
    var impt = this.impTestTypes[lt];
    if ( impt ) return impt;
    return t;     
};
// =================================================================================================================

var removeXML = function(s) {
"use strict";
    s = s.replace(/^<\?xml [^>]*\?>/,"");
    s = s.replace(/(<svg [^>]*height="[0-9]+)pt/,"$1");
    s = s.replace(/(<svg [^>]*width="[0-9]+)pt/,"$1");
    return s;
};



csApp.directive('contenteditable', ['$sce', function($sce) {
"use strict";
    return {
      restrict: 'A', // only activate on element attribute
      require: '?ngModel', // get a hold of NgModelController
      link: function(scope, element, attrs, ngModel) {
        if(!ngModel) return; // do nothing if no ng-model

        // Specify how UI should be updated
        ngModel.$render = function() {
          // element.html($sce.getTrustedHtml(ngModel.$viewValue || ''));
          element.text(ngModel.$viewValue || '');
        };

        // Listen for change events to enable binding
        element.on('blur keyup change', function() {
          scope.$apply(read);
        });
        read(); // initialize

        // Write data to the model
        function read() {
          // var html = element.html();
          var text = element.html();
		  // if ( text.indexOf("<div>") >= 0 )  { text = text.replace("<div>","").replace("</div>","\n"); element.html(text); }
          text = element.text();
          // When we clear the content editable the browser leaves a <br> behind
          // If strip-br attribute is provided then we strip this out
          // if( attrs.stripBr && html == '<br>' ) {
          //   html = '';
          // }
          ngModel.$setViewValue(text);
        }
      }
    };
  }]);


csApp.commentTrim = function(s) {
"use strict";
	if ( !s ) return "";
	var n = s.indexOf("//\n");
    if (n !== 0) return s;
	return s.substr(3); 
};

csApp.getHeading = function(a,key,$scope,defElem) {
"use strict";
	var h = csApp.set($scope,a,key,"");
	if ( !h ) return "";
	// if ( h.toLowerCase().indexOf("script") >= 0 ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = defElem;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	var ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	// var ea = elem.split(" ",2);
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	// if ( elem.toLowerCase().indexOf("script") >= 0 ) return "";
	// attributes = "";  // ei laiteta näitä, niin on vähän turvallisempi
	try {
	  // val = decodeURIComponent(escape(val));
	  val = decodeURIComponent(encodeURI(val));
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = csApp.sanitize(html);
	return html;
};


csApp.directiveTemplateCS = function(t,isInput) {
"use strict";
	csApp.taunoPHIndex = 3;
    if ( TESTWITHOUTPLUGINS ) return '';
	return  '<div class="csRunDiv">' + 
    
				  '<p>Here comes header</p>' +
				//  '<p ng-bind-html="getHeader()"></p>
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
  				  (t === "tauno" ?
				    '<p ng-if="taunoOn" class="pluginHide""><a ng-click="hideTauno()">hide Tauno</a></p>' +
				    '<div ><p></p></div>' + // Tauno code place holder nr 3!!
				    '<p ng-if="!taunoOn" class="pluginShow" ><a ng-click="showTauno()">Click here to show Tauno</a></p>' +
				    '<p ng-if="taunoOn" class="pluginHide"" ><a ng-click="copyTauno()">copy from Tauno</a> | <a ng-click="hideTauno()">hide Tauno</a></p>' +
				    '<p ng-if="taunoOn" class="taunoOhje">Kopioi Taunolla tekemäsi koodi "copy from Tauno"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa että ajossa voi olla eri taulukko kuin Taunossa!</a></p>' +
					"" : "") +   
				  '<pre ng-if="viewCode && codeover">{{code}}</pre>'+
				  '<div class="csRunCode">'+'<p></p>'+
				  '<pre class="csRunPre" ng-if="viewCode &&!codeunder &&!codeover">{{precode}}</pre>'+
                  '<div>'+
				  // '<textarea class="csRunArea" ng-if="!noeditor" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea>'+
				  '<textarea class="csRunArea" ng-hide="noeditor" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea>'+
				  '<div class="csRunChanged" ng-if="usercode!=byCode"></div>'+
				  //'<div class="csRunChanged" ng-if="muokattu"></div>'+
                  '</div>'+
				  //'<div class="csRunArea" contentEditable ng-model="usercode" ng-trim="false" "></div>'+
				  '<pre class="csRunPost" ng-if="viewCode &&!codeunder &&!codeover">{{postcode}}</pre>'+
				  '</div>'+
				  //'<br />'+ 
                  
                  (isInput  ?
                  '<div class="csInputDiv" ng-hide="!showInput">' +
                  '<p ng-if="inputstem" class="stem" >{{inputstem}}</p>' +
                  '<div class="csRunCode" >' +
				  '<textarea class="csRunArea csInputArea"  rows={{inputrows}} ng-model="userinput" ng-trim="false" placeholder="{{inputplaceholder}}"></textarea>'+
                  '</div>' + 
                  '</div>' + 
                  '<div class="csArgsDiv" ng-hide="!showArgs">' +
				  '<label>{{argsstem}}</label><span><input type ="text" class="csArgsArea" ng-model="userargs" ng-trim="false" placeholder="{{argsplaceholder}}"></span>'+
                  '</div>' : "") + // end of isInput
                  
				  '<p class="csRunSnippets" ng-if="buttons && viewCode">' +
				  '<button ng-repeat="item in buttons" ng-click="addText(item);">{{addTextHtml(item)}}</button>&nbsp&nbsp' +
                  '</p>' +
                  
				  '<p class="csRunMenu" >' +
				  '<button ng-if="isRun"  ng-click="runCode();">{{buttonText}}</button>&nbsp&nbsp'+
				  '<button ng-if="isTest" ng-click="runTest();">Test</button>&nbsp&nbsp'+
				  '<a href="" ng-if="!attrs.nocode && (file || attrs.program)" ng-click="showCode();">{{showCodeLink}}</a>&nbsp&nbsp'+
				  '<a href="" ng-if="muokattu" ng-click="initCode();">Alusta</a>' +
				  '<a href="" ng-if="toggleEditor" ng-click="hideShowEditor();">Editor</a>' +
                  '</p>'+
				  '<pre ng-if="viewCode && codeunder">{{code}}</pre>'+
				  (t === "comtest" || t === "tauno" ? '<p class="unitTestGreen"  ng-if="runTestGreen" >&nbsp;ok</p>' : "") +
				  (t === "comtest" || t === "tauno"? '<pre class="unitTestRed"    ng-if="runTestRed">{{comtestError}}</pre>' : "") +
				  // '<p>{{resImage}}</p>'+
				  // '<p>Testi valituksesta</p>' +
				  '<pre class="csRunError" ng-if="runError">{{error}}</pre>'+
				  '<pre  class="console" ng-if="result">{{result}}</pre>'+
				  '<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="svgImageSnippet()"></span></div>'+
                  
                  //'<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="htmlresult"></span></div>'+
				  //'<div  class="htmlresult" ng-if="htmlresult" >{{htmlresult}}</span></div>'+
                  
				  (t === "jypeli" || true ? '<img  ng-if="imgURL" class="grconsole" ng-src="{{imgURL}}" alt=""  />' : "") +
				  //(t == "jypeli" ? '<img  class="grconsole" ng-src="{{imgURL}}" alt=""  ng-if="runSuccess"  />' : "") +
				  '<p class="footer">Here comes footer</p>'+
                 
				  '</div>';
};


csApp.set = function(scope,attrs,name,def) {
"use strict";
    scope[name] = def;
    if ( attrs && attrs[name] ) scope[name] = attrs[name];
    if ( scope.attrs && scope.attrs[name] ) scope[name] = scope.attrs[name];
    return scope[name];
};


csApp.directiveFunction = function(t,isInput) {
"use strict";
	return {
		link: function (scope, element, attrs) {
            if ( TESTWITHOUTPLUGINS ) return;

            scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
			scope.taunoHtml = element[0].childNodes[csApp.taunoPHIndex]; // Check this carefully, where is Tauno placeholder
            scope.plugin = element.parent().attr("data-plugin");
            scope.taskId  = element.parent().attr("id");

			csApp.set(scope,attrs,"file");
			csApp.set(scope,attrs,"lang");
			csApp.set(scope,attrs,"type","cs");
			csApp.set(scope,attrs,"width");
			csApp.set(scope,attrs,"height");
			csApp.set(scope,attrs,"table");
			csApp.set(scope,attrs,"variables");
			csApp.set(scope,attrs,"indices");
			csApp.set(scope,attrs,"replace");
			csApp.set(scope,attrs,"taunotype");
			csApp.set(scope,attrs,"stem");
			csApp.set(scope,attrs,"iframe",false);
			csApp.set(scope,attrs,"usercode","");
			csApp.set(scope,attrs,"codeunder",false);
			csApp.set(scope,attrs,"codeover",false);
			csApp.set(scope,attrs,"open",false);
			csApp.set(scope,attrs,"rows",1);
			csApp.set(scope,attrs,"maxrows",-1);
			csApp.set(scope,attrs,"attrs.bycode");
			csApp.set(scope,attrs,"placeholder","Write your code here");
			csApp.set(scope,attrs,"inputplaceholder","Write your input here");
			csApp.set(scope,attrs,"argsplaceholder","Write your program args here");
			csApp.set(scope,attrs,"argsstem","Args:");
			csApp.set(scope,attrs,"userinput","");
			csApp.set(scope,attrs,"userargs","");
			csApp.set(scope,attrs,"inputstem","");
			csApp.set(scope,attrs,"inputrows",1);
			csApp.set(scope,attrs,"toggleEditor",false);
            csApp.set(scope,attrs,"indent",-1);
            csApp.set(scope,attrs,"user_id");
            csApp.set(scope,attrs,"isHtml",false);
            csApp.set(scope,attrs,"autoupdate",false);
            csApp.set(scope,attrs,"button","");
            csApp.set(scope,attrs,"noeditor",false);
            csApp.set(scope,attrs,"showCodeOn","Näytä koko koodi");
            csApp.set(scope,attrs,"showCodeOff","Piilota muu koodi");
            // csApp.set(scope,attrs,"program");

            scope.showCodeLink = scope.showCodeOn;
			scope.minRows = csApp.getInt(scope.rows);
			scope.maxRows = csApp.getInt(scope.maxrows);
			if ( scope.usercode === "" && scope.byCode )  scope.usercode = scope.byCode;
			scope.usercode = csApp.commentTrim(scope.usercode);
			scope.byCode = csApp.commentTrim(scope.byCode);
			// scope.usercode = csApp.commentTrim(decodeURIComponent(escape(scope.usercode)));
			// scope.byCode = csApp.commentTrim(decodeURIComponent(escape(scope.byCode)));

            if ( scope.usercode ) {
                var rowCount = csApp.countChars(scope.usercode,'\n') + 1;
                if ( scope.maxRows < 0 && scope.maxRows < rowCount ) scope.maxRows = rowCount; 
            } else if ( scope.maxRows < 0 ) scope.maxRows = 10;
            
            scope.isRun = languageTypes.getRunType(scope.type,false) !== false;
            scope.isTest = languageTypes.getTestType(scope.type,false) !== false;
            
            scope.showInput = (scope.type.indexOf("input") >= 0);
            scope.showArgs = (scope.type.indexOf("args") >= 0);
            scope.buttonText = "Aja";
            if ( scope.type.indexOf("text") >= 0 ) {
                scope.isRun = true;
                scope.buttonText = "Lähetä";
            }            
            if ( scope.button ) scope.buttonText = scope.button;
            
			scope.indent = csApp.getInt(scope.indent);
            if ( scope.indent < 0 )
                if ( scope.file ) scope.indent = 8; else scope.indent = 0;

            
			scope.edit = element.find("textarea")[0]; // angular.element(e); // $("#"+scope.editid);
			element[0].childNodes[0].outerHTML = csApp.getHeading(attrs,"header",scope,"h4");
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = csApp.getHeading(attrs,"footer",scope,'p class="footer"');
            scope.muokattu = false; // scope.usercode != scope.byCode;
        //    scope.header = head;
		//	scope.getHeader = function() { return head; };
		//	csApp.updateEditSize(scope);
            if (scope.open) scope.showTauno();

            //attrs.buttons = "$hellobuttons$\nMunOhjelma\n$typebuttons$\n$charbuttons$";
            var b = attrs.buttons || scope.attrs.buttons;
            if ( b ) {
                var helloButtons = 'public \nclass \nHello \n\\n\n{\n}\n'+
                                    'static \nvoid \n Main\n(\n)\n' + 
                                    '        Console.WriteLine(\n"\nworld!\n;\n ';
                var typeButtons =  'bool \nchar\n int \ndouble \nstring \nStringBuilder \nPhyscisObject \n[] \nreturn \n, ';
                var charButtons = 'a\nb\nc\nd\ne\ni\nj\n.\n0\n1\n2\n3\n4\n5\nfalse\ntrue\nnull\n=';
                // var b = attrs.buttons;
                b = b.replace('$hellobuttons$', helloButtons);
                b = b.replace('$typebuttons$' , typeButtons);
                b = b.replace('$charbuttons$' , charButtons);
                scope.buttons = b.split("\n");
            }
            if ( scope.attrs.autorun ) scope.runCode();
		},		
		scope: {},				 
		controller: csApp.Controller,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: csApp.directiveTemplateCS(t,isInput)
		// templateUrl: 'csTempl.html'
	}; 
};


csApp.getInt = function(s) {
"use strict";
   var n = parseInt(s);
   if ( isNaN(n) ) return 0;
   return n;
};


csApp.countChars = function (s,c) {
"use strict";
	var n = 0;
	for (var i=0; i<s.length; n += +(c===s[i++]));
	return n;
};


csApp.updateEditSize = function(scope) {
"use strict";
//return;
    if ( !scope ) return;
	if ( !scope.usercode ) return;
    var n = csApp.countChars(scope.usercode,'\n') + 1;
	if ( n < scope.minRows ) n = scope.minRows;
	if ( n > scope.maxRows ) n = scope.maxRows;
	if ( n < 1 ) n = 1;
	scope.rows = n;
};

csApp.ifIs = function(value,name,def) {
"use strict";
	if ( !value && !def ) return "";
	if ( !value  ) return name+'="'+def+'" ';
	return name+'="'+value+'" ';
};
		
csApp.doVariables = function(v,name) {
"use strict";
	if ( !v ) return "";
	var r = "";
	var va = v.split(";");
	for ( var n in va ) {
        if ( va.hasOwnProperty(n)) {
            var nv = va[n].trim();
            if (nv) {
                r += name + nv + "&";
            }
        }
	}
	return r.replace(/ /g,"");
};


csApp.Hex2Str = function (s) {
"use strict";
  var result = '';
  for (var i=0; i<s.length; i+=2) {
    var c = String.fromCharCode(parseInt(s[i]+s[i+1],16));
    result += c;
  }
  return result;
};


csApp.Controller = function($scope,$http,$transclude,$sce) {
"use strict";
	$scope.byCode ="";
	$scope.attrs = {};
    $scope.svgImageSnippet = function() {
        var s = $sce.trustAsHtml($scope.htmlresult); 
        return s;
        // return $scope.htmlresult;
    };

	$transclude(function(clone, scope) {
        if ( TESTWITHOUTPLUGINS ) return;
		if ( !clone[0] ) return;
		var markJSON = "xxxJSONxxx";
		var markHex = "xxxHEXJSONxxx";
		var s = clone[0].textContent;
		var chex = s.indexOf(markHex) === 0;
		var cjson = s.indexOf(markJSON) === 0;
		if ( !chex && !cjson ) {
		    $scope.byCode = s;
		    return;
        }
        if ( cjson ) s = s.substring(markJSON.length);
        if ( chex ) s = csApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
	    $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
	});
	$scope.errors = [];
	$scope.taunoOn = false;
	// $scope.replace = "INSERT YOUR CODE HERE";
	// $scope.file = "https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs";
	$scope.result = "";
	$scope.htmlresult = "";
	$scope.resImage = "";
	$scope.imgURL = "";
	$scope.viewCode = false;
	$scope.runSuccess = false;
	$scope.copyingFromTauno = false;

	$scope.$watch('[usercode]', function() {
		if ( !$scope.copyingFromTauno && $scope.usercode !== $scope.byCode ) $scope.muokattu = true;
		$scope.copyingFromTauno = false;
		if ( $scope.minRows < $scope.maxRows ) 
			csApp.updateEditSize($scope);
		if ( $scope.viewCode ) $scope.pushShowCodeNow();
        window.clearInterval($scope.runTimer);
        if ( $scope.autoupdate ) $scope.runTimer = setInterval($scope.runCode,$scope.autoupdate);

		/* // tällä koodilla on se vika, että ei voi pyyhkiä alusta välilyönteä ja yhdistää rivejä
		if ( $scope.carretPos && $scope.carretPos >= 0 ) {
		    $scope.edit.selectionStart = $scope.carretPos;
		    $scope.edit.selectionEnd = $scope.carretPos;
		    $scope.carretPos = -1;
		}
		else $scope.checkIndent(); // ongelmia saada kursori paikalleen
		*/
	}, true);
	
	$scope.runCode = function() {
		var t = languageTypes.getRunType($scope.type,"cs"); 
		$scope.doRunCode(t);
	};
	
	$scope.runTest = function() {
		var t = languageTypes.getTestType($scope.type,"comtest"); 
		$scope.doRunCode(t);
	};
	
    
    $scope.hideShowEditor = function() {
        $scope.noeditor = !$scope.noeditor;
    };
    
	$scope.doRunCode = function(runType) {
		// $scope.viewCode = false;
        window.clearInterval($scope.runTimer);
		if ( $scope.taunoOn && ( !$scope.muokattu || !$scope.usercode ) ) $scope.copyTauno();
		$scope.checkIndent();
		if ( !$scope.autoupdate ) {
            $scope.error = "... running ...";
            $scope.runError = true;
        }
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.result = "";
		$scope.runTestGreen = false;
		$scope.runTestRed = false;
        var isInput = false;
        if ( $scope.type.indexOf("input") >= 0 ) isInput = true;

        var ucode = "";
        var uinput = "";
        var uargs = "";
        if ( $scope.usercode ) ucode = $scope.usercode.replace($scope.cursor,"");
        if ( $scope.userinput ) uinput = $scope.userinput;
        if ( $scope.userargs ) uargs = $scope.userargs;
		var t = runType;
		// if ( t == "tauno" ) t = "comtest";

		// params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		var params = {
                  //   'input': 1
				  'input': {'usercode':ucode, 'userinput':uinput, 'isInput' : isInput, 'userargs': uargs,
                            // 'markup': {'type':t, 'file': $scope.file, 'replace': $scope.replace, 'lang': $scope.lang, 'taskId': $scope.taskId, 'user_id': $scope.user_id}, 
				      'markup': { 'type': t, 'taskId': $scope.taskId, 'user_id': $scope.user_id }
				  }
                  };
		//		  alert($scope.usercode);
        var url = "/cs/answer";
        // url = "http://tim-beta.it.jyu.fi/cs/answer";
        if ( $scope.plugin ) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if ( i > 0 ) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
		$http({method: 'PUT', url: url, data:params, headers: {'Content-Type': 'application/json'}}
		).success(function(data, status, headers, config) {
			if ( data.web.error && false ) {
				$scope.error = data.web.error;
				//return;
			}
			$scope.error = data.web.error;
			var imgURL = "";
			$scope.runSuccess = true;
			$scope.runError = $scope.error; // !$scope.runSuccess;

			imgURL = data.web.image;
			if ( data.web.testGreen ) $scope.runTestGreen = true;
			if ( data.web.testRed ) $scope.runTestRed = true;
			$scope.comtestError = data.web.comtestError;
			if ( imgURL ) {
				// $scope.resImage = '<img src="' + imgURL + ' " alt="Result image" />';
				$scope.imgURL = imgURL;
				$scope.result = data.web.console;
			} else {
				if ( $scope.runSuccess )  
                    if ( $scope.isHtml )
                        $scope.htmlresult = removeXML(data.web.console);
                    else
                        $scope.result = data.web.console;
				else   
				   $scope.error = data.replace("*** Success!\n","");
			}

		}).error(function(data, status) {
			$scope.errors.push(status);
			// $scope.error = data;
            $scope.error = "Ikuinen silmukka?";
		});
	};
	
	$scope.hideTauno = function() {
		$scope.taunoOn = false;
		$scope.taunoHtml.innerHTML = "<p></p>";
	};

	
	$scope.copyTauno = function() {
		var f = document.getElementById($scope.taunoId);
		// var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
		var s = f.contentWindow.getUserCodeFromTauno();
		$scope.copyingFromTauno = true;
        var treplace = $scope.attrs.treplace || "";
        if ( treplace ) {
            var treps = treplace.split("&");
            for (var i = 0; i < treps.length; i++) {
                var reps = (treps[i]+"|").split("|");
                s = s.replace(new RegExp(reps[0],'g'),reps[1]);
                s = s.replace(new RegExp("\n\n",'g'),"\n");
            }
        }
		$scope.usercode = s;
        $scope.checkIndent();
		$scope.muokattu = false;
	};
	
	
	$scope.addText = function (s) {
	    // $scope.usercode += s;
	    var tbox = $scope.edit;
	    var i = tbox.selectionStart || 0;
	    var uc = $scope.usercode || "";
	    // $scope.usercode = uc.substring(0, i) + s.replace(/\\n/g,"\n") + uc.substring(i);
	    $scope.usercode = (uc + s.replace(/\\n/g,"\n")).replace($scope.cursor,"")+$scope.cursor;
	    // $scope.insertAtCursor(tbox, s);
	    //tbox.selectionStart += s.length;
	    //tbox.selectionEnd += s.length;
	};

	$scope.addTextHtml = function (s) {
	    var ret = s.trim();
	    if (ret.length === 0) ret = "\u00A0";
	    return ret;
	};


	$scope.insertAtCursor = function(myField, myValue) {
	    //IE support
	    if (document.selection) {
	        myField.focus();
	        var sel = document.selection.createRange();
	        sel.text = myValue;
	    }
	        //MOZILLA and others
	    else if (myField.selectionStart || myField.selectionStart === 0) {
	        var startPos = myField.selectionStart;
	        var endPos = myField.selectionEnd;
	        myField.value = myField.value.substring(0, startPos) +
                myValue +
                myField.value.substring(endPos, myField.value.length);
	    } else {
	        myField.value += myValue;
	    }
	};

	$scope.showTauno = function () {
	
		csApp.taunoNr++;
		var vid = 'tauno'+csApp.taunoNr;
		$scope.taunoId = vid;
		var w = csApp.ifIs($scope.width,"width",800);
		var h = csApp.ifIs($scope.height,"height",500);
		var p = "";
		// var tt = "http://users.jyu.fi/~ji/js/tdbg/";
		// var tt = "http://tim-beta.it.jyu.fi/cs/ptauno/indexi.html";
		var tt = "/cs/tauno/index.html?";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://users.jyu.fi/~vesal/js/ptauno/index.html";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://tim-beta.it.jyu.fi/cs/ptauno/index.html";
		if ( $scope.taunotype && $scope.taunotype === "ptauno" ) tt = "/cs/tauno/index.html?s&";
		var taunoUrl = tt; // +"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
		var s = $scope.table;
		if ( s && s.length > 0) {
			if ( s[0] === 's' ) p = "ts="+s.substring(1) + "&"; // table it's size param "table: s10"
			else p = "t="+s.trim() + "&";                      // table by it's items
		}
		 
		p += csApp.doVariables($scope.variables,"m");
		p += csApp.doVariables($scope.indices,"i");
		
		taunoUrl = taunoUrl + p;
		$scope.iframe = true;
		if ( $scope.iframe )
			$scope.taunoHtml.innerHTML = 
			// '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
			'<iframe id="'+vid+'" class="showTauno" src="' + taunoUrl + '" ' + w + h + ' ></iframe>';
			// youtube: <iframe width="480" height="385" src="//www.youtube.com/embed/RwmU0O7hXts" frameborder="0" allowfullscreen></iframe>
		else   
			$scope.taunoHtml.innerHTML = '<div class="taunoNaytto" id="'+vid+'" />';
		$scope.taunoOn = true;	
	};
	
	
	$scope.initCode = function() {
		$scope.muokattu = false;
		$scope.usercode = $scope.byCode;
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.runError = false;
		$scope.result = "";
		$scope.viewCode = false;

	};

	
	$scope.stopShow = function() {
		if (angular.isDefined($scope.stop)) {
			$interval.cancel($scope.stop);
			$scope.stop = undefined;
		}
	};
	
	$scope.pushShowCodeNow = function() {
		if ( !$scope.viewCode ) return;
		$scope.showCodeNow();
        return;
        /*
		$scope.stopShow();
		$scope.stop = $interval(function() {
			$scope.showCodeNow();
		}, 1000,1);
		*/
	};
	
	
	$scope.showCode = function() {
		$scope.viewCode = !$scope.viewCode;
        
        if ( $scope.viewCode ) 
            $scope.showCodeLink = $scope.showCodeOff;
        else
            $scope.showCodeLink = $scope.showCodeOn;

		$scope.localcode = undefined;
		$scope.showCodeNow();
	};
		
        
    $scope.checkIndent = function() {
        if ( !$scope.indent || !$scope.usercode ) return;
        var start = $scope.edit.selectionStart;
        var spaces = "";
        for (var j1=0; j1<$scope.indent; j1++) spaces += " ";
        var n = 0;
        var len = 0;
		var st = $scope.usercode.split("\n");
        for (var i in st) if ( st.hasOwnProperty(i) ) {
			var s = st[i];
			var l = s.length;
			len += l;
			var j = 0;
			for ( ; j<s.length; j++) if ( s[j] !== " " ) break;
			// if ( s.lastIndexOf(spaces,0) == 0 ) continue;
			if ( j >= spaces.length ) continue;
            if ( s.trim() === "" ) continue; // do not indent empty lines
			s = spaces + s.substring(j);
			var dl = s.length - l;
			if ( len - l < start ) start += dl;
			len += dl;
			st[i] = s;
			n++;
        }
        if ( !n ) return;
        $scope.usercode = st.join("\n");
        // $scope.edit[0].selectionStart = start; // aiheuttaa uuden tapahtuman
        $scope.carretPos = start; // seuraava tapahtuma nappaa tämän ja siirtää vain kursorin.
    };
		
	$scope.showCodeLocal = function() {
		// $scope.code = $scope.localcode;
		if ( $scope.localcode === "" ) { $scope.code = $scope.usercode; return; }
		var st = $scope.localcode.split("\n");
		var r = "";
		var rp = ["",""]; // alkuosa, loppuosa
		var step = 0;
		var nl = "";
		var nls = "";
		for (var i in st) if ( st.hasOwnProperty(i) ) {
			var s = st[i];
			if ( s.indexOf($scope.replace) >= 0 ) {
				r += nl + $scope.usercode;
				if ( step === 0 ) { step++; nls = ""; continue; }
			} else { 
				r += nl + s;
				rp[step] += nls + s;
			}
			nl = nls = "\n";
		}
		$scope.code = r;
		$scope.precode = rp[0];
		$scope.postcode = rp[1];
	};
		
		
	$scope.showCodeNow = function() {
	    // var copyEvent = new ClipboardEvent('copy', { dataType: 'text/plain', data: 'kissa' } );
        // document.dispatchEvent(copyEvent);
		if ( !$scope.viewCode ) return;
		if ( angular.isDefined($scope.localcode) ) { $scope.showCodeLocal(); return; } 
		if ( !$scope.file &&!$scope.attrs.program ) { $scope.localcode = ""; $scope.showCodeLocal(); return; }
		
		// params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&keplace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		var params = $scope.attrs;
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		$http({method: 'POST', url: '/cs/?print=1&replace=', data:params, headers: {'Content-Type': 'application/json'}}
		// $http({method: 'POST', url:"/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		).success(function(data, status, headers, config) {
			if (data.msg !== '')
			{
				$scope.localcode = data;
				$scope.showCodeLocal();
			}
			else
			{
				$scope.errors.push(data.error);
			}
		}).error(function(data, status) {
			$scope.errors.push(status);
		});
	};
};


var csConsoleApp = angular.module('csConsoleApp', ['ngSanitize']);
// csApp.directive('csRunner', ['$sanitize', function ($sanitize) { csApp.sanitize = $sanitize; return csApp.directiveFunction('console', false); }]);
csConsoleApp.directive('csConsole', ['$sanitize', '$timeout', function ($sanitize, $timeout) { csConsoleApp.sanitize = $sanitize; return csConsoleApp.directiveFunction('shell', true, $timeout); }]);

csConsoleApp.directiveFunction = function (t, isInput, $timeout) {
"use strict";

// csConsoleApp.directive('csConsole', function ($timeout) {
     return {
         restrict: 'E',
         controller: csConsoleApp.Controller,
         scope: {},
         template: csConsoleApp.directiveTemplateCS(t, isInput),
         // templateUrl: function (elem, attrs) {
         //    return "http://" + elem.parent().attr('data-plugin') + "/" + 'NewConsole/Console.template.html';
         // },
         transclude: true,
         replace: true
     };
};


// controller: function($scope,$http,$transclude,$element, $sce ) {
csConsoleApp.Controller = function ($scope, $http, $transclude, $element, $timeout) {
"use strict";
    $scope.attrs = {};

    $transclude(function(clone, scope) {
        if ( TESTWITHOUTPLUGINS ) return;
        if ( !clone[0] ) return;
        var markJSON = "xxxJSONxxx";
        var markHex = "xxxHEXJSONxxx";
        var s = clone[0].textContent;
        var chex = s.indexOf(markHex) === 0;
        var cjson = s.indexOf(markJSON) === 0;
        if ( !chex && !cjson ) {
            $scope.byCode = s;
            return;
        }
        if ( cjson ) s = s.substring(markJSON.length);
        if ( chex ) s = csApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
        $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
    });
    //controller: function ($scope, $element, $sce, $http) {
    // This block could be re-used
    $scope.taskId = $element.parent().attr("id");
    $scope.plugin = $element.parent().attr("data-plugin");
    var reqPath = $scope.plugin + "/" + $scope.ident + "/";
    // $scope.content = JSON.parse($element.attr("data-content"));
    $scope.content = $scope.attrs;
    // End of generally re-usable TIM stuff

    $scope.examples = "";

    if ( $scope.content.examples) {
        var s = $scope.content.examples.replace(/'/g, '"');
        $scope.examples = JSON.parse(s);
    }


    $scope.history = [];

    $scope.loadExample = function (i) {
        $scope.currentInput = $scope.examples[i].expr;
        $scope.focusOnInput();
    };
    $scope.focusOnInput = function () {
        var el = $element[0].querySelector(".console-input");
        el.focus();

    };
    $scope.handler = function (e) {
        var url = "/cs/answer";
        if ($scope.plugin) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
        var t = languageTypes.getRunType($scope.content.type, "shell");
        var ucode = $scope.currentInput;
        var isInput = false;
        var uargs = "";
        var uinput = "";

        $http({
                method: 'PUT',
                url: url,
                data: {
                  'input': {
                      'usercode': ucode, 'userinput': uinput, 'isInput': isInput, 'userargs': uargs,
                      // "input": $scope.currentInput,
                      'markup': { 'type': t, 'taskId': $scope.taskId, 'user_id': $scope.content.user_id }
                  }
              }
        })
         .success(function (data) {
             var s = "";
             if (data.web.error ) {
                 s = data.web.error;
                 s = "<pre>" + s + "</pre>";
             } else {
                 s = data.web.console
                 if (!$scope.isHtml) s = "<pre>" + s + "</pre>";
             }
             $scope.submit(s);
             // console.log(["data", data.web]);
         })
         .error(function (data, status, hdrs, cfg) {
             console.log(["protocol error", data, status, hdrs, cfg]);
             $scope.submit("Endless loop?");
         });

    };
    $scope.currentSize = "normal";
    $scope.currentInput = "";
    $scope.cursor = $scope.history.length; // $scope.history.length means new input is last command.

    $scope.toggleSize = function () {
        if ($scope.currentSize === "normal")
        { $scope.currentSize = "enlarged"; }
        else { $scope.currentSize = "normal"; }
    };

    $scope.submit = function (result) {

        $scope.history.push({
            input: $scope.currentInput,
            response: result
        });
        $scope.currentInput = ""; $scope.cursor = $scope.history.length;
        $timeout(function () {
            var el = $element[0].querySelector(".console-output");
            el.scrollTop = el.scrollHeight;
        });
    };

    $scope.load = function () {
        if ($scope.cursor >= $scope.history.length)
        { $scope.currentInput = ""; $scope.cursor = $scope.history.length; return; }
        var norm = Math.min($scope.history.length - 1, Math.max(0, $scope.cursor));
        $scope.currentInput = $scope.history[norm].input;
        $scope.cursor = norm;
    };


    $scope.handleKey = function (ev) {
        if (ev.which === 13) $scope.handler();// submit();
        if (ev.which === 40) { $scope.cursor++; $scope.load(); }
        if (ev.which === 38) { $scope.cursor--; $scope.load(); }
    };
};
 

csConsoleApp.directiveTemplateCS = function (t, isInput) {
"use strict";
    if (TESTWITHOUTPLUGINS) return '';
    return '<div class="web-console {{currentSize}}"'+
     '    ng-keydown="handleKey($event)" >'+
     ''+
     '<code class="console-output">'+
     ' <div class="console-output-elem" '+
     '    ng-repeat="item in history track by $index">'+
     '<span class="console-oldinput">'+
     '  <span class="console-in">in_{{$index}}: </span>'+
     '  <span class="console-userInput">{{item.input}}</span>'+
     ' </span>'+
     ' <br/>'+
     ' <span class="console-oldresponse">'+
     '  <span class="console-out">out_{{$index}}: </span>'+
     '  <span class="console-response" ng-class="{error:item.error}"><span ng-bind-html="item.response"></span></span> <!-- Double span since ng-bind eats the innermost one --!>'+
     ' </span>'+
     '</div>'+
     '<span class="console-expander-sym"'+
     '    ng-click="toggleSize()"></span>'+
     '</code>'+
     '<div class="console-examples-box">'+
     '    <span class="examples-title"'+
     '    ng-click="examplesVisible=!examplesVisible" >'+
     '    ▼ example expressions ▲</span>' +
     '<div>Click to load:</div>'+
     '<ul >'+
     '    <li ng-repeat="example in examples track by $index">'+
     '    <a ng-click="loadExample($index)" title="{{example.expr}}">{{example.title||example.expr}}</a>' +
     '    </li>'+
     '<ul>'+
     '</div>'+
     ''+
     '<span class="console-curIndex">in_{{cursor}}</span>'+
     '<input type="text" '+
     '    placeholder="type expressions here"'+
     '        class="console-input"'+
     '        ng-model="currentInput" />'+
     '</div>';
};
