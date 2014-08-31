var csApp = angular.module('csApp', ['ngSanitize']);
csApp.directive('csRunner',['$sanitize', function ($sanitize) {	csApp.sanitize = $sanitize; return csApp.directiveFunction('console'); }]);
csApp.directive('csJypeliRunner', ['$sanitize', function ($sanitize) { csApp.sanitize = $sanitize; return csApp.directiveFunction('jypeli'); }]);
csApp.directive('csComtestRunner', ['$sanitize', function ($sanitize) { csApp.sanitize = $sanitize; return csApp.directiveFunction('comtest'); }]);
csApp.directive('csTaunoRunner', ['$sanitize', function ($sanitize) { csApp.sanitize = $sanitize; return csApp.directiveFunction('tauno'); }]);
// csApp.directive('csRunner',function() {	csApp.sanitize = $sanitize; return csApp.directiveFunction('console'); }); // jos ei tarviiis sanitize

csApp.taunoNr = 0;

csApp.directive('contenteditable', ['$sce', function($sce) {
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
	if ( !s ) return;
	n = s.indexOf("//\n");
	if ( n < 0 ) return s;
	return s.substr(3); 
}

csApp.getHeading = function(a,key,$scope,defElem) {
	var h = csApp.set($scope,a,key,"");
	if ( !h ) return "";
	// if ( h.toLowerCase().indexOf("script") >= 0 ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = defElem;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	// var ea = elem.split(" ",2);
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	// if ( elem.toLowerCase().indexOf("script") >= 0 ) return "";
	// attributes = "";  // ei laiteta näitä, niin on vähän turvallisempi
	try {
	  val = decodeURIComponent(escape(val))
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = csApp.sanitize(html);
	return html;
}


csApp.directiveTemplateCS = function(t) {
	csApp.taunoPHIndex = 3;
	return  '<div class="csRunDiv">' + 
				  '<p>Here comes header</p>' +
				//  '<p ng-bind-html="getHeader()"></p>
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
  				  (t == "tauno" ? 
				    '<p ng-if="taunoOn" class="pluginHide""><a ng-click="hideTauno()">hide Tauno</a></p>' +
				    '<div ><p></p></div>' + // Tauno code place holder nr 3!!
				    '<p ng-if="!taunoOn" class="pluginShow" ><a ng-click="showTauno()">Click here to show Tauno</a></p>' +
				    '<p ng-if="taunoOn" class="pluginHide"" ><a ng-click="copyTauno()">copy from Tauno</a> | <a ng-click="hideTauno()">hide Tauno</a></p>' +
				    '<p ng-if="taunoOn" class="taunoOhje">Kopioi Taunolla tekemäsi koodi "copy from Tauno"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa että ajossa voi olla eri taulukko kuin Taunossa!</a></p>' +
					"" : "") +   
				  '<pre ng-if="viewCode && codeover">{{code}}</pre>'+
				  '<div class="csRunCode">'+'<p></p>'+
				  '<pre class="csRunPre" ng-if="viewCode &&!codeunder &&!codeover">{{precode}}</pre>'+
				  '<textarea class="csRunArea" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea>'+
				  //'<div class="csRunArea" contentEditable ng-model="usercode" ng-trim="false" "></div>'+
				  '<pre class="csRunPost" ng-if="viewCode &&!codeunder &&!codeover">{{postcode}}</pre>'+
				  '</div>'+
				  //'<br />'+ 
				  '<p class="csRunMenu" >'+
				  '<button ng-if="isRun"  ng-click="runCode();">Aja</button>&nbsp&nbsp'+
				  '<button ng-if="isTest" ng-click="runTest();">Test</button>&nbsp&nbsp'+
				  '<a href="" ng-click="showCode();">Näytä koko koodi</a>&nbsp&nbsp'+
				  '<a href="" ng-click="initCode();">Alusta</a></p>'+
				  '<pre ng-if="viewCode && codeunder">{{code}}</pre>'+
				  (t == "comtest" || t == "tauno" ? '<p class="unitTestGreen"  ng-if="runTestGreen" >&nbsp;ok</p>' : "") +
				  (t == "comtest" || t == "tauno"? '<pre class="unitTestRed"    ng-if="runTestRed">{{comtestError}}</pre>' : "") +
				  '<pre  class="console" ng-if="result">{{result}}</pre>'+
				  // '<p>{{resImage}}</p>'+
				  // '<p>Testi valituksesta</p>' +
				  '<pre ng-if="runError">{{error}}</pre>'+
				  (t == "jypeli" ? '<img  class="console" ng-src="{{imgURL}}" alt="" width="400" ng-if="runSuccess" />' : "") +
				  '<p class="footer">Here comes footer</p>'+
				  '</div>';
}


csApp.set = function(scope,attrs,name,def) {
    scope[name] = def;
    if ( attrs && attrs[name] ) scope[name] = attrs[name];
    if ( scope.attrs && scope.attrs[name] ) scope[name] = scope.attrs[name];
    return scope[name];
}


csApp.directiveFunction = function(t) {
	return {
		link: function (scope, element, attrs) {
			scope.taunoHtml = element[0].childNodes[csApp.taunoPHIndex]; // Check this carefully, where is Tauno placeholder
            scope.plugin = element.parent().attr("data-plugin");
            scope.taskId  = element.parent().attr("id");

			csApp.set(scope,attrs,"file");
			csApp.set(scope,attrs,"lang");
			csApp.set(scope,attrs,"type","console");
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
			csApp.set(scope,attrs,"rows",1);
			csApp.set(scope,attrs,"maxrows",-1);
			csApp.set(scope,attrs,"attrs.bycode");
			csApp.set(scope,attrs,"placeholder","Write your code here");

			scope.minRows = csApp.getInt(scope.rows);
			scope.maxRows = csApp.getInt(scope.maxrows);
			if ( scope.usercode == "" )  scope.usercode = scope.byCode;
			scope.usercode = csApp.commentTrim(scope.usercode);
			scope.byCode = csApp.commentTrim(scope.byCode);
			// scope.usercode = csApp.commentTrim(decodeURIComponent(escape(scope.usercode)));
			// scope.byCode = csApp.commentTrim(decodeURIComponent(escape(scope.byCode)));

            if ( scope.usercode ) {
                var rowCount = csApp.countChars(scope.usercode,'\n') + 1;
                if ( scope.maxRows < 0 && scope.maxRows < rowCount ) scope.maxRows = rowCount; 
            } else if ( scope.maxRows < 0 ) scope.maxRows = 10;
            
			scope.isRun = (scope.type.indexOf("console") >= 0) || (scope.type.indexOf("jypeli") >= 0);
			scope.isTest = scope.type.indexOf("comtest") >= 0;

			scope.edit = element.find("textarea"); // angular.element(e); // $("#"+scope.editid);
			element[0].childNodes[0].outerHTML = csApp.getHeading(attrs,"header",scope,"h4");
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = csApp.getHeading(attrs,"footer",scope,'p class="footer"');
        //    scope.header = head;
		//	scope.getHeader = function() { return head; };
		//	csApp.updateEditSize(scope);
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
		template: csApp.directiveTemplateCS(t),
		// templateUrl: 'csTempl.html'
	}; 
}


csApp.getInt = function(s) {
   var n = parseInt(s);
   if ( n == NaN ) return 0;
   return n;
}   


csApp.countChars = function (s,c) {
	var n = 0;
	for (var i=0; i<s.length; n += +(c===s[i++]));
	return n;
}


csApp.updateEditSize = function(scope) {
//return;
    if ( !scope ) return;
	if ( !scope.usercode ) return;
    n = csApp.countChars(scope.usercode,'\n') + 1;
	if ( n < scope.minRows ) n = scope.minRows;
	if ( n > scope.maxRows ) n = scope.maxRows;
	if ( n < 1 ) n = 1;
	scope.rows = n;
}

csApp.ifIs = function(value,name,def) {
	if ( !value && !def ) return "";
	if ( !value  ) return name+'="'+def+'" ';
	return name+'="'+value+'" ';
}
		
csApp.doVariables = function(v,name) {
	if ( !v ) return "";
	var r = "";
	var va = v.split(";");
	var n;
	for ( n in va ) {
		nv = va[n].trim();
		if ( nv ) r += name+nv+"&";
	}
	return r.replace(/ /g,"");
}

csApp.Hex2Str = function (s) {
  var result = '';
  for (var i=0; i<s.length; i+=2) {
    c = String.fromCharCode(parseInt(s[i]+s[i+1],16));
    result += c;
  }
  return result;
}


csApp.Controller = function($scope,$http,$transclude) {
	$scope.byCode ="";
	$scope.attrs = {};

	$transclude(function(clone, scope) {
		if ( !clone[0] ) return;
		var markJSON = "xxxJSONxxx";
		var markHex = "xxxHEXJSONxxx";
		var s = clone[0].textContent;
		var chex = s.indexOf(markHex) == 0;
		var cjson = s.indexOf(markJSON) == 0;
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
	$scope.resImage = "";
	$scope.imgURL = "";
	$scope.viewCode = false;
	$scope.runSuccess = false;
	$scope.copyingFromTauno = false;

	$scope.$watch('[usercode]', function() {
		if ( !$scope.copyingFromTauno) $scope.muokattu = true;
		$scope.copyingFromTauno = false
		if ( $scope.minRows < $scope.maxRows ) 
			csApp.updateEditSize($scope);
		if ( $scope.viewCode ) $scope.pushShowCodeNow();
	}, true);
	
	
	$scope.runCode = function() {
		var t = "console";
		if ( $scope.type.indexOf("jypeli") >= 0 ) t = "jypeli";
		$scope.doRunCode(t);
	}
	
	$scope.runTest = function() {
		$scope.doRunCode("comtest");
	}
	
	$scope.doRunCode = function(runType) {
		// $scope.viewCode = false;
		if ( $scope.taunoOn && ( !$scope.muokattu || !$scope.usercode ) ) $scope.copyTauno();
		$scope.error = "... running ...";
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.runError = true;
		$scope.result = "";
		$scope.runTestGreen = false;
		$scope.runTestRed = false;

		var t = runType;
		// if ( t == "tauno" ) t = "comtest";

		// params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		params = {
                  //   'input': 1
				  'input': {'usercode':$scope.usercode,
                            'markup': {'type':t, 'file': $scope.file, 'replace': $scope.replace, 'lang': $scope.lang, 'taskId': $scope.taskId}, 
                           }
                  };
		//		  alert($scope.usercode);
        url = "http://tim-beta.it.jyu.fi/cs/answer";
        if ( $scope.plugin ) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if ( i > 0 ) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
		$http({method: 'PUT', url: url, data:params, headers: {'Content-Type': 'application/json'}}
		).success(function(data, status, headers, config) {
			if ( data.web.error ) {
				$scope.error = data.web.error;
				return;
			}
			var imgURL = "";
			$scope.runSuccess = true;
			$scope.runError = !$scope.runSuccess;

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
				   $scope.result = data.web.console;
				else   
				   $scope.error = data.replace("*** Success!\n","");
			}

		}).error(function(data, status) {
			$scope.errors.push(status);
			$scope.error = data.error;
		})
	};
	
	$scope.hideTauno = function() {
		$scope.taunoOn = false;
		$scope.taunoHtml.innerHTML = "<p></p>";
	}

	
	$scope.copyTauno = function() {
		var f = document.getElementById($scope.taunoId);
		// var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
		var s = f.contentWindow.getUserCodeFromTauno();
		$scope.copyingFromTauno = true;
		$scope.usercode = s;
		$scope.muokattu = false;
	}
	
	
	$scope.showTauno = function() {
	
		csApp.taunoNr++;
		var vid = 'tauno'+csApp.taunoNr;
		$scope.taunoId = vid;
		var w = csApp.ifIs($scope.width,"width",700);
		var h = csApp.ifIs($scope.height,"height",500);
		var p = "";
		// var tt = "http://users.jyu.fi/~ji/js/tdbg/";
		// var tt = "http://tim-beta.it.jyu.fi/cs/ptauno/indexi.html";
		var tt = "/cs/ptauno/indexi.html";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://users.jyu.fi/~vesal/js/ptauno/index.html";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://tim-beta.it.jyu.fi/cs/ptauno/index.html";
		if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "/cs/ptauno/index.html";
		var taunoUrl = tt+"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
		var s = $scope.table;
		if ( s && s.length > 0) {
			if ( s[0] == 's' ) p = "ts="+s.substring(1) + "&"; // table it's size param "table: s10"
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
	}
	
	
	$scope.initCode = function() {
		$scope.muokattu = false;
		$scope.usercode = $scope.byCode;
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.runError = false;
		$scope.result = "";
		$scope.viewCode = false;

	}

	
	$scope.stopShow = function() {
		if (angular.isDefined($scope.stop)) {
			$interval.cancel(stop);
			$scope.stop = undefined;
		}
	}
	
	$scope.pushShowCodeNow = function() {
		if ( !$scope.viewCode ) return;
		$scope.showCodeNow(); return;
		$scope.stopShow();
		$scope.stop = $interval(function() {
			$scope.showCodeNow();
		}, 1000,1);	  
	}
	
	
	$scope.showCode = function() {
		$scope.viewCode = !$scope.viewCode;
		$scope.localcode = undefined;
		$scope.showCodeNow();
	}
		
		
	$scope.showCodeLocal = function() {
		// $scope.code = $scope.localcode;
		if ( $scope.localcode == "" ) { $scope.code = $scope.usercode; return; }
		var st = $scope.localcode.split("\n");
		var r = "";
		var rp = ["",""]; // alkuosa, loppuosa
		var step = 0;
		var nl = "";
		var nls = "";
		for (i in st) {
			var s = st[i];
			if ( s.indexOf($scope.replace) >= 0 ) {
				r += nl + $scope.usercode;
				if ( step == 0 ) { step++; nls = ""; continue; }
			} else { 
				r += nl + s;
				rp[step] += nls + s;
			}
			nl = nls = "\n";
		}
		$scope.code = r;
		$scope.precode = rp[0];
		$scope.postcode = rp[1];
	}	
		
		
	$scope.showCodeNow = function() {
	    // var copyEvent = new ClipboardEvent('copy', { dataType: 'text/plain', data: 'kissa' } );
        // document.dispatchEvent(copyEvent);
		if ( !$scope.viewCode ) return;
		if ( angular.isDefined($scope.localcode) ) { $scope.showCodeLocal(); return; } 
		if ( !$scope.file ) { $scope.localcode = ""; $scope.showCodeLocal(); return; }
		
		params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&keplace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		$http({method: 'POST', url:"/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		).success(function(data, status, headers, config) {
			if (data.msg != '')
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
		})
	};
};
