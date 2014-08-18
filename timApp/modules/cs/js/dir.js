var csApp = angular.module('csApp', ['ngSanitize']);
csApp.directive('csRunner',function() {	return csApp.directiveFunction('console'); });
csApp.directive('csJypeliRunner',function() { return csApp.directiveFunction('jypeli'); });


csApp.commentTrim = function(s) {
	if ( !s ) return;
	n = s.indexOf("//\n");
	if ( n < 0 ) return s;
	return s.substr(3);
}

csApp.getHeading = function(a,key,$scope) {
	if ( !a ) return "";
	var h = a[key];
	if ( !h ) return "";
	// if ( h.toLowerCase().indexOf("script") >= 0 ) return "";
	var st = h.split("!!");
	var elem = "h4";
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	// var ea = elem.split(" ",2);
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	if ( elem.toLowerCase().indexOf("script") >= 0 ) return "";
	attributes = "";  // ei laiteta näitä, niin on vähän turvallisempi
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	// html = $sanitize(html);
	return html;
}

csApp.directiveFunction = function(t) {
	return {
		link: function (scope, element, attrs) {
			scope.file = attrs.file;
			scope.type = attrs.type;
			scope.replace = attrs.replace;
			scope.rows = 1;
			scope.taskId = "omanimi";
			if ( t == "jypeli" ) {
				scope.taskId = "lumiukko";
			}
			scope.usercode = "";
			
			scope.stem = attrs.stem;
			scope.placeholder = "Write your code here";
			
			if ( attrs.usercode ) scope.usercode = attrs.usercode;
			if ( attrs.rows ) scope.rows = attrs.rows;
			scope.minRows = csApp.getInt(scope.rows);
			if ( attrs.maxrows )scope.maxRows = csApp.getInt(attrs.maxrows);
			if ( attrs.bycode ) scope.byCode = attrs.bycode;
			if ( scope.usercode == "" )  scope.usercode = scope.byCode;
			if ( attrs.placeholder ) scope.placeholder = attrs.placeholder;
			scope.usercode = csApp.commentTrim(scope.usercode);
			scope.byCode = csApp.commentTrim(scope.byCode);
			scope.edit = element.find("textarea"); // angular.element(e); // $("#"+scope.editid);
			head = csApp.getHeading(attrs,"header",scope);
			element[0].childNodes[0].outerHTML = csApp.getHeading(attrs,"header",scope);
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = csApp.getHeading(attrs,"footer",scope);
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
		template: '<div class="csRunDiv">' +
				  '<p>Hero comes header</p>' +
				//  '<p ng-bind-html="getHeader()"></p>
				  '<p ng-show="stem" class="csRunStem" >{{stem}}</p>' +
				  '<p><textarea class="csRunArea" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea></p>'+
				  //'<br />'+ 
				  '<p><button ng-click="runCode();">Aja</button>'+
				  '<a href="" ng-click="showCode();">Näytä koko koodi</a> '+
				  '<a href="" ng-click="initCode();">Alusta</a></p>'+
				  '<pre ng-show="viewCode">{{code}}</pre>'+
				  '<pre  class="console" ng-show="result">{{result}}</pre>'+
				  // '<p>{{resImage}}</p>'+
				  '<pre ng-show="runError">{{error}}</pre>'+
				  (t == "jypeli" ? '<img  class="console" ng-src="{{imgURL}}" alt="" width="500" ng-show="runSuccess" />' : "") +
				  '<p>Hero comes footer</p>'+
				  '</div>'
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
    if ( !scope ) return;
	if ( !scope.usercode ) return;
    n = csApp.countChars(scope.usercode,'\n') + 1;
	if ( n < scope.minRows ) n = scope.minRows;
	if ( n > scope.maxRows ) n = scope.maxRows;
	if ( n < 1 ) n = 1;
	scope.rows = n;
}

		
csApp.Controller = function($scope,$http,$transclude) {
	$scope.byCode ="";
	$transclude(function(clone, scope) {
		if ( clone[0] )
			$scope.byCode = clone[0].textContent;
	});
	$scope.header = "";
	$scope.rows = 5;
	$scope.errors = [];
	$scope.type = "jypeli";
	// $scope.replace = "INSERT YOUR CODE HERE";
	// $scope.file = "https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs";
	$scope.result = "";
	$scope.resImage = "";
	$scope.imgURL = "";
	$scope.viewCode = false;
	$scope.runSuccess = false;

	$scope.$watch('[usercode]', function() {
		if ( $scope.minRows < $scope.maxRows ) 
			csApp.updateEditSize($scope);
		if ( $scope.viewCode ) $scope.pushShowCodeNow();
	}, true);
	
	
	$scope.runCode = function() {
		// $scope.viewCode = false;
		$scope.error = "... running ...";
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.runError = true;
		$scope.result = "";

		// params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		params = {'markup': {'type':$scope.type, 'file': $scope.file, 'replace': $scope.replace}, 
				  'taskId': $scope.taskId,
				  'input': {'usercode':$scope.usercode}};
		//		  alert($scope.usercode);
		$http({method: 'PUT', url:"http://tim-beta.it.jyu.fi/cs/input", data:params, headers: {'Content-Type': 'application/json'}}
		).success(function(data, status, headers, config) {
			if ( data.web.error ) {
				$scope.error = data.web.error;
				return;
			}
			var imgURL = "";
			$scope.runSuccess = true;
			$scope.runError = !$scope.runSuccess;

			imgURL = data.web.image;
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
	
	
	$scope.initCode = function() {
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
		var nl = "";
		for (i in st) {
			var s = st[i];
			if ( s.indexOf($scope.replace) >= 0 ) r += nl + $scope.usercode;
			else r += nl + s;
			nl = "\n";
		}
		$scope.code = r;
	}	
		
		
	$scope.showCodeNow = function() {
		if ( !$scope.viewCode ) return;
		if ( angular.isDefined($scope.localcode) ) { $scope.showCodeLocal(); return; } 
		if ( !$scope.file ) { $scope.localcode = ""; $scope.showCodeLocal(); return; }
		
		params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&keplace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		$http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
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
