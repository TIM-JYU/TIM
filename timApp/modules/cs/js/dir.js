		var csApp = angular.module('csApp', []);
		csApp.directive('csRunner',function() {
			return {
			    link: function (scope, element, attrs) {
				    scope.file = attrs.file;
				    scope.type = attrs.type;
				    scope.replace = attrs.replace;
					scope.rows = 5;
					scope.usercode = "";
					scope.taskId = "omanimi";
				    if ( attrs.usercode ) scope.usercode = attrs.usercode;
					if ( attrs.rows ) scope.rows = attrs.rows;
					if ( attrs.bycode ) scope.byCode = attrs.bycode;
					if ( scope.usercode == "" )  scope.usercode = scope.byCode;
				},		
                scope: {},				
				controller: csApp.Controller,
				restrict: 'AE',
				replace: 'true',
				template: '<div >'+
						  '    <textarea cols="80" rows={{rows}} ng-model="usercode" ng-trim="false" ></textarea>'+
						  '	<br />'+
						  '    <button ng-click="runCode();">Aja</button>'+ 
						  '    <a href="" ng-click="showCode();">Näytä koko koodi</a> '+
						  '    <a href="" ng-click="initCode();">Alusta</a>'+
						  '    <pre ng-show="viewCode">{{code}}</pre>'+
						  '    <pre ng-show="runError">{{error}}</pre>'+
						  '    <pre class="console" ng-show="runSuccess">{{result}}</pre>'+
						  '</div>'
				// templateUrl: 'csTempl.html'
			}; 
		});
		csApp.directive('csJypeliRunner',function() {
			return {
			    link: function (scope, element, attrs) {
				    scope.file = attrs.file;
				    scope.type = attrs.type;
				    scope.replace = attrs.replace;
					scope.rows = 15;
					scope.usercode = "";
					scope.taskId = "lumiukko";
				    if ( attrs.usercode ) scope.usercode = attrs.usercode;
					if ( attrs.rows ) scope.rows = attrs.rows;
					if ( attrs.bycode ) scope.byCode = attrs.bycode;
					if ( scope.usercode == "" )  scope.usercode = scope.byCode;
				},		
                scope: {},				
				controller: csApp.Controller,
				restrict: 'AE',
				replace: 'true',
				template: '<div >'+
						  '<textarea cols="80" rows={{rows}} ng-model="usercode" ng-trim="false"></textarea>'+
						  '<br />'+ 
						  '<button ng-click="runCode();">Aja</button>'+
						  '<a href="" ng-click="showCode();">Näytä koko koodi</a> '+
						  '<a href="" ng-click="initCode();">Alusta</a>'+
						  '<pre ng-show="viewCode">{{code}}</pre>'+
						  '<pre  class="console" ng-show="result">{{result}}</pre>'+
						  '<p>{{resImage}}</p>'+
						  '<pre ng-show="runError">{{error}}</pre>'+
						  '<img  class="console" ng-src="{{imgURL}}" alt="" width="500" ng-show="runSuccess" />'+
						  '</div>'
				// templateUrl: 'csTempl.html'
			}; 
		});
        csApp.Controller = function($scope,$http) {
		            $scope.rows = 5;
                    $scope.errors = [];
                    /* $scope.msgs = [];*/
                    $scope.type = "jypeli";
                    // $scope.replace = "INSERT YOUR CODE HERE";
                    // $scope.file = "https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs";
                    $scope.byCode ="";
                    $scope.result = "";
                    $scope.resImage = "";
                    $scope.imgURL = "";
                    $scope.viewCode = false;
                    $scope.runSuccess = false;

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
					}
					
					
                    $scope.showCode = function() {
                        $scope.viewCode = !$scope.viewCode;
                        // $scope.result = "";
                        // $scope.resImage = "";
                        // $scope.imgURL = "";
                        // $scope.runSuccess = false;

						if ( !$scope.viewCode ) return;
						
                        params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
                        $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
                        ).success(function(data, status, headers, config) {
							if (data.msg != '')
							{
								$scope.code = data;
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
