		var csApp = angular.module('csApp', []);
		csApp.directive('csRunner',function() {
			return {
			    link: function (scope, element, attrs) {
				    scope.file = attrs.file;
				    scope.type = attrs.type;
				    scope.replace = attrs.replace;
					scope.rows = 5;
					if ( attrs.rows ) scope.rows = attrs.rows;
					if ( attrs.bycode ) scope.byCode = attrs.bycode;
				},		
                scope: {},				
				controller: csApp.Controller,
				restrict: 'AE',
				replace: 'true',
				template: '<div >'+
						  '    <textarea cols="80" rows={{rows}} ng-model="byCode" ng-trim="false" ></textarea>'+
						  '	<br />'+
						  '    <button ng-click="runCode();">Aja</button>'+ 
						  '    <button ng-click="runCodeJSON();">Aja JSON</button>'+
						  '    <a href="" ng-click="showCode();">Näytä koko koodi</a>'+
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
					if ( attrs.rows ) scope.rows = attrs.rows;
					if ( attrs.bycode ) scope.byCode = attrs.bycode;
				},		
                scope: {},				
				controller: csApp.Controller,
				restrict: 'AE',
				replace: 'true',
				template: '<div >'+
						  '<textarea cols="80" rows={{rows}} ng-model="byCode" ng-trim="false"></textarea>'+
						  '<br />'+ 
						  '<button ng-click="runCode();">Aja</button>'+
						  '<a href="" ng-click="showCode();">Näytä koko koodi</a>'+
						  '<pre ng-show="viewCode">{{code}}</pre>'+
						  '<!-- <pre  class="console" ng-show="runSuccess">{{result}}</pre> -->'+
						  '<p>{{resImage}}</p>'+
						  '<pre ng-show="runError">{{error}}</pre>'+
						  '<img  class="console" src="{{imgURL}}" alt="" width="500" ng-show="runSuccess" />'+
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

                        //$scope.errors.splice(0, $scope.errors.length); // remove all error messages
                        //$scope.msgs.splice(0, $scope.msgs.length);
                        //params = {_action:'insert'};
                        //params = {'file': $scope.file, 'replace': $scope.replace, 'by': $scope.byCode};
                        params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.byCode);
                        //_.extend(params, objData); // using underscore or lodash
                        // $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/svn/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
                        $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
                                // $http.post("http://tim-beta.it.jyu.fi/svn/?linefmt={0:03d}%20", 'file='+$scope.file+ '&replace='+ $scope.replace+ '&by=' + $scope.byCode
                        ).success(function(data, status, headers, config) {
                                    if (data.msg != '')
                                    {
                                        var imgURL = "";
                                        var res = data.split("\n");
										$scope.runSuccess = data.indexOf("*** Success!\n","") >= 0;
										$scope.runError = !$scope.runSuccess;

                                        if ( res.length > 1) {
                                            var imgres = "*** Screenshot: ";
                                            var n = res[1].indexOf(imgres);
                                            if (n  == 0) imgURL = res[1].substring(imgres.length);
                                        }
                                        if ( imgURL != "" ) {
                                            // $scope.resImage = '<img src="' + imgURL + ' " alt="Result image" />';
                                            $scope.imgURL = imgURL;
											$scope.result = "";
                                        } else {
										    if ( $scope.runSuccess )  
                                               $scope.result = data.replace("*** Success!\n","");
											else   
                                               $scope.error = data.replace("*** Success!\n","");
                                        }

                                    }
                                    else
                                    {
                                        $scope.errors.push(data.error);
                                        $scope.error = data.error;
                                    }
                                }).error(function(data, status) {
                                    $scope.errors.push(status);
                                    $scope.error = data.error;
                                })
                    };
                    $scope.runCodeJSON = function() {
                        // $scope.viewCode = false;
                        $scope.error = "... running ...";
                        $scope.resImage = "";
                        $scope.imgURL = "";
                        $scope.runSuccess = false;
                        $scope.runError = true;

                        //$scope.errors.splice(0, $scope.errors.length); // remove all error messages
                        //$scope.msgs.splice(0, $scope.msgs.length);
                        //params = {_action:'insert'};
                        params = {'type':$scope.type, 'file': $scope.file, 'replace': $scope.replace, 'by': $scope.byCode};
                        // params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.byCode);
                        //_.extend(params, objData); // using underscore or lodash
                        // $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/svn/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
                        $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/json'}}
                                // $http.post("http://tim-beta.it.jyu.fi/svn/?linefmt={0:03d}%20", 'file='+$scope.file+ '&replace='+ $scope.replace+ '&by=' + $scope.byCode
                        ).success(function(data, status, headers, config) {
                                    if (data.msg != '')
                                    {
                                        var imgURL = "";
                                        var res = data.split("\n");
										$scope.runSuccess = data.indexOf("*** Success!\n","") >= 0;
										$scope.runError = !$scope.runSuccess;

                                        if ( res.length > 1) {
                                            var imgres = "*** Screenshot: ";
                                            var n = res[1].indexOf(imgres);
                                            if (n  == 0) imgURL = res[1].substring(imgres.length);
                                        }
                                        if ( imgURL != "" ) {
                                            // $scope.resImage = '<img src="' + imgURL + ' " alt="Result image" />';
                                            $scope.imgURL = imgURL;
											$scope.result = "";
                                        } else {
										    if ( $scope.runSuccess )  
                                               $scope.result = data.replace("*** Success!\n","");
											else   
                                               $scope.error = data.replace("*** Success!\n","");
                                        }

                                    }
                                    else
                                    {
                                        $scope.errors.push(data.error);
                                        $scope.error = data.error;
                                    }
                                }).error(function(data, status) {
                                    $scope.errors.push(status);
                                    $scope.error = data.error;
                                })
                    };
                    $scope.showCode = function() {
                        $scope.viewCode = !$scope.viewCode;
                        // $scope.result = "";
                        // $scope.resImage = "";
                        // $scope.imgURL = "";
                        // $scope.runSuccess = false;

						if ( !$scope.viewCode ) return;
						
                        params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.byCode);
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
