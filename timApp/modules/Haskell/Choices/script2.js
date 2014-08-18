angular.module('MCQ', [])
  .directive('mmcq', standardDirective("MMCQTemplate.html"
				     , function($scope){
					$scope.active  = false;
					$scope.checked = true;
					$scope.answer = $scope.content.state; //new Array($scope.content.question.length);
					if ($scope.answer==null) 
                        {
                        $scope.checked = false;
                        $scope.active  = true;
    					for(var j=0;j++;j<$scope.answer.length) {$scope.answer[j]=false;};
	    			    }
                    }
                    , function(scope){scope.active=false;return scope.answer;})) //TODO: cleanup
  .directive('mcq', standardDirective("MCQTemplate.html"
				     , function($scope){return;}
                                     , function(scope){return scope.userSelection;}));
