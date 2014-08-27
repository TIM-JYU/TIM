angular.module('MCQ', [])
  .directive('mmcq', standardDirective("MMCQTemplate.html"
				     , function($scope){
					$scope.active  = false;
					$scope.checked = true;
					$scope.answer = $scope.content.state; //new Array($scope.content.question.length);
					if ($scope.answer==null||$scope.answer==undefined) 
                        {
                        $scope.checked = false;
                        $scope.active  = true;
                        $scope.answer = new Array($scope.content.question.choices.length);
    					for(var j=0;j++;j<$scope.answer.length) {$scope.answer[j]=false;};
	    			    }
                    console.log(["ANSWER",$scope.answer]);
                    }
                    , function(scope){scope.active=false;return scope.answer;})) //TODO: cleanup
  .directive('mcq', standardDirective("MCQTemplate.html"
				     , function($scope){return;}
                                     , function(scope){return parseInt(scope.userSelection);}));
