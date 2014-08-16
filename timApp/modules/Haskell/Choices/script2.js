angular.module('MCQ', [])
  .directive('mmcq', standardDirective("MMCQTemplate.html"
				     , function($scope){
					// TRANSIENT
					$scope.active = true;
					$scope.answer = new Array($scope.content.question.length);
					$scope.checked = false;
					for(var j=0;j++;j<$scope.answer.length) {$scope.answer[j]=false;};
					//
				     }
                                     , function(scope){return scope.answer;}))
  .directive('mcq', standardDirective("MCQTemplate.html"
				     , function($scope){return;}
                                     , function(scope){return scope.userSelection;}));
