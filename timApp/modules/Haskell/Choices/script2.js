function standardDirective(template,extract) {
 return function() {
    return {
      restrict: 'E',
      templateUrl: function(elem,attrs) {
                    return elem.parent().attr('data-plugin')+"/"+template;
                   }, 
      replace: true,
      scope: {},
      controller: function($scope, $element, $http) {
        $scope.ident     = $element.parent().attr("id"); 
        $scope.content = JSON.parse($element.attr("data-content"));
        console.log(["initial",$scope.content]);
        $scope.plugin = $element.parent().attr("data-plugin");

        $scope.active = true;
	$scope.answer = new Array($scope.content.question.length);
	$scope.checked = true;
        for(var j=0;j++;j<$scope.answer.length) {$scope.answer[j]=false;};

        $scope.submit = function () {
            var message = {input:extract($scope)};
            var localState  = JSON.parse($element.parent().attr("data-state")  || "null");
            var localMarkup = JSON.parse($element.parent().attr("data-markup") || "null");
            if (localState!==null) {message.state = localState};
            if (localMarkup!==null) {message.markup = localMarkup};
            $http({method:'PUT'
                  ,url:$scope.plugin+"/"+$scope.ident+"/answer/"
                  ,data:message})
             .success(function(data){
                  $scope.content = data.web
                  console.log(["data",$scope.content]);
                  $scope.checked = true; 			
                 })
             .error(function(data,status,hdrs,cfg){
                  alert(["error",data,status,hdrs,cfg]);
                 });
            };
       }
    }
  }
}
angular.module('MCQ', [])
  .directive('mcq', standardDirective("MMCQTemplate.html"
                                     , function(scope){return scope.userSelection;}));

