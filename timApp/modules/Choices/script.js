angular.module('MCQ', [])
  .directive('mcq', function() {
    return {
      restrict: 'E',
      templateUrl:function(elem,attrs) {
              return elem.parent().attr('data-plugin')+"MCQTemplate.html";
                  }, 
      replace: true,
      scope: {},
      controller: function($scope, $element, $http) {
        $scope.ident     = $element.parent().attr("id"); 
        $scope.mcq = JSON.parse($element.attr("data-content"));
        $scope.plugin = $element.parent().attr("data-plugin");
        $scope.userSelection = null;
        $scope.submit = function () {
            var message = {input:$scope.userSelection};
            var localState  = JSON.parse($element.parent().attr("data-state")  || "null");
            var localMarkup = JSON.parse($element.parent().attr("data-markup") || "null");
            if (localState!==null) {message.state = localState};
            if (localMarkup!==null) {message.markup = localMarkup};
            console.log(JSON.stringify(message));
            $http({method:'PUT'
                  ,url:$scope.plugin+"/answer/"
                  ,data:message})
             .success(function(data){
                  $scope.mcq = data.web;
                  console.log(["data",data]);
                 })
             .error(function(data,status,hdrs,cfg){
                  alert(["error",data,status,hdrs,cfg]);
                 });
            };
       }
    }
  });
