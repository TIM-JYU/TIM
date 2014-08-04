angular.module('MCQ', [])
  .directive('mcq', function() {
    return {
      restrict: 'E',
      templateUrl:"MCQTemplate.html",
      replace: true,
      scope: {},
      controller: function($scope, $element, $http) {
        $scope.ident     = $element.parent().attr("id"); 
        $scope.mcq = JSON.parse($element.attr("data-content"));
        $scope.plugin = $element.parent().attr("data-plugin");
        $scope.userSelection = null;
        $scope.submit = function () {
            $http({method:'PUT'
                  ,url:$scope.plugin+"/answer/"
                  ,markup:null
                  ,state:null
                  ,input:$scope.userSelection})
             .success(function(data){
                  $scope.mcq = data;
                  console.log(["data",data]);
                 })
             .error(function(data,status,hdrs,cfg){
                  alert(["error",data,status,hdrs,cfg]);
                 });
            };
       }
    }
  });
