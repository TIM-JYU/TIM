function standardDirective(template,build,extract) {
 return function() {
    return {
      restrict: 'E',
      templateUrl: function(elem,attrs) {
                    return "http://"+elem.parent().attr('data-plugin')+"/"+template;
                   }, 
      replace: true,
      scope: {},
      controller: function($scope, $element, $http) {
        $scope.ident     = $element.parent().attr("id"); 
        $scope.content = JSON.parse($element.attr("data-content"));
        console.log(["initial",$scope.content]);
        $scope.plugin = $element.parent().attr("data-plugin");
        build($scope,$element);
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
