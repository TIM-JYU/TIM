angular.module('console',['ngSanitize'])
 .directive('console', function($timeout) {
   return {
     restrict: 'E',
     scope:{},
     controller: function($scope,$element,$sce,$http){
        // This block could be re-used
        $scope.ident     = $element.parent().attr("id"); 
        $scope.plugin    = $element.parent().attr("data-plugin");
        var reqPath      = $scope.plugin+"/"+$scope.ident+"/";  
        // End of generally re-usable TIM stuff

        $scope.history = [{input:"1+2",response:"3-0"},
                          {input:"1+1",response:"error"}];

        $scope.handler=function(e){
            $http({method:'PUT'
                  ,url:$scope.plugin+"/answer/"
                  ,data:{"input":$scope.currentInput}})
             .success(function(data){
                  $scope.submit(data.web.reply);
                  console.log(["data",data.web]);
                 })
             .error(function(data,status,hdrs,cfg){
                  alert(["protocol error",data,status,hdrs,cfg]);
                 });
        
            };
        $scope.currentSize = "normal";
        $scope.currentInput = "";
        $scope.cursor = $scope.history.length; // $scope.history.length means new input is last command.

        $scope.toggleSize = function() {
            if ($scope.currentSize === "normal") 
                 {$scope.currentSize = "enlarged";}
            else {$scope.currentSize = "normal";}
            };

        $scope.submit = function(result){
         
         $scope.history.push({input:$scope.currentInput,
                              response:result});
         $scope.currentInput=""; $scope.cursor=$scope.history.length;
         $timeout(function() {
               var el = $element[0].querySelector(".console-output");
               el.scrollTop = el.scrollHeight;
            });
        };

        $scope.load = function() {
            if ($scope.cursor>=$scope.history.length)
                {$scope.currentInput=""; $scope.cursor=$scope.history.length; return;}
            var norm = Math.min($scope.history.length-1,Math.max(0,$scope.cursor));
            $scope.currentInput=$scope.history[norm].input;
            $scope.cursor = norm;
        }
        

        $scope.handleKey = function(ev) {
         if (ev.which===13) $scope.handler();// submit();
         if (ev.which===40) {$scope.cursor++;$scope.load();}
         if (ev.which===38) {$scope.cursor--;$scope.load();}
        };
      },
     templateUrl: 'NewConsole/Console.template.html',
     replace: true
    };
  });
