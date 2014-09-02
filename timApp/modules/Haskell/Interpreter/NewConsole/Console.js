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
        $scope.content = JSON.parse($element.attr("data-content"));
        // End of generally re-usable TIM stuff

        $scope.examples = $scope.content.examples;

        $scope.history = []; 

        $scope.loadExample=function(i){$scope.currentInput=$scope.examples[i].expr;$scope.focusOnInput()};
        $scope.focusOnInput=function(){
               var el = $element[0].querySelector(".console-input");
               el.focus();
        
        };
        $scope.handler=function(e){
            $http({method:'PUT'
                  ,url:$scope.plugin+"/"+$scope.ident+"/answer/"
                  ,data:{"input":$scope.currentInput}})
             .success(function(data){
                  $scope.submit(data.web);
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
      templateUrl: function(elem,attrs) {
                    return "http://"+elem.parent().attr('data-plugin')+"/"+'NewConsole/Console.template.html';
                   }, 
     replace: true
    };
  });
