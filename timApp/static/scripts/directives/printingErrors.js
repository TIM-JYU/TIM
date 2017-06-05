'use strict';

var angular;
var timApp = angular.module('timApp');

timApp.directive('printing-errors', function() {
    return {
        restrict : 'E',
        template: '<div><h4>An error occurred during printing!</h4><p>{{errorMsg}}</p></div>'
    }

});

/*
app.directive('compile',function($compile, $timeout){
    return{
        restrict:'A',
        link: function(scope,elem,attrs){
            $timeout(function(){
                $compile(elem.contents())(scope);
            });
        }
    };
});

*/
