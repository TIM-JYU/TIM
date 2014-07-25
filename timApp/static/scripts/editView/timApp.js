var TimApp = angular.module('timApp', ['ngSanitize', 'controller']);
TimApp.directive('bindOnce', function() {
        return {
                scope: true,
        link: function( $scope ) {
                setTimeout(function() {
                        $scope.$destroy();
                }, 0);
        }
        }
});





