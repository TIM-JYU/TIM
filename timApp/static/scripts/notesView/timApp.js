var timApp = angular.module('timApp', ['ngSanitize', 'controller', 'angularFileUpload']);
timApp.directive('bindOnce', function() {
        return {
                scope: true,
        link: function( $scope ) {
                setTimeout(function() {
                        $scope.$destroy();
                }, 0);
        }
        }
});





