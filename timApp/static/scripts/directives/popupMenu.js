var timApp = angular.module('timApp');

/**
 * A popup menu directive that is used in the document view.
 * Requires a paragraph (element with class "par") as its ancestor.
 */
timApp.directive('popupMenu', ['$window', '$filter', function ($window, $filter) {
    return {
        restrict: 'E',
        scope: true, // we need functions from parent scope
        templateUrl: "/static/templates/popupMenu.html",
        replace: true,

        link: function ($scope, $element, $attrs) {

        },

        controller: function ($scope, $element) {
            $scope.closePopup = function () {
                $scope.$destroy();
                $element.remove();
            };

            /**
             * Angular expressions can't reference DOM elements, so we use a "proxy" function.
             * @param e Event object
             * @param f The function to call
             */
            $scope.callFunc = function (e, f) {
                f.func(e, $scope.$par);
                $scope.closePopup();
            };

            $scope.saveChoice = function () {
                $scope.$storage.defaultAction = $scope.defaultAction.desc;
            };

            $element.css('position', 'absolute'); // IE needs this
            $scope.$par = $element.parents('.par');
        }
    };
}]);
