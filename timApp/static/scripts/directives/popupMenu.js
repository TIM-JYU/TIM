var timApp = angular.module('timApp');

/**
 * A popup menu directive that is used in the document view.
 * Requires a paragraph (element with class "par") or
 * an area (element with a class "area") as its ancestor.
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

            $scope.getChecked = function (fDesc) {
                if (fDesc === null || $scope.$storage.defaultAction === null)
                    return "";

                return fDesc === $scope.$storage.defaultAction ? "checked" : "";
            };

            $scope.clicked = function (fDesc) {
                if ($scope.$storage.defaultAction === fDesc)
                    $scope.$storage.defaultAction = $scope.$storage.defaultDefaultAction;
                else
                    $scope.$storage.defaultAction = fDesc;

                console.log($scope.$storage.defaultAction);
            };

            $element.css('position', 'absolute'); // IE needs this
            $scope.$par = $element.parents('.par, .area');
        }
    };
}]);
