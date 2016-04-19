var timApp = angular.module('timApp');

/**
 * A popup menu directive that is used in the document view.
 * Requires a paragraph (element with class "par") or
 * an area (element with a class "area") as its ancestor.
 */
timApp.directive('popupMenu', ['$http', '$window', '$filter', function ($http, $window, $filter) {
    return {
        restrict: 'E',
        scope: true, // we need functions from parent scope
        templateUrl: "/static/templates/popupMenu.html",
        replace: true,

        link: function ($scope, $element, $attrs) {
            $scope.actions = eval('$scope.' + $attrs['actions']);
            $scope.actionsAttr = $attrs['actions'];
            $scope.getContent($attrs['contenturl']);
            if ($attrs['save'])
                $scope.storageAttribute = '$scope.$storage.' + $attrs['save'];
        },

        controller: function ($scope, $element) {
            $scope.$watchGroup(['getLatestActions()'], function (newValues, oldValues, scope) {
                // Update the menu if its items are changed
                $scope.actions = newValues[0];
            });

            $scope.getLatestActions = function () {
                return eval('$scope.' + $scope.actionsAttr);
            };

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
                if (eval($scope.storageAttribute) === fDesc)
                    eval($scope.storageAttribute + ' = null;');
                else
                    eval($scope.storageAttribute + ' = fDesc;');
            };
            
            $scope.getContent = function (contentUrl) {
                if (!contentUrl) {
                    $('#content').remove();
                    return;
                }

                $http.get(contentUrl, {}
                ).success(function (data, status, headers, config) {
                    //$scope.content = data.texts;
                    $('#content').append(data.texts);
                }).error(function () {
                    $window.alert('Error occurred when getting contents.')
                });
            };

            $element.css('position', 'absolute'); // IE needs this
            $scope.$par = $element.parents('.par, .area');
        }
    };
}]);
