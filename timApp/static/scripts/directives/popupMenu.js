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
            $scope.$pars = $($attrs['srcid']);
            $scope.actions = eval('$scope.' + $attrs['actions']);
            $scope.actionsAttr = $attrs['actions'];
            $scope.editButton = false;
            $scope.areaEditButton = false;
            $scope.editContext = null;

            $scope.getContent($attrs['contenturl']);
            if ($attrs['save'])
                $scope.storageAttribute = '$scope.$storage.' + $attrs['save'];
            if ($attrs['onclose'])
                $scope.onClose = eval('$scope.' + $attrs['onclose']);

            if ($attrs['editcontext'])
                $scope.editContext = $attrs['editcontext'];
            if ($attrs['editbutton'])
                $scope.editButton = eval($attrs['editbutton']);
            if ($attrs['areaeditbutton'])
                $scope.areaEditButton = eval($attrs['areaeditbutton']);

            $scope.colClass = $scope.storageAttribute ? 'col-xs-10' : 'col-xs-12';
            $scope.halfColClass = $scope.storageAttribute ? 'col-xs-5' : 'col-xs-6';
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

                if ($scope.onClose)
                    $scope.onClose($scope.$pars);
            };

            /**
             * Angular expressions can't reference DOM elements, so we use a "proxy" function.
             * @param e Event object
             * @param f The function to call
             */
            $scope.callFunc = function (e, f) {
                f.func(e, $scope.$pars);
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

            $scope.watchEditMode = function(newEditMode, oldEditMode) {
                //console.log("Edit context set from " + oldEditMode + " to " + newEditMode);
                if ($scope.editContext && newEditMode && newEditMode != $scope.editContext) {
                    // We don't want to destroy our scope before returning from this function
                    $window.setTimeout($scope.closePopup, 0.1);
                }
            };

            $scope.model = {editState: $window.editMode};
            $scope.$watch('model.editState', $window.watchEditMode);
            $scope.$watch('model.editState', $scope.watchEditMode);

            $element.css('position', 'absolute'); // IE needs this
        }
    };
}]);
