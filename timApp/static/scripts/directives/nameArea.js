var timApp = angular.module('timApp');

/**
 * A popup window directive that is used in the document view
 * when creating a new area.
 */
timApp.directive('nameArea', ['$http', '$window', '$filter', function ($http, $window, $filter) {
    return {
        restrict: 'E',
        scope: true, // we need functions from parent scope
        templateUrl: "/static/templates/nameArea.html",
        replace: true,

        link: function ($scope, $element, $attrs) {
            $scope.$area = $element.parents('.area').first();
            if ($attrs['onclose'])
                $scope.onClose = eval('$scope.' + $attrs['onclose']);
            if ($attrs['onok'])
                $scope.onOk = eval('$scope.' + $attrs['onok']);
            if ($attrs['oncancel'])
                $scope.onCancel = eval('$scope.' + $attrs['oncancel']);

            $('#areaname').keypress(function (e) {
                if (e.which == 13)
                    $scope.addArea();
            });

            $(function() {
                $('.date').datetimepicker({format: "DD.MM.YYYY HH:mm", locale: "en-GB"});
            });
        },

        controller: function ($scope, $element) {
            $scope.closePopup = function () {
                $scope.$destroy();
                $element.remove();

                if ($scope.onClose)
                    $scope.onClose($scope.$area);
            };
            
            $scope.addArea = function () {
                $scope.options.starttime = $('#starttime').val();
                $scope.options.endtime = $('#endtime').val();
                $scope.closePopup();

                if ($scope.onOk)
                    $scope.onOk($scope.$area, $scope.areaName, $scope.options);
            };

            $scope.cancelAdd = function () {
                $scope.closePopup();

                if ($scope.onCancel)
                    $scope.onCancel($scope.$area);
            };

            $scope.toggleCollapsible = function () {
                if ($scope.options.collapsible) {
                    $('#collapsible-options').removeClass('hidden');
                } else {
                    $('#collapsible-options').addClass('hidden');
                }
            };

            $scope.toggleTimed = function () {
                if ($scope.options.timed) {
                    $('#timed-options').removeClass('hidden');
                } else {
                    $('#timed-options').addClass('hidden');
                }
            };

            $scope.areaName = "";
            $scope.options = {collapse: true, hlevel: 0};
            $element.css('position', 'absolute'); // IE needs this

            $('#areaname').focus();
        }
    };
}]);
