timApp.directive('popUpDialog', function () {
    return {
        restrict: 'E',
        template: "<div class='pop-up' ng-show='show' id='popUpBack'>"+
        "<div class='pop-up-overlay'></div> " +
        "<div class='pop-up-dialog' tim-draggable-fixed ng-mousedown='checkDown($event)' ng-mouseup='checkUp($event)'>" +
        "<div class='pop-up-dialog-content' ng-transclude></div>" +
        "</div>" +
        "</div>",

        scope: {
            show: '='
        },
        replace: true,
        transclude: true,

        link: function ($scope, $element) {

            $scope.checkDown = function (e) {
                $scope.mouseDownX = e.clientX;
                $scope.mouseDownY = e.clientY;
                var window = $element.find("popUpBack");
                window.context.style.position = "absolute";
                window.context.style.bottom = 'auto';

            };

            $scope.checkUp = function () {
                var window = $element.find("popUpBack");
                window.context.style.position = "fixed";
            };
        }

    };
});