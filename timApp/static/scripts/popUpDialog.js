timApp.directive('popUpDialog', function() {
    return {
        restrict: 'E',
        template: "<div class='pop-up' ng-show='show'> " +
        "<div class='pop-up-overlay'></div> " +
        "<div class='pop-up-dialog'>" +
        "<div class='pop-up-dialog-content' ng-transclude></div>" +
        "</div>" +
        "</div>",

        scope: {
            show: '='
        },
        replace: true,
        transclude: true
    };
});