var timApp = window.angular.module('timApp');

/**
 * A reference popup window directive that is used in the document view.
 */
timApp.directive('refPopup', ['$window', '$filter', '$http', function ($window, $filter, $http) {
    return {
        restrict: 'E',
        scope: true, // we need functions from parent scope
        templateUrl: "/static/templates/refPopup.html",
        replace: true,

        link: function ($scope, $element, $attrs) {
            $scope.loaded = false;
            if ('docid' in $attrs && 'parid' in $attrs) {
                //$scope.ref_docid = $attrs.docid;
                //$scope.ref_parid = $attrs.parid;
                $http.get('/par_info/' + $attrs.docid + '/' + $attrs.parid).then($scope.loadSuccess, $scope.loadFail);
                $scope.ref_loaded = true;
            }
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

            $scope.loadSuccess = function (response) {
                var known_attrs = ['doc_name', 'doc_author', 'par_name'];

                for (var i in known_attrs) {
                    attr = known_attrs[i];
                    if (attr in response.data) {
                        $scope[attr] = response.data[attr];
                    }
                }

                $scope.loaded = true;
            };

            $scope.loadFail = function (response) {
                $scope.error = response.data['error'];
                $scope.loaded = true;
            };

            $scope.loaded = true;
            $element.css('position', 'absolute'); // IE needs this
        }
    };
}]);
