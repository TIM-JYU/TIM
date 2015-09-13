var angular;

var timApp = angular.module('timApp');

timApp.controller('UserListController', ['$scope', '$filter', 'ngTableParams',
    function ($scope, $filter, ngTableParams) {
        $scope.tableParams = new ngTableParams({
            page: 1,
            count: $scope.users.length,
            filter: {
                //name: 'M'
            },
            sorting: {
                //name: 'asc'
            }
        }, {
            counts: [],
            total: $scope.users.length,
            filterDelay: 20,
            getData: function ($defer, params) {

                var filteredData = params.filter() ?
                    $filter('filter')($scope.users, params.filter()) :
                    $scope.users;
                var orderBy = params.orderBy();
                if ( !orderBy || orderBy.length < 1 ) orderBy = ["+real_name"];    
                var orderedData = params.sorting() ?
                    $filter('orderBy')(filteredData, orderBy ) :
                    $scope.users;

                params.total(orderedData.length);
                $defer.resolve(orderedData.slice((params.page() - 1) * params.count(), params.page() * params.count()));
            }
        });
    }]);
