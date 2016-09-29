var angular;
var timApp = angular.module('timApp');

timApp.directive("bookmarks", ['$window', '$log', '$http', '$uibModal', '$timeout', function ($window, $log, $http, $uibModal, $timeout) {
    "use strict";
    return {
        restrict: 'E',
        scope: {
            userId: '=?',
            data: '=?'
        },
        templateUrl: "/static/templates/bookmarks.html",
        link: function ($scope, $element) {

        },

        controller: function ($scope, $element, $attrs) {
            var sc = $scope;
            sc.data = [];

            sc.getFromServer = function (response, groupToKeepOpen) {
                sc.data = response.data;
                sc.keepGroupOpen(groupToKeepOpen);
            };

            sc.keepGroupOpen = function (groupToKeepOpen) {
                if (!groupToKeepOpen) {
                    return;
                }
                for (var i = 0; i < sc.data.length; ++i) {
                    if (sc.data[i].name === groupToKeepOpen.name) {
                        sc.data[i].isOpen = true;
                        return;
                    }
                }
            };

            sc.getTopLevelBookmarks = function () {
                for (var i = 0; i < sc.data.length; ++i) {
                    if (sc.data[i].name === '') {
                        return sc.data[i].items;
                    }
                }
                return [];
            };

            sc.isSaveablePage = function () {
                return true;
            };

            sc.newBookmark = function (group, e) {
                e.stopPropagation();
                e.preventDefault();
                var currentPage = $window.location.pathname.replace(/\/$/, "");
                var modalInstance = $uibModal.open({
                    animation: false,
                    ariaLabelledBy: 'modal-title',
                    ariaDescribedBy: 'modal-body',
                    templateUrl: 'createBookmark.html',
                    controller: 'CreateBookmarkCtrl',
                    controllerAs: '$ctrl',
                    size: 'md',
                    resolve: {
                        bookmark: function () {
                            return {
                                group: group || '',
                                name: currentPage.slice(currentPage.lastIndexOf('/') + 1),
                                link: ''
                            };
                        }
                    }
                });

                modalInstance.result.then(function (bookmark) {
                    if (!bookmark.name) {
                        return;
                    }
                    $http.post('/bookmarks/add', bookmark)
                        .then(sc.getFromServer, function (response) {
                            $window.alert("Could not add bookmark.");
                        });
                }, function () {
                });
            };

            sc.editItem = function (group, item, e) {
                e.stopPropagation();
                e.preventDefault();
                var modalInstance = $uibModal.open({
                    animation: false,
                    ariaLabelledBy: 'modal-title',
                    ariaDescribedBy: 'modal-body',
                    templateUrl: 'createBookmark.html',
                    controller: 'CreateBookmarkCtrl',
                    controllerAs: '$ctrl',
                    size: 'md',
                    resolve: {
                        bookmark: function () {
                            return {
                                group: group.name,
                                name: item.name,
                                link: item.path
                            };
                        }
                    }
                });

                modalInstance.result.then(function (bookmark) {
                    if (!bookmark.name) {
                        return;
                    }
                    sc.deleteItem(group, item, e).then(function (response) {
                        $http.post('/bookmarks/add', bookmark)
                            .then(function (response) {
                                sc.getFromServer(response, group);
                            }, function (response) {
                                $window.alert("Could not add bookmark.");
                            });
                    });
                }, function () {
                    $timeout(function () {
                        sc.keepGroupOpen(group);
                    }, 0);
                });
            };

            sc.deleteItem = function (group, item, e) {
                e.stopPropagation();
                e.preventDefault();
                return $http.post('/bookmarks/delete', {
                    group: group.name,
                    name: item.name
                })
                    .then(function (response) {
                        sc.getFromServer(response, group);
                    }, function (response) {
                        $window.alert("Could not delete bookmark.");
                    });
            };

            sc.deleteGroup = function (group, e) {
                e.stopPropagation();
                e.preventDefault();
                if ($window.confirm('Are you sure you want to delete this bookmark group?')) {
                    $http.post('/bookmarks/deleteGroup', {group: group.name})
                        .then(sc.getFromServer, function (response) {
                            $window.alert("Could not delete bookmark group.");
                        });
                }
            };

            sc.toggleDelete = function (e) {
                e.stopPropagation();
                e.preventDefault();
                sc.deleting = !sc.deleting;
            };

            sc.deleting = false;

            if (sc.userId) {
                $http.get('/bookmarks/get/' + sc.userId).then(sc.getFromServer, function (response) {
                    $window.alert("Could not fetch bookmarks.");
                });
            }
        }
    };
}]);

timApp.controller('CreateBookmarkCtrl', ['$uibModalInstance', '$window', 'bookmark', function ($uibModalInstance, $window, bookmark) {
    "use strict";
    var $ctrl = this;
    $ctrl.bookmarkForm = {};
    $ctrl.bookmark = bookmark;
    $ctrl.focusGroup = !bookmark.group;
    $ctrl.focusName = !$ctrl.focusGroup;
    $ctrl.showUrlCheckbox = $window.location.search.length > 1;

    $ctrl.ok = function () {
        if (!$ctrl.bookmark.link) {
            $ctrl.bookmark.link = $window.location.pathname;
        }
        if ($ctrl.includeParams) {
            $ctrl.bookmark.link += $window.location.search;
        }
        $uibModalInstance.close($ctrl.bookmark);
    };

    $ctrl.cancel = function () {
        $uibModalInstance.dismiss('cancel');
    };
}]);
