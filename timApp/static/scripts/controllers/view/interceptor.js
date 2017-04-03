/* globals angular, timLogTime */

var timApp = angular.module('timApp').config(['$httpProvider', function ($httpProvider) {
    "use strict";
    timLogTime("timApp config", "view");
    function escapeId(id) {
        return "#" + id.replace(/(:|\.|\[|\]|,|=)/g, "\\$1");
    }

    var interceptor = [
        '$q',
        '$rootScope',
        '$window',
        function ($q, $rootScope, $window) {
            var re = /\/[^/]+\/([^/]+)\/answer\/$/;
            return {
                'request': function (config) {
                    if (re.test(config.url)) {
                        var match = re.exec(config.url);
                        var taskIdFull = match[1];
                        var parts = taskIdFull.split('.');
                        var docId = parseInt(parts[0], 10);
                        var taskName = parts[1];
                        var taskId = docId + '.' + taskName;
                        if (taskName !== '') {
                            var ab = angular.element("answerbrowser[task-id='" + taskId + "']");
                            if (ab.isolateScope()) {
                                var browserScope = ab.isolateScope();
                                angular.extend(config.data, {abData: browserScope.getBrowserData()});
                            }
                        }
                        var par = angular.element(escapeId(taskIdFull)).parents('.par');
                        angular.extend(config.data, {ref_from: {docId: $window.item.id, par: par.attr('id')}});
                    }
                    return config;
                },
                'response': function (response) {
                    if (re.test(response.config.url)) {
                        var match = re.exec(response.config.url);
                        var taskIdFull = match[1];
                        var parts = taskIdFull.split('.');
                        var docId = parseInt(parts[0], 10);
                        var taskName = parts[1];
                        var taskId = docId + '.' + taskName;
                        $rootScope.$broadcast('answerSaved', {
                            taskId: taskId,
                            savedNew: response.data.savedNew,
                            error: response.data.error
                        });
                    }
                    return response;
                },
                'responseError': function (response) {
                    if (re.test(response.config.url)) {
                        var match = re.exec(response.config.url);
                        var taskIdFull = match[1];
                        var parts = taskIdFull.split('.');
                        var docId = parseInt(parts[0], 10);
                        var taskName = parts[1];
                        var taskId = docId + '.' + taskName;
                        $rootScope.$broadcast('answerSaved', {
                            taskId: taskId,
                            savedNew: false,
                            error: response.data.error
                        });
                    }
                    return $q.reject(response);
                }
            };
        }
    ];
    $httpProvider.interceptors.push(interceptor);
}]);
