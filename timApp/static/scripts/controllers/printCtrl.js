/**
 * Created by miimatku on 27.3.2017.
 */

var angular, $;
var timApp = angular.module('timApp');

timApp.controller("PrintCtrl", ['$scope', "$http", "$window", 'Users', '$log', '$uibModal', 'document', '$uibModalInstance', '$location', 'templates', '$localStorage',

    function ($scope, $http, $window, $uibModal, Users, $log, document, $uibModalInstance, $location, templates, $localStorage) {
        $scope.dismissModal = function() {
            $uibModalInstance.dismiss();
        };
        $scope.$storage = $localStorage.$default({
            id: null
        });
        $scope.options = $scope.$storage.id;
        
        $scope.document = document;
        $scope.templates = angular.fromJson(templates);
        $scope.errormsg = null;
        $scope.notificationmsg = null;
        $scope.createdUrl = null;
        $scope.loading = false;
        $scope.showPaths = true;

        $scope.chosenTemplate = initTemplate();

        function initTemplate() {
            var id = null;

            if ($scope.$storage.id) {
                id = $scope.$storage.id;
            }
            else if ($scope.defaultTemplates[0] && $scope.defaultTemplates[0].doc_id) {
                id = $scope.defaultTemplates[0].doc_id;
            }  
            else if ($scope.userTemplates[0] && $scope.userTemplates[0].doc_id) {
                id = $scope.userTemplates[0].doc_id;
            }

            return { 'id': id };
        };

        $scope.selected = {
            name: 'PDF'
        };


        $scope.getPrintedDocument = function(fileType) {
            if (fileType !== 'latex' && fileType !== 'pdf') {
                console.log("The filetype '" + fileType + "' is not valid");
                return; //TODO: the error should do something visible
                // TODO: also kind of pointless as the filetype comes from the predefined functions
            }
            var chosenTemplateId = $scope.chosenTemplate.id;
            $scope.$storage.id = chosenTemplateId;

            if (chosenTemplateId) {
                $scope.notificationmsg = null;
                var requestURL = '/print/' + $scope.document.path + '?file_type=' + fileType + '&template_doc_id=' + chosenTemplateId;

                $http({
                    method: 'GET',
                    url: requestURL,
                    headers: {
                        'Cache-Control': 'no-cache'
                    }
                }).then(function success(response) {
                    $scope.errormsg = null;

                    // Uncomment this line to automatically open the created doc in a popup tab.
                    // $scope.openURLinNewTab(requestURL);

                    $scope.createdUrl = requestURL;

                    $scope.loading = false;

                }, function error(response) {
                    $scope.errormsg = response.data.error;
                    $scope.loading = false;
                });
            } else {
                $scope.notificationmsg = "You need to choose a template first!";
            }
        };

        $scope.openURLinNewTab = function(url) {
            $window.open(url, '_blank');
        };

        $scope.create = function () {
            $scope.loading = true;
            $scope.createdUrl = null;
            if ($scope.selected.name === 'PDF'){
                $scope.getPrintedDocument('pdf');
            }
            else if ($scope.selected.name === 'LaTeX'){
                $scope.getPrintedDocument('latex');
            }
        };

        $scope.cancel = function () {
            $uibModalInstance.dismiss('cancel');
        };

        $scope.formatPath = function (path) {
            return path.replace('Templates/Printing', '../..');
        };

    }
])
;
