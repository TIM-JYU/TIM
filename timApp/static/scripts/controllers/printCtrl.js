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

        $scope.storage = $localStorage.$default({
            timPrintingTemplateId: null
        });

        $scope.options = $scope.storage.id;
        
        $scope.document = document;
        $scope.templates = angular.fromJson(templates);
        $scope.errormsg = null;
        $scope.notificationmsg = null;
        $scope.createdUrl = null;
        $scope.loading = false;
        $scope.showPaths = true;

        $scope.selectedTemplate = initTemplate();

        function initTemplate() {
            var id = null;

            if ($scope.storage.timPrintingTemplateId && $scope.templates) {

                angular.forEach($scope.templates, function(template, key) {
                    if (template.id === $scope.storage.timPrintingTemplateId) {
                        id = template.id
                    }
                });

            }

            return { 'id': id };
        }

        $scope.selected = {
            name: 'PDF'
        };


        $scope.getPrintedDocument = function(fileType) {
            if (fileType !== 'latex' && fileType !== 'pdf') {
                console.log("The filetype '" + fileType + "' is not valid");
                return; //TODO: the error should do something visible
                // TODO: also kind of pointless as the filetype comes from the predefined functions
            }
            var chosenTemplateId = $scope.selectedTemplate.id;
            $scope.storage.timPrintingTemplateId = chosenTemplateId;

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
