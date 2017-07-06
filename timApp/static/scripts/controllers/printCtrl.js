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
        $scope.templatesObject = angular.fromJson(templates);
        $scope.defaultTemplates = $scope.templatesObject.defaultTemplates;
        $scope.userTemplates = $scope.templatesObject.userTemplates;
        $scope.errormsg = null;
        $scope.notificationmsg = null;
        $scope.createdUrl;
        $scope.loading = false;

        $scope.chosenTemplate = initTemplate();
        function initTemplate() {
            if ($scope.$storage.id) {
                var tmp = { 'id' : $scope.$storage.id };
                return tmp;
            }
            else if ($scope.defaultTemplates[0] && $scope.defaultTemplates[0].doc_id) {
                var tmp = { 'id' : $scope.defaultTemplates[0].doc_id}
                return tmp;   
            }  
            else if ($scope.userTemplates[0] && $scope.userTemplates[0].doc_id) {
                var tmp = { 'id' : $scope.userTemplates[0].doc_id}
                return tmp;
            }
            else {
                var tmp =  {'id' : null }
                return tmp;
            }            
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
                    $scope.openURLinNewTab(requestURL);
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
        }

        $scope.cancel = function () {
            $uibModalInstance.dismiss('cancel');
        };

    }
])
;
