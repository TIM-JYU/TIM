import angular from "angular";
import {timApp} from "../app";
import {$http, $window} from "../ngimport";

/**
 * Created by miimatku on 27.3.2017.
 */

timApp.controller("PrintCtrl", ['$scope', '$uibModal', 'document', '$uibModalInstance', '$location', 'templates', '$localStorage',

    function ($scope, $uibModal, document, $uibModalInstance, $location, templates, $localStorage) {
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
        $scope.docUrl = null;
        $scope.loading = false;
        $scope.showPaths = false;
        $scope.pluginsUserCode = false;

        $scope.selectedTemplate = initTemplate();

        function initTemplate() {
            var id = null;

            if ($scope.storage.timPrintingTemplateId && $scope.templates) {

                angular.forEach($scope.templates, function(template, key) {
                    if (template.id === $scope.storage.timPrintingTemplateId) {
                        id = template.id
                    }
                });

            } else if ($scope.templates) {
                if ($scope.templates.length > 0) {
                    id = templates[0].id
                }
            }

            return { 'id': id };
        }

        $scope.selected = {
            name: 'PDF'
        };


        $scope.getPrintedDocument = function(fileType) {
            $scope.errormsg = null;
            $scope.docUrl = null;

            /*
            if (fileType !== 'latex' && fileType !== 'pdf') {
                console.log("The filetype '" + fileType + "' is not valid");
                return; //TODO: the error should do something visible
                // TODO: also kind of pointless as the filetype comes from the predefined functions
            }
            */
            var chosenTemplateId = $scope.selectedTemplate.id;
            $scope.storage.timPrintingTemplateId = chosenTemplateId;

            var pluginsUserCode = $scope.pluginsUserCode;
            var removeOldImages = $scope.removeOldImages;
            var force = $scope.forceRefresh;

            if (chosenTemplateId) {
                $scope.notificationmsg = null;

                var postURL = '/print/' + $scope.document.path;
                var data = JSON.stringify({
                    'fileType' : fileType,
                    'templateDocId' : chosenTemplateId,
                    'printPluginsUserCode' : pluginsUserCode,
                    'removeOldImages' : removeOldImages,
                    'force' : force
                });
                $http.post<{url: string}>(postURL, data)
                    .then(function success(response) {
                        // console.log(response);

                        // Uncomment this line to automatically open the created doc in a popup tab.
                        // $scope.openURLinNewTab(requestURL);

                        // $scope.docUrl = '/print/' + $scope.document.path + '?file_type=' + fileType
                        //    + '&template_doc_id=' + chosenTemplateId + '&plugins_user_code=' + pluginsUserCode;
                        $scope.docUrl = response.data.url;
                        // console.log($scope.docUrl);

                        $scope.loading = false;

                    }, function error(response) {
                        var reformatted = response.data.error.split("\\n").join("<br/>");
                        $scope.errormsg = reformatted;
                        $scope.loading = false;
                    })
                ;
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
            $scope.getPrintedDocument($scope.selected.name.toLowerCase());
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
