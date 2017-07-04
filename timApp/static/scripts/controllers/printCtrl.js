/**
 * Created by miimatku on 27.3.2017.
 */

var angular, $;
var timApp = angular.module('timApp');

timApp.controller("PrintCtrl", ['$scope', "$http", "$window", 'Users', '$log', '$uibModal', 'document', '$uibModalInstance', '$location', 'templates',

    function ($scope, $http, $window, $uibModal, Users, $log, document, $uibModalInstance, $location, templates) {
        $scope.dismissModal = function() {
            $uibModalInstance.dismiss();
        };

        $scope.document = document;
        // console.log($scope.document);
        $scope.templatesObject = angular.fromJson(templates);
        // console.log($scope.templatesObject);
        $scope.defaultTemplates = $scope.templatesObject.defaultTemplates;
        $scope.userTemplates = $scope.templatesObject.userTemplates;
        $scope.errormsg = null;
        $scope.notificationmsg = null;

        $scope.chosenTemplate = {
           // 'id' : null

           'id': $scope.userTemplates[0].doc_id // TODO: Check if exists. 
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
            if (chosenTemplateId) {
                $scope.notificationmsg = null;
                var requestURL = '/print/' + $scope.document.path + '?file_type=' + fileType + '&template_doc_id=' + chosenTemplateId;

                // console.log('request url: ' + requestURL);
                $http({
                    method: 'GET',
                    url: requestURL,
                    headers: {
                        'Cache-Control': 'no-cache'
                    }
                }).then(function success(response) {
                    $scope.errormsg = null;
                    // console.log(response);

                    // A nice way of doing this without accessing the resource a second time,
                    // but messes up the resource name which seems inconvenient
                    // TODO: investigate if the ObjcetURL could be the same one that is first accessed
                    // var file = new Blob([response.data], {type: 'text/plain'});
                    // var fileURL = URL.createObjectURL(file);
                    // window.open(fileURL);

                    // The dumber(?) way of doing the same thing.
                    $scope.openURLinNewTab(requestURL);

                }, function error(response) {
                    // Check if the error routes correctly.
                    // console.log(response.data.error);
                    $scope.errormsg = response.data.error;
                });
            } else {
                $scope.notificationmsg = "You need to choose a template first!";
            }
        };

        $scope.openURLinNewTab = function(url) {
            $window.open(url, '_blank');
        };

        $scope.create = function () {
            if ($scope.selected.name === 'PDF'){
                $scope.getPrintedDocument('pdf');
            }
            else if ($scope.selected.name === 'LaTeX'){
                $scope.getPrintedDocument('latex');
            }
        }

        /*
        $scope.createLatex = function() {
            $scope.getPrintedDocument('latex');
        };

        $scope.createPdf = function() {
            $scope.getPrintedDocument('pdf');
        };
        */

        $scope.cancel = function () {
            $uibModalInstance.dismiss('cancel');
        };

    }
])
;
