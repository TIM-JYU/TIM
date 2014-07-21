var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("IndexCtrl", [
        '$scope',
        '$http',
        '$q',
        'fileUpload',
        function(sc, http, q, fileUpload) {
            sc.createDocument = function(name) {
                http.post('/createDocument', {
                    "doc_name" : name
                }).success(function(data, status, headers, config) {
                    window.location.href = "/documents/" + data.id;
                }).error(function(data, status, headers, config) {
                    alert("Could not create the document.");
                });
            };

            sc.getDocIds = function() {
                http( {
                    method : 'GET',
                    url : '/getDocuments/'
                }).success(function(data, status, headers, config) {
                    sc.documentList = data;
                    sc.displayIndex = true;
                }).error(function(data, status, headers, config) {
                    sc.documentList = [];
                    // TODO: Show some error message.
                    });
            };

            sc.documentList = [];
            sc.getDocIds();
            sc.displayIndex = false;
            sc.myFile;
            sc.uploadFile = function(){
                var file = sc.myFile;
                var uploadUrl = '/upload/';
                fileUpload.uploadFileToUrl(file, uploadUrl);
            };

        }]);



TimCtrl.directive('fileModel', ['$parse', function ($parse) {
                return {
                       restrict: 'A',
                       link: function(scope, element, attrs) {
                                 var model = $parse(attrs.fileModel);
                                 var modelSetter = model.assign;
                
                                 element.bind('change', function(){
                                    scope.$apply(function(){
                                        modelSetter(scope, element[0].files[0]);
                                   });
                                 });
        }
        };
}]);

TimCtrl.service('fileUpload', ['$http', function ($http) {
       this.uploadFileToUrl = function(file, uploadUrl){
                var fd = new FormData();
              fd.append('file', file);
                $http.post(uploadUrl, fd, {
                    transformRequest: angular.identity,
                     headers: {'Content-Type': undefined}
             })
             .success(function(){
            })
           .error(function(){
            });
     }
}]); 
