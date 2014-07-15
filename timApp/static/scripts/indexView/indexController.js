var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("IndexCtrl", [
        '$scope',
        '$http',
        '$q',
        function(sc, http, q) {
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
        } ]);
