var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("IndexCtrl", [ '$scope', '$http', '$q', function(sc, http, q) {
    sc.getDocIds = function() {
        http( {
            method : 'GET',
            url : '/getDocuments/'
        }).success(function(data, status, headers, config) {
            sc.documentList = data;
            sc.displayIndex = true;
        }).error(function(data, status, headers, config) {
            sc.documentList = [];
            //TODO: Show some error message.
        });
    };
    sc.documentList = [];
    sc.getDocIds();
    sc.displayIndex = false;
} ]);
