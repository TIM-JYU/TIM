var TimCtrl = angular.module('controller', []);

TimCtrl.controller("IndexCtrl", [ '$scope', '$controller', '$http', '$q', '$upload',

function(sc, controller, http, q, $upload) {

    sc.createDocument = function(name) {
        http.post('/createDocument', {
            "doc_name" : name
        }).success(function(data, status, headers, config) {
            window.location.href = "/edit/" + data.id;
        }).error(function(data, status, headers, config) {
            alert(data.message);
        });
    };

    sc.getDocs = function() {
        http({
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
    sc.getDocs();
    sc.displayIndex = false;
    sc.m = {};

    sc.uploadedFile;
    sc.progress;
    sc.uploadInProgress;
    sc.selectedFile = "";
    sc.onFileSelect = function(url, $files) {
        // $files: an array of files selected, each file has name, size,
        // and type.
        sc.progress = 'Uploading... ';
        sc.uploadInProgress = true;
        for (var i = 0; i < $files.length; i++) {
            var file = $files[i];
            sc.upload = $upload.upload({
                url : url,
                method : 'POST',
                file : file,
            }).progress(function(evt) {
                sc.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
            }).success(function(data, status, headers, config) {
                sc.uploadedFile = '/images/' + data.file;
                sc.progress = 'Uploading... Done!';
                sc.getDocs();
                sc.uploadInProgress = false;
            }).error(function(data, status, headers, config) {
                sc.progress = 'Error: ' + data.message;
                sc.uploadInProgress = false;
            });
            // 
        }
    };

    sc.updateDocument = function(doc, $files) {
        sc.onFileSelect('/update/' + doc.id + '/' + doc.versions[0].hash, $files);
    };

    sc.setCurrentDocument = function(doc) {
        sc.m.currDoc = doc;
        sc.progress = '';
    };

} ]);
