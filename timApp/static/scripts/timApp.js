var modules = [];
var timApp = angular.module('timApp', ['ngSanitize', 'angularFileUpload']);

timApp.filter('escape', function () {
    "use strict";
    return function (str) {
        return encodeURIComponent(str).replace(/%2F/g, '/');
    };
});

var angular, folder, crumbs, groups;
timApp.controller("IndexCtrl", [ '$scope', '$controller', '$http', '$q', '$upload', '$window',

function(sc, controller, http, q, $upload, $window) {
    sc.endsWith = function(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    };

    sc.getParameterByName = function(name) {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec($window.location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    };

    sc.getAbsolutePath = function(name) {
        // startsWith and endsWith are ES6 features and do not work on IE
        if (name.lastIndexOf('/', 0) === 0)
            return name.substr(1);

        if (sc.folder === '')
            return name;

        if (sc.endsWith(sc.folder, '/'))
            return sc.folder + name;

        return sc.folder + '/' + name;
    };

    sc.createDocument = function(name) {
        http.post('/createDocument', {
            "doc_name" : sc.getAbsolutePath(name)
        }).success(function(data, status, headers, config) {
            $window.location.href = "/view/" + data.name;
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.createFolder = function(name, owner) {
        http.post('/createFolder', {
            "name" : sc.getAbsolutePath(name),
            "owner" : owner
        }).success(function(data, status, headers, config) {
            $window.window.location.href = "/view/" + data.name;
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
    };

    sc.initFolderVars = function() {
       sc.folder = folder;

       if (sc.getParameterByName('folder') !== '')
           sc.folder = sc.getParameterByName('folder');

       if (sc.folder === '' || sc.folder === undefined || sc.folder === null)
           sc.parentfolder = null;
       else
           sc.parentfolder = sc.folder.substr(0, sc.folder.lastIndexOf('/'));
    };

    sc.getFolders = function() {
       http({
            method : 'GET',
            url : '/getFolders',
            params: {
                root_path: sc.folder,
                _: Date.now()
            }
        }).success(function(data, status, headers, config) {
            sc.folderList = data;
            sc.displayIndex++;
        }).error(function(data, status, headers, config) {
            sc.folderList = [];
            // TODO: Show some error message.
        });
    };

    sc.getDocs = function() {
       http({
            method : 'GET',
            url : '/getDocuments',
            params: {
                versions: 0,
                folder: sc.folder,
                _: Date.now()
            }
        }).success(function(data, status, headers, config) {
            sc.documentList = data;
            sc.displayIndex++;
        }).error(function(data, status, headers, config) {
            sc.documentList = [];
            // TODO: Show some error message.
        });
    };

    sc.crumbs = crumbs;
    sc.userGroups = groups;
	sc.parentfolder = "";
    sc.initFolderVars();
    sc.folderList = [];
    sc.documentList = [];
    sc.getFolders();
    sc.getDocs();
    sc.displayIndex = 0;
    sc.m = {};

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
                fields : {'folder' : sc.folder}
            }).progress(function(evt) {
                sc.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
            }).success(function(data, status, headers, config) {
                sc.uploadedFile = '/images/' + data.file;
                sc.progress = 'Uploading... Done!';
                sc.getDocs();
                sc.uploadInProgress = false;
            }).error(function(data, status, headers, config) {
                sc.progress = 'Error: ' + data.error;
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
