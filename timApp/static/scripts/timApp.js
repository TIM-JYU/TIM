var timApp = angular.module('timApp');

// Filter to make string URL friendly
timApp.filter('escape', function () {
    "use strict";
    return function (str) {
        return encodeURIComponent(str).replace(/%2F/g, '/');
    };
});

// Controller used in document index and folders
var angular, folder, crumbs, groups;
timApp.controller("IndexCtrl", [ '$scope', '$controller', '$http', '$q', 'Upload', '$window', '$timeout',

function(sc, controller, http, q, Upload, $window, $timeout) {
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

    sc.onFileSelect = function(file) {
        sc.file = file;
        if (file) {
            sc.file.progress = 0;
            file.upload = Upload.upload({
                url: '/upload/',
                data: {
                    file: file,
                    folder: sc.folder
                }
            });

            file.upload.then(function (response) {
                $timeout(function () {
                    sc.getDocs();
                });
            }, function (response) {
                if (response.status > 0)
                    sc.file.progress = 'Error occurred: ' + response.data.error;
            }, function (evt) {
                sc.file.progress = Math.min(100, parseInt(100.0 *
                    evt.loaded / evt.total));
            });

            file.upload.finally(function () {
                sc.uploadInProgress = false;
            })
        }
    };

    sc.showUploadFn = function() {
        sc.showUpload = true;
        sc.file = null;
    };

} ]);
