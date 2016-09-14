var timApp = angular.module('timApp');

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

    sc.copyDocument = function(docId, templateName) {
        http.post('/update/' + docId, {
            "template_name" : templateName
        }).success(function(data, status, headers, config) {
            $window.location.reload();
        }).error(function(data, status, headers, config) {
            $window.alert(data.error);
        });
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

    sc.getItems = function() {
       http({
            method : 'GET',
            url : '/getItems',
            params: {
                folder: sc.folder,
                _: Date.now()
            }
        }).success(function(data, status, headers, config) {
            sc.itemList = data;
        }).error(function(data, status, headers, config) {
            sc.itemList = [];
            // TODO: Show some error message.
        });
    };

    sc.getTemplates = function() {
       http({
            method : 'GET',
            url : '/getTemplates',
            params: {
                root_path: sc.folder,
                _: Date.now()
            }
        }).success(function(data, status, headers, config) {
            sc.templateList = data;
        }).error(function(data, status, headers, config) {
            // TODO: Show some error message.
        });
    };

    sc.crumbs = crumbs;
    sc.folderOwner = $window.current_user.name;
	sc.parentfolder = "";
    sc.initFolderVars();
    sc.itemList = $window.items;
    sc.templateList = [];
    sc.templates = $window.templates;
    if (sc.templates)
    {
        sc.getTemplates();
    }

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
                    sc.getItems();
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
