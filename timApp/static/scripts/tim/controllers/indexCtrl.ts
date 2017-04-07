import {timApp} from "tim/app";

// Controller used in document index and folders
timApp.controller("IndexCtrl", [ '$scope', '$controller', '$http', '$q', 'Upload', '$window', '$timeout',

function(sc, controller, http, q, Upload, $window, $timeout) {
    "use strict";
    sc.endsWith = function(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    };

    sc.getParameterByName = function(name) {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec($window.location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
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

    sc.getItems = function() {
       http({
            method : 'GET',
            url : '/getItems',
            params: {
                folder: sc.item.location
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
                item_path: sc.item.path
            }
        }).success(function(data, status, headers, config) {
            sc.templateList = data;
        }).error(function(data, status, headers, config) {
            // TODO: Show some error message.
        });
    };

    sc.user = $window.current_user;
    sc.folderOwner = $window.current_user.name;
	sc.parentfolder = "";
    sc.itemList = $window.items;
    sc.item = $window.item;
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
                    folder: sc.item.location
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
                sc.file.progress = Math.min(100, Math.floor(100.0 *
                    evt.loaded / evt.total));
            });

            file.upload.finally(function () {
                sc.uploadInProgress = false;
            });
        }
    };

    sc.showUploadFn = function() {
        sc.showUpload = true;
        sc.file = null;
    };

} ]);
