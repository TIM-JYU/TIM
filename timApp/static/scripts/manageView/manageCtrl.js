var PermApp = angular.module('permApp', [ 'ngSanitize', 'angularFileUpload']);

PermApp.directive('focusMe', function($timeout) {
    return {
        link : function(scope, element, attrs) {
            scope.$watch(attrs.focusMe, function(value) {
                if (value === true) {
                    // $timeout(function() {
                    element[0].focus();
                    element[0].select();
                    scope[attrs.focusMe] = false;
                    // });
                }
            });
        }
    };
});

PermApp.controller("PermCtrl", [
        '$scope',
        '$http',
        '$upload',
        function(sc, $http, $upload) {
            sc.editors = editors;
            sc.viewers = viewers;
            sc.doc = doc;
            sc.newName = doc.name;

            sc.removeConfirm = function(group, type) {
                if (confirm("Are you sure you want to remove this usergroup?")) {
                    sc.removePermission(group, type);
                }
            };

            sc.getPermissions = function() {
                $http.get('/getPermissions/' + sc.doc.id).success(function(data, status, headers, config) {
                    sc.editors = data.editors;
                    sc.viewers = data.viewers;
                }).error(function(data, status, headers, config) {
                    alert("Could not fetch permissions.");
                });
            };

            sc.removePermission = function(group, type) {
                $http.put('/removePermission/' + sc.doc.id + '/' + group.UserGroup_id + '/' + type).success(
                        function(data, status, headers, config) {
                            sc.getPermissions();
                        }).error(function(data, status, headers, config) {
                    alert(data.message);
                });
            };

            sc.addPermission = function(groupname, type) {
                $http.put('/addPermission/' + sc.doc.id + '/' + groupname + '/' + type).success(
                        function(data, status, headers, config) {
                            sc.getPermissions();
                        }).error(function(data, status, headers, config) {
                    alert(data.message);
                });
                sc.showAddEditor = false;
                sc.showAddViewer = false;
            };

            sc.renameDocument = function(newName) {
                $http.put('/rename/' + sc.doc.id, {
                    'new_name' : newName
                }).success(function(data, status, headers, config) {
                    sc.doc.name = newName;
                }).error(function(data, status, headers, config) {
                    alert(data.message);
                });
            };
            
            sc.deleteDocument = function(doc) {
                if (confirm('Are you sure you want to delete this document?')) {
                    $http.delete('/documents/' + doc)
                    .success(function(data, status, headers, config) {
                        location.replace('/');
                    }).error(function(data, status, headers, config) {
                        alert(data.message);
                    });
                }
            };
            
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
                        sc.uploadInProgress = false;
                    }).error(function(data, status, headers, config) {
                        sc.progress = 'Error occurred: ' + data.message;
                        sc.uploadInProgress = false;
                    });
                    // 
                }
            };

            sc.updateDocument = function(doc, $files) {
                sc.onFileSelect('/update/' + doc.id + '/' + doc.versions[0].hash, $files);
            };

        } ]);