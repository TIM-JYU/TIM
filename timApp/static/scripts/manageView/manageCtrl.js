var PermApp = angular.module('permApp', ['ngSanitize', 'angularFileUpload']);

PermApp.directive('focusMe', function ($timeout) {
    return {
        link: function (scope, element, attrs) {
            scope.$watch(attrs.focusMe, function (value) {
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
    '$window',
    function (sc, $http, $upload, $window) {
        $http.defaults.headers.common.Version = function() {
            return sc.doc.versions[0].hash;
        };

        sc.getJustDocName = function(fullName) {
            i = fullName.lastIndexOf('/');
            return i < 0 ? fullName : fullName.substr(i + 1);
        };

        sc.getFolderName = function(fullName) {
            i = fullName.lastIndexOf('/');
            return i < 0 ? '' : doc.name.substring(0, doc.name.lastIndexOf('/'));
        };

        sc.changeOwner = function() {
            sc.ownerUpdating = true;
            $http.put('/changeOwner/' + sc.doc.id + '/' + sc.doc.owner.id).success(
                function (data, status, headers, config) {

                }).error(function (data, status, headers, config) {
                    alert(data.error);
                }).then(function () {
                    sc.ownerUpdating = false;
                });
        };

        sc.removeConfirm = function (group, type) {
            if (confirm("Are you sure you want to remove this usergroup?")) {
                sc.removePermission(group, type);
            }
        };

        sc.getPermissions = function () {
            $http.get('/getPermissions/' + sc.doc.id).success(function (data, status, headers, config) {
                sc.editors = data.editors;
                sc.viewers = data.viewers;
            }).error(function (data, status, headers, config) {
                alert("Could not fetch permissions.");
            });
        };

        sc.removePermission = function (group, type) {
            $http.put('/removePermission/' + sc.doc.id + '/' + group.UserGroup_id + '/' + type).success(
                function (data, status, headers, config) {
                    sc.getPermissions();
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                });
        };

        sc.addPermission = function (groupname, type) {
            $http.put('/addPermission/' + sc.doc.id + '/' + groupname + '/' + type).success(
                function (data, status, headers, config) {
                    sc.getPermissions();
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                });
            sc.showAddEditor = false;
            sc.showAddViewer = false;
        };

        sc.renameDocument = function (newName) {
            $http.put('/rename/' + sc.doc.id, {
                'new_name': sc.oldFolderName + '/' + newName
            }).success(function (data, status, headers, config) {
                sc.doc.name = sc.oldFolderName + '/' + newName;
                sc.oldName = newName;
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.moveDocument = function (newLocation) {
            $http.put('/rename/' + sc.doc.id, {
                'new_name': newLocation + '/' + sc.oldName
            }).success(function (data, status, headers, config) {
                sc.doc.name = newLocation + '/' + sc.oldName;
                sc.oldFolderName = newLocation;
            }).error(function (data, status, headers, config) {
                alert(data.message);
            });
        };

        sc.deleteDocument = function (doc) {
            if (confirm('Are you sure you want to delete this document?')) {
                $http.delete('/documents/' + doc)
                    .success(function (data, status, headers, config) {
                        location.replace('/');
                    }).error(function (data, status, headers, config) {
                        alert(data.message);
                    });
            }
        };

        sc.deleteFolder = function (folder) {
            if (confirm('Are you sure you want to delete this folder?')) {
                $http.delete('/folders/' + folder)
                    .success(function (data, status, headers, config) {
                        location.replace('/');
                    }).error(function (data, status, headers, config) {
                        alert(data.message);
                    });
            }
        };

        sc.onFileSelect = function (url, $files) {
            // $files: an array of files selected, each file has name, size,
            // and type.
            sc.progress = 'Uploading... ';
            sc.uploadInProgress = true;
            for (var i = 0; i < $files.length; i++) {
                var file = $files[i];
                sc.upload = $upload.upload({
                    url: url,
                    method: 'POST',
                    file: file
                }).progress(function (evt) {
                    sc.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                }).success(function (data, status, headers, config) {
                    sc.doc.versions = data;
                    $http.get('/download/' + sc.doc.id).success(function (data) {
                        sc.doc.fulltext = data;
                        sc.fulltext = data;
                        sc.progress = 'Uploading... Done!';
                    })
                }).error(function (data, status, headers, config) {
                    sc.progress = 'Error occurred: ' + data.error;
                }).then(function () {
                    sc.uploadInProgress = false;
                });
            }
        };

        sc.updateDocument = function (doc, $files) {
            sc.onFileSelect('/update/' + doc.id + '/' + doc.versions[0].hash, $files);
        };

        sc.saveDocument = function (doc) {
            sc.saving = true;
            $http.post('/update/' + doc.id + '/' + doc.versions[0].hash, {'fulltext': sc.fulltext}).success(
                function (data, status, headers, config) {
                    sc.doc.fulltext = sc.fulltext;
                    sc.doc.versions = data;
                }).error(function (data, status, headers, config) {
                    alert(data.error);
                }).then(function () {
                    sc.saving = false;
                });
        };

        sc.markAllAsRead = function() {
            sc.readUpdating = true;
            $http.put('/read/' + sc.doc.id + '?_=' + Date.now())
                .success(function (data, status, headers, config) {

                }).error(function (data, status, headers, config) {
                    $window.alert('Could not mark the document as read.');
                }).finally(function (data, status, headers, config) {
                    sc.readUpdating = false;
                });
        };

        sc.editors = editors;
        sc.viewers = viewers;
        sc.userGroups = groups;
        sc.doc = doc;
        sc.crumbs = crumbs;
        sc.isFolder = isFolder;
        sc.newName = sc.getJustDocName(doc.name);
        sc.newFolderName = sc.getFolderName(doc.name);
        sc.oldName = sc.newName;
        sc.oldFolderName = sc.newFolderName;
        doc.fulltext = doc.fulltext.trim();
        sc.fulltext = doc.fulltext;

    }]);