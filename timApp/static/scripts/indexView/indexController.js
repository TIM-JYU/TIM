var TimCtrl = angular.module('controller', []);

TimCtrl.controller("IndexCtrl", [ '$scope', '$controller', '$http', '$q', '$upload',

function(sc, controller, http, q, $upload) {
    sc.getParameterByName = function(name) {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec(location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    };

    sc.getAbsolutePath = function(name) {
        if (name.startsWith('/'))
            return name.substr(1)

        if (sc.folder == '')
            return name

        if (sc.folder.endsWith('/'))
            return sc.folder + name;

        return sc.folder + '/' + name;
    };

    sc.createDocument = function(name) {
        http.post('/createDocument', {
            "doc_name" : sc.getAbsolutePath(name)
        }).success(function(data, status, headers, config) {
            window.location.href = "/view/" + data.name;
        }).error(function(data, status, headers, config) {
            alert(data.error);
        });
    };

    sc.createFolder = function(name, owner) {
        http.post('/createFolder', {
            "name" : sc.getAbsolutePath(name),
            "owner" : owner
        }).success(function(data, status, headers, config) {
            window.location.href = "/view/" + data.name;
        }).error(function(data, status, headers, config) {
            alert(data.error);
        });
    };

    sc.initFolderVars = function() {
       sc.folder = folder;

       if (sc.getParameterByName('folder') != '')
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
            params: {root_path: sc.folder}
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
            params: {versions: 0, folder: sc.folder}
        }).success(function(data, status, headers, config) {
            sc.documentList = data;
            sc.displayIndex++;
        }).error(function(data, status, headers, config) {
            sc.documentList = [];
            // TODO: Show some error message.
        });
    };

    sc.userGroups = groups;
	sc.parentfolder = "";
    sc.initFolderVars();
    sc.folderList = [];
    sc.documentList = [];
    sc.getFolders();
    sc.getDocs();
    sc.displayIndex = 0;
    sc.displayTimes = false;
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
