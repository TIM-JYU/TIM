import {timApp} from "tim/app";
import {$http, $timeout, $upload, $window} from "../ngimport";

// Controller used in document index and folders

class IndexCtrl {
    private user: any;
    private folderOwner: string;
    private parentfolder: string;
    private itemList: any[];
    private item: any;
    private templateList: any[];
    private templates: any[];
    private showUpload: boolean;
    private file: any;

    constructor() {
        this.user = $window.current_user;
        this.folderOwner = $window.current_user.name;
        this.parentfolder = "";
        this.itemList = $window.items;
        this.item = $window.item;
        this.templateList = [];
        this.templates = $window.templates;
        if (this.templates) {
            this.getTemplates();
        }
    }

    endsWith(str, suffix) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }

    getParameterByName(name) {
        name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
        const regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
            results = regex.exec($window.location.search);
        return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
    }

    copyDocument(docId, templateName) {
        $http.post("/update/" + docId, {
            template_name: templateName,
        }).then(function(response) {
            $window.location.reload();
        }, function(response) {
            $window.alert(response.data.error);
        });
    }

    getItems() {
        $http({
            method: "GET",
            url: "/getItems",
            params: {
                folder: this.item.location,
            },
        }).then(function(response) {
            this.itemList = response.data;
        }, function(response) {
            this.itemList = [];
            // TODO: Show some error message.
        });
    }

    getTemplates() {
        $http({
            method: "GET",
            url: "/getTemplates",
            params: {
                item_path: this.item.path,
            },
        }).then(function(response) {
            this.templateList = response.data;
        }, function(response) {
            // TODO: Show some error message.
        });
    }

    onFileSelect(file) {
        this.file = file;
        if (file) {
            this.file.progress = 0;
            file.upload = $upload.upload({
                url: "/upload/",
                data: {
                    file,
                    folder: this.item.location,
                },
                method: "POST",
            });

            file.upload.then(function(response) {
                $timeout(function() {
                    this.getItems();
                });
            }, function(response) {
                if (response.status > 0)
                    this.file.progress = "Error occurred: " + response.data.error;
            }, function(evt) {
                this.file.progress = Math.min(100, Math.floor(100.0 *
                    evt.loaded / evt.total));
            });

            file.upload.finally(function() {
                this.uploadInProgress = false;
            });
        }
    }

    showUploadFnfunction() {
        this.showUpload = true;
        this.file = null;
    }
}

timApp.controller("IndexCtrl", IndexCtrl);
