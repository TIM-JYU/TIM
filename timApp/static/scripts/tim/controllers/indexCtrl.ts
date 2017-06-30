import {timApp} from "tim/app";
import {$http, $timeout, $upload, $window} from "../ngimport";
import {Users} from "../services/userService";

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
    private canCreate: boolean;

    constructor() {
        this.user = $window.current_user;
        this.folderOwner = $window.current_user.name;
        this.parentfolder = "";
        this.itemList = $window.items;
        this.item = $window.item;
        this.templateList = [];
        this.templates = $window.templates;
        this.canCreate = Users.isLoggedIn();
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

timApp.component("timIndex", {
    controller: IndexCtrl,
    template: `<table class="table" ng-show="$ctrl.itemList.length > 0 || $ctrl.item.path">
    <thead>
    <tr>
        <th></th>
        <th>Name</th>
        <th></th>
        <th>Last modified</th>
        <th>Owner</th>
        <th>Rights</th>
    </tr>
    </thead>
    <tr ng-show="$ctrl.item.path">
        <td>
            <a href="/view/{{ $ctrl.item.location | escape }}">
                <span class="glyphicon glyphicon-level-up" aria-hidden="true"></span>
            </a>
        </td>
        <td><a href="/view/{{ $ctrl.item.location | escape }}">Go to parent folder</a></td>
        <td></td>
        <td></td>
        <td></td>
        <td></td>
    </tr>
    <tr ng-repeat="item in $ctrl.itemList">
        <td>
            <a ng-show="item.isFolder" href="/view/{{ item.path | escape }}">
                <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
            </a>
        </td>
        <td>
            <a href="/view/{{ item.path | escape }}">{{ item.title }}</a>
            <a><i ng-show="item.unpublished" class="glyphicon glyphicon-lock" title="Unpublished item"></i></a>
        </td>
        <td></td>
        <td>{{ item.modified }}</td>
        <td>{{ item.owner.name }}</td>
        <td>
            <a title="Edit" ng-show="item.rights.editable && !item.isFolder" href="/view/{{ item.id }}"><i
                    class="glyphicon glyphicon-pencil"></i></a>
            <a title="Manage" ng-show="item.rights.manage" href="/manage/{{ item.id }}"><i
                    class="glyphicon glyphicon-cog"></i></a>
            <a title="Teacher" ng-show="item.rights.teacher && !item.isFolder"
               href="/teacher/{{ item.path | escape }}"><i class="glyphicon glyphicon-education"></i></a>
        </td>
    </tr>
</table>
<p ng-show="$ctrl.itemList.length == 0">There are no items to show.</p>

<uib-tabset ng-if="$ctrl.canCreate" active="-1">
    <!--
    <uib-tab heading="Upload a new document">
        <div class="form-group">
            <label for="docUpload">Select a file:</label>
            <input id="docUpload" class="form-control" type="file" ngf-select="$ctrl.onFileSelect($file)">
        </div>
     <span ng-show="$ctrl.file.progress >= 0"
           ng-bind="$ctrl.file.progress < 100 ? 'Uploading... ' + $ctrl.file.progress + '%' : 'Done!'">
     </span>
    </uib-tab>
    -->
    <uib-tab heading="Create a new document">
        <create-item item-type="document" item-location="{{ $ctrl.item.path }}"></create-item>
    </uib-tab>
    <uib-tab heading="Create a new folder">
        <create-item item-type="folder" item-location="{{ $ctrl.item.path }}"></create-item>
    </uib-tab>
</uib-tabset>
<div ng-if="!$ctrl.canCreate">
</div>
    `,
});
