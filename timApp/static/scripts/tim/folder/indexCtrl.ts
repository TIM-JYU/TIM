import {IController} from "angular";
import {timApp} from "tim/app";
import {IFolder, IItem} from "../item/IItem";
import {IUser} from "../user/IUser";
import {Users} from "../user/userService";
import {folderglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {to} from "../util/utils";

// Controller used in document index and folders

class IndexCtrl implements IController {
    private user: IUser;
    private itemList: IItem[];
    private item: IFolder;
    private showUpload: boolean = false;
    private canCreate: boolean;
    private uploadInProgress: boolean = false;
    private showId: boolean = false;

    constructor() {
        const fg = folderglobals();
        this.user = fg.current_user;
        this.itemList = folderglobals().items;
        this.item = fg.item;
        this.canCreate = Users.isLoggedIn();
    }

    $onInit() {

    }

    endsWith(str: string, suffix: string) {
        return str.indexOf(suffix, str.length - suffix.length) !== -1;
    }

    async getItems() {
        const r = await to($http<IItem[]>({
            method: "GET",
            url: "/getItems",
            params: {
                folder: this.item.location,
            },
        }));
        if (r.ok) {
            this.itemList = r.result.data;
        } else {
            this.itemList = [];
        }
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
        <td class="gray" ng-show="$ctrl.showId || true" ng-click="$ctrl.showId = !$ctrl.showId">Id</td>
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
        <td ng-show="$ctrl.showId">
          {{item.id}}
        </td>
    </tr>
</table>
<p ng-show="$ctrl.itemList.length == 0">There are no items to show.</p>

<uib-tabset ng-if="$ctrl.canCreate" active="-1">
    <uib-tab heading="Create a new document">
        <create-item item-type="document" item-location="{{ $ctrl.item.path }}"></create-item>
    </uib-tab>
    <uib-tab heading="Create a new folder">
        <create-item item-type="folder" item-location="{{ $ctrl.item.path }}"></create-item>
    </uib-tab>
</uib-tabset>
<div ng-if="!$ctrl.canCreate">
</div>
<div>
<!--<label>Show id's <input type="checkbox" ng-model="$ctrl.showId"></label> -->
</div>
    `,
});
