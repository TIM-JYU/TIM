import {IController} from "angular";
import {timApp} from "tim/app";
import {IFolder, IItem} from "../item/IItem";
import {Users} from "../user/userService";
import {folderglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {to} from "../util/utils";

// Controller used in document index and folders

class IndexCtrl implements IController {
    private itemList: IItem[];
    private item: IFolder;
    private showUpload: boolean = false;
    private canCreate: boolean;
    private showId: boolean = false;

    constructor() {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isLoggedIn();
    }

    $onInit() {}

    async getItems() {
        const r = await to(
            $http<IItem[]>({
                method: "GET",
                url: "/getItems",
                params: {
                    folder: this.item.location,
                },
            })
        );
        if (r.ok) {
            this.itemList = r.result.data;
        } else {
            this.itemList = [];
        }
    }

    listOwnerNames(i: IItem) {
        return i.owners.map((o) => o.name).join(", ");
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
        <th>Owners</th>
        <th>Rights</th>
        <th class="gray" ng-show="$ctrl.showId || true" ng-click="$ctrl.showId = !$ctrl.showId">Id</th>
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
        <td></td>
    </tr>
    <tr ng-repeat="item in ::$ctrl.itemList">
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
        <td>{{ ::item.modified }}</td>
        <td>{{ ::$ctrl.listOwnerNames(item) }}</td>
        <td>
            <a title="Edit" ng-show="item.rights.editable && !item.isFolder" href="/view/{{ item.path | escape }}"><i
                    class="glyphicon glyphicon-pencil"></i></a>
            <a title="Manage" ng-show="item.rights.manage" href="/manage/{{ item.path | escape }}"><i
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
</div>
    `,
});
