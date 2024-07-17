import {Component} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {folderglobals} from "tim/util/globals";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

const MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives/";
const TIM_MESSAGES_FOLDER_PREFIX = "messages/tim-messages";

enum AccessLevelBadge {
    NO_BADGE = -1,
    PUBLIC = 0,
    GROUP = 1,
    PRIVATE = 2,
}

@Component({
    selector: "tim-index",
    template: `
        <table class="table" *ngIf="itemList.length > 0 || item.path">
            <thead>
            <tr>
                <th></th>
                <th>Name</th>
                <th></th>
                <th>Last modified</th>
                <th>Owners</th>
                <th>Rights</th>
                <th class="gray" (click)="showId = !showId">Id</th>
            </tr>
            </thead>
            <tbody>
            <tr *ngIf="item.path">
                <td>
                    <a href="/view/{{ item.location }}">
                        <span class="glyphicon glyphicon-level-up" aria-hidden="true"></span>
                    </a>
                </td>
                <td><a href="/view/{{ item.location }}">Go to parent folder</a></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
                <td></td>
            </tr>
            <tr *ngFor="let item of itemList; index as i">
                <td>
                    <a *ngIf="item.isFolder" href="/view/{{ item.path }}">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                <td>
                    <a href="/view/{{ item.path }}">{{ item.title }}</a>&ngsp;
                    <ng-container [ngSwitch]="getItemBadge(i)">  
                        <a *ngSwitchCase="AccessLevelBadge.PUBLIC">
                            <!-- <i *ngIf="item.unpublished" class="glyphicon glyphicon-lock" title="Unpublished item"></i> -->
                            <i class="accessbadge-public" title="Item is visible to public groups (anonymous users, logged-in users or jyu.fi users.">Public</i>
                        </a>
                        <a *ngSwitchCase="AccessLevelBadge.PRIVATE">
                            <i class="accessbadge-private" title="Private item">Private</i>
                        </a>
                        <a *ngSwitchDefault></a>
                    </ng-container>
                </td>
                <td></td>
                <td>{{ item.modified }}</td>
                <td>{{ listOwnerNames(item) }}</td>
                <td>
                    <a title="Edit" *ngIf="item.rights.editable && !item.isFolder"
                       href="/view/{{ item.path }}"><i
                            class="glyphicon glyphicon-pencil"></i></a>
                    &ngsp;<a title="Manage" *ngIf="item.rights.manage" href="/manage/{{ item.path }}"><i
                            class="glyphicon glyphicon-cog"></i></a>
                    &ngsp;<a title="Teacher" *ngIf="item.rights.teacher && !item.isFolder"
                       href="/teacher/{{ item.path }}"><i class="glyphicon glyphicon-education"></i></a>
                </td>
                <td *ngIf="showId">
                    {{item.id}}
                </td>
            </tr>
            </tbody>
        </table>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
        <tabset *ngIf="canCreate">
            <tab [active]="false">
                        <ng-template tabHeading>
                            <span>Create a new document</span>
                            <span class="glyphicon glyphicon-file icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="document" itemLocation="{{ item.path }}"></create-item>
            </tab>
            <tab [active]="false">
                        <ng-template tabHeading>
                            <span>Create a new folder</span>
                            <span class="glyphicon glyphicon-folder-open icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="folder" itemLocation="{{ item.path }}"></create-item>
            </tab>
        </tabset>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryListComponent {
    itemList: DocumentOrFolder[];
    itemBadges: AccessLevelBadge[];
    item: IFolder;
    canCreate: boolean;
    showId = false;

    constructor() {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isRealUser();

        // TODO: Allow to sort all columns instead
        if (
            this.item.path.startsWith(MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX) ||
            this.item.path.startsWith(TIM_MESSAGES_FOLDER_PREFIX)
        ) {
            this.itemList = this.itemList.sort((a, b) => b.id - a.id);
        }
        this.itemBadges = [this.itemList.length];
        for (let i = 0; i < this.itemList.length; i++) {
            this.getAccessLevelBadge(this.itemList[i]).then(
                (value) => (this.itemBadges[i] = value)
            );
        }
    }

    listOwnerNames(i: IItem) {
        return i.owners.map((o) => o.name).join(", ");
    }

    async getAccessLevelBadge(i: IItem): Promise<AccessLevelBadge> {
        const res = await to($http.get<number[]>(`/items/accesses/${i.id}`));
        let ugids: number[] = [];
        if (res.ok) {
            ugids = res.result.data;
        }

        // TODO: clear scheme for badges, ie.
        //  - what badges are needed
        //  - how do badges map to different access combinations
        // Logged-in users = 0, Anon users = 1, jyu.fi users = 5
        if (ugids.includes(0) || ugids.includes(1) || ugids.includes(5)) {
            return AccessLevelBadge.PUBLIC;
        } else if (ugids.length > 1 && i.owners.length != ugids.length) {
            // we currently don't use the 'Group' badge, since we would have to do db queries to distinguish
            // personal groups from actual groups
            // return AccessLevelBadge.PUBLIC;
            return AccessLevelBadge.NO_BADGE;
        } else {
            return AccessLevelBadge.PRIVATE;
        }
    }

    getItemBadge(index: number) {
        return this.itemBadges[index];
    }

    protected readonly AccessLevelBadge = AccessLevelBadge;
}
