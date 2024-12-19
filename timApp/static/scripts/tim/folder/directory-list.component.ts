import {Component} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem, ITag} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {folderglobals} from "tim/util/globals";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

const MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives/";
const TIM_MESSAGES_FOLDER_PREFIX = "messages/tim-messages";

enum AccessLevelBadge {
    NO_BADGE = -1,
    PUBLIC = 0,
    LOGGED_IN = 1,
    ORGANIZATION = 2, // Haka organizations
    LIMITED = 3, // custom groups, users, etc.
    PRIVATE = 4, // only owner(s)
}

const AccessLevelBadgeInfo: Record<AccessLevelBadge, string> = {
    [AccessLevelBadge.NO_BADGE]: "",
    [AccessLevelBadge.PUBLIC]: $localize`Item is visible publicly (ie. to everyone), including anonymous users.`,
    [AccessLevelBadge.LOGGED_IN]: $localize`Item is visible to logged-in users.`,
    [AccessLevelBadge.ORGANIZATION]: $localize`Item is visible to groups belonging to a Haka organization.`,
    [AccessLevelBadge.LIMITED]: $localize`Item is visible only to specific users, check the Manage-page for details.`,
    [AccessLevelBadge.PRIVATE]: $localize`Item is visible only to its owners.`,
};

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
            <tr *ngFor="let item of itemList">
                <td>
                    <a *ngIf="item.isFolder" href="/view/{{ item.path }}">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                
                <td>
                    <a href="/view/{{ item.path }}">{{ item.title }}</a>&ngsp;
                    <a><i class="accessbadge ab-{{ getItemBadgeName(item.id).toLowerCase() }}" 
                          title="{{ AccessLevelBadgeInfo[getItemBadge(item.id)] }}">{{ getItemBadgeName(item.id) }}</i>
                    </a>
                    <span class="itemtags">
                        <a *ngFor="let tag of getItemTags(item)">
                            <i class="itemtag tagtype-{{ getTagTypeString(tag) }}" 
                              title="{{ tag.name }} {{ tag.expires ? '(expires on ' + tag.expires + ')' : '' }}">{{ tag.name }}</i>
                        </a>
                    </span>
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
    itemBadges: Record<number, AccessLevelBadge>;
    itemTags: Record<number, ITag[]>;
    item: IFolder;
    canCreate: boolean;
    showId = false;

    constructor() {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isRealUser();
        this.itemBadges = {};
        this.itemTags = {};

        // TODO: Allow to sort all columns instead
        if (
            this.item.path.startsWith(MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX) ||
            this.item.path.startsWith(TIM_MESSAGES_FOLDER_PREFIX)
        ) {
            this.itemList = this.itemList.sort((a, b) => b.id - a.id);
        }

        // this.item is the current directory folder
        this.getAccessLevelBadges(this.item).then((value) => {
            this.itemBadges = value ?? {};
        });
        this.getFolderItemTags(this.item).then((value) => {
            this.itemTags = value ?? {};
        });
    }

    listOwnerNames(i: IItem) {
        return i.owners.map((o) => o.name).join(", ");
    }

    async getAccessLevelBadges(
        parentFolder: IItem
    ): Promise<Record<number, AccessLevelBadge>> {
        const res = await to(
            $http.get<Record<number, number[]>>(
                `/items/accesses_all/${parentFolder.id}`
            )
        );
        let item_accesses: Record<number, number[]>;
        const badges: Record<number, AccessLevelBadge> = {};
        if (res.ok) {
            item_accesses = res.result.data;

            let hakaOrgIds: number[] = [];
            this.getHakaOrgIds().then((value) => (hakaOrgIds = value));

            for (const item_id in item_accesses) {
                if (
                    Object.prototype.hasOwnProperty.call(item_accesses, item_id)
                ) {
                    // Logged-in users = 0, Anon users = 1, jyu.fi users = 5
                    const ids = item_accesses[item_id];

                    if (ids.includes(1)) {
                        badges[item_id] = AccessLevelBadge.PUBLIC;
                    } else if (ids.includes(0)) {
                        badges[item_id] = AccessLevelBadge.LOGGED_IN;
                    } else if (this.hasSomeHakaOrgAccess(ids, hakaOrgIds)) {
                        badges[item_id] = AccessLevelBadge.ORGANIZATION;
                    } else if (this.hasOtherThanOwnerRights(item_id, ids)) {
                        // for more info we would have to do db queries, so perhaps just use
                        // a label like 'Limited' or 'Restricted'
                        badges[item_id] = AccessLevelBadge.LIMITED;
                    } else {
                        // Only owners
                        badges[item_id] = AccessLevelBadge.PRIVATE;
                    }
                }
            }
            return badges;
        }
        return {};
    }

    getItemBadge(itemId: number) {
        return this.itemBadges[itemId];
    }

    getItemBadgeName(itemId: number) {
        const name = AccessLevelBadge[this.itemBadges[itemId]];
        if (name) {
            return (
                name[0] + name.substring(1, name.length).toLowerCase()
            ).replace("_", "-");
        }
        return "";
    }

    async getFolderItemTags(
        parentFolder: IItem
    ): Promise<Record<number, ITag[]>> {
        const res = await to(
            $http.get<Record<number, ITag[]>>(
                `/tags/getTags/${parentFolder.id}`
            )
        );
        if (res.ok) {
            return res.result.data;
        }
        return {};
    }

    getItemTags(item: IItem) {
        return this.itemTags[item.id];
    }

    getTagTypeString(tag: ITag) {
        return tag.name.startsWith("group:")
            ? "group"
            : TagType[tag.type].toLowerCase();
    }

    async getHakaOrgIds() {
        const res = await to($http.get<number[]>(`/groups/getOrgs/ids`));
        if (res.ok) {
            return res.result.data;
        }
        return [];
    }

    hasSomeHakaOrgAccess(groupIds: number[], hakaGroupIds: number[]) {
        return groupIds.some((id) => hakaGroupIds.includes(id));
    }

    hasOtherThanOwnerRights(itemId: string, groupIds: number[]) {
        const item_id = parseInt(itemId, 10);
        const items = this.itemList.filter((item) => item.id == item_id);
        if (items.length != 1) {
            return false; // if we got multiple items with the same id, something is wrong
        }

        const ownerIds: number[] = items[0].owners.map((group) => group.id);
        const otherIds = groupIds.filter((id) => !ownerIds.includes(id));
        return otherIds.length > 0;
    }

    protected readonly AccessLevelBadge = AccessLevelBadge;
    protected readonly AccessLevelBadgeInfo = AccessLevelBadgeInfo;
    protected readonly TagType = TagType;
}
