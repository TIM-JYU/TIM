import {Component} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem, ITag} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {Users} from "tim/user/userService";
import {folderglobals, genericglobals} from "tim/util/globals";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";

const MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives/";
const TIM_MESSAGES_FOLDER_PREFIX = "messages/tim-messages";

enum AccessLevelBadge {
    NO_BADGE = 0,
    PUBLIC = 1,
    LOGGED_IN = 2,
    ORGANIZATION = 3, // Haka organizations
    LIMITED = 4, // custom groups, users, etc.
    PRIVATE = 5, // only owner(s)
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
                <th>Last modified</th>
                <th *ngIf="displayAccessBadges" (click)="showAccessBadges = !showAccessBadges">{{showAccessBadges ? "Access" : "A" }}</th>
                <th>Owners</th>
                <th>Rights</th>
                <th *ngIf="displayDocumentTags" (click)="showTags = !showTags">{{ showTags ? "Tags" : "T"}}</th>
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
                <td *ngIf="displayAccessBadges"></td>
                <td *ngIf="displayDocumentTags"></td>
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
                </td>
                <td>{{ item.modified }}</td>
                <td *ngIf="displayAccessBadges" class="col-access-badges">
                    <ng-container  *ngIf="showAccessBadges">
                        <span class="accessbadge ab-{{ getItemBadgeName(item.id).toLowerCase() }}" 
                              title="{{ AccessLevelBadgeInfo[getItemBadge(item.id)] }}">{{ getItemBadgeName(item.id) }}</span>
                    </ng-container>
                </td>
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
                <td *ngIf="displayDocumentTags" class="col-item-tags">
                    <ng-container *ngIf="showTags">
                        <span class="itemtags" *ngFor="let tag of getItemTags(item)">
                            <span class="itemtag tagtype-{{ getTagTypeString(tag) }}" 
                              title="{{ tag.name }} {{ tag.expires ? '(expires on ' + tag.expires!.toDate() + ')' : '' }}">{{ tag.name }}</span>
                        </span>
                    </ng-container>
                </td>
                <td *ngIf="showId">
                    {{item.id}}
                </td>
            </tr>
            </tbody>
        </table>
        <p *ngIf="itemList.length == 0">There are no items to show.</p>
        <tabset *ngIf="canCreate">
            <tab [active]="false" (selectTab)="tabSelection(0)">
                        <ng-template tabHeading>
                            <span>Create a new document</span>
                            <span class="glyphicon glyphicon-file icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="document" itemLocation="{{ item.path }}" [inputAutofocus]="activeTab === 0"></create-item>
            </tab>
            <tab [active]="false" (selectTab)="tabSelection(1)">
                        <ng-template tabHeading>
                            <span>Create a new folder</span>
                            <span class="glyphicon glyphicon-folder-open icon-inline" aria-hidden="true"></span>
                        </ng-template>
                <create-item itemType="folder" itemLocation="{{ item.path }}" [inputAutofocus]="activeTab === 1"></create-item>
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
    // TODO: persist these visibility modifiers via user settings
    showId = false;
    showTags = true;
    showAccessBadges = true;
    displayAccessBadges: boolean;
    displayDocumentTags: boolean;
    activeTab: number = -1;

    constructor(private http: HttpClient) {
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

        this.displayAccessBadges =
            genericglobals().userPrefs.display_dir_list_badges;
        this.displayDocumentTags =
            genericglobals().userPrefs.display_dir_list_tags;

        // this.item is the current directory folder
        if (this.displayAccessBadges) {
            this.getAccessLevelBadges(this.item).then((value) => {
                this.itemBadges = value;
            });
        }
        if (this.displayDocumentTags) {
            this.getFolderItemTags(this.item).then((value) => {
                this.itemTags = value;
            });
        }
    }

    listOwnerNames(i: IItem) {
        return i.owners.map((o) => o.name).join(", ");
    }

    async getAccessLevelBadges(
        parentFolder: IItem
    ): Promise<Record<number, AccessLevelBadge>> {
        const resp = await toPromise(
            this.http.get<Record<number, number>>("/items/getBadges", {
                params: {
                    folder_id: parentFolder.id,
                },
            })
        );
        let badges: Record<number, AccessLevelBadge> = {};
        if (resp.ok) {
            badges = resp.result;
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

    protected tabSelection(tabNum: number) {
        this.activeTab = tabNum;
    }

    async getFolderItemTags(
        parentFolder: IItem
    ): Promise<Record<number, ITag[]>> {
        const resp = await toPromise(
            this.http.get<Record<number, ITag[]>>("/tags/getTags", {
                params: {
                    folder_id: parentFolder.id,
                },
            })
        );
        if (resp.ok) {
            return resp.result;
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

    protected readonly AccessLevelBadge = AccessLevelBadge;
    protected readonly AccessLevelBadgeInfo = AccessLevelBadgeInfo;
    protected readonly TagType = TagType;
}
