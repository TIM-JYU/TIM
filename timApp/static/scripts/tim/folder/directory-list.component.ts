import type {AfterViewInit} from "@angular/core";
import {Component, ElementRef, Renderer2, ViewChild} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem, ITag} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {isAdmin, Users} from "tim/user/userService";
import {folderglobals, genericglobals} from "tim/util/globals";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TEACHERS_GROUPNAME} from "tim/user/IUser";
import {computeHiddenRowsFromFilters} from "tim/plugin/timTable/filtering";

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

type DirectoryFilterKey =
    | "title"
    | "modified"
    | "access"
    | "owners"
    | "rights"
    | "tags"
    | "id";

@Component({
    selector: "tim-index",
    template: `
            <table #directoryTable class="table" *ngIf="itemList.length > 0 || item.path">
            <thead>
            <ng-template #sortAsc><span class="glyphicon glyphicon-triangle-top" aria-hidden="true"></span>
            </ng-template>
            <ng-template #sortDesc><span class="glyphicon glyphicon-triangle-bottom" aria-hidden="true"></span>
            </ng-template>
            <tr>
                <th class="count-cell pointer" (click)="clearFilters()" 
                    title="Total number of items in this folder. Click to clear all filters.">
                    <span class="filter-count" >
                        {{ itemList.length }}
                    </span>        
                </th>
                <th (click)="sortListing('name')" class="sort-table-header">
                    Name
                    <span *ngIf="currentSortOrder === 'name'">
                        <span *ngIf="sortByNameAsc; then sortDesc else sortAsc"></span>
                    </span>
                </th>
                <th (click)="sortListing('time')" class="sort-table-header">
                    Last modified
                    <span *ngIf="currentSortOrder === 'time'">
                        <span *ngIf="sortByModifiedDesc; then sortAsc else sortDesc"></span>
                    </span>
                </th>
                <th *ngIf="displayAccessBadges" 
                    class="minimize sort-table-header" 
                    (click)="sortListing('access')"
                    title="Access">
                    Access
                </th>
                <th (click)="sortListing('owners')"  class="sort-table-header">
                    Owners
                    <span *ngIf="currentSortOrder === 'owners'">
                        <span *ngIf="!sortByOtherAsc; then sortAsc else sortDesc"></span>
                    </span>
                </th>
                <th (click)="sortListing('rights')" class="sort-table-header">
                    Rights
                    <span *ngIf="currentSortOrder === 'rights'">
                        <span *ngIf="!sortByOtherAsc; then sortAsc else sortDesc"></span>
                    </span>
                </th>
                <th *ngIf="displayDocumentTags"
                    class="minimize sort-table-header" 
                    (click)="sortListing('tags')"
                    title="Tags">
                    Tags
                </th>
                <th *ngIf="showId" class="gray sort-table-header" (click)="sortListing('id')">
                    Id
                    <span *ngIf="currentSortOrder === 'id'">
                        <span *ngIf="sortByIdAsc; then sortDesc else sortAsc"></span>
                    </span>
                </th>
            </tr>
            <tr class="filters-row">
                <th class="count-cell" title="Number of items matching the filters">
                    <span class="filter-count" *ngIf="filteredItemList.length < itemList.length">
                        {{ filteredItemList.length }}
                    </span>        
                </th>
                <th *ngFor="let column of filterColumns; let i = index">
                    <div class="filter-input-wrapper">
                    <input
                            type="text"
                            class="form-control input-sm"
                            [placeholder]="column.key === 'modified'  
                                    ? 'YYYY-MM-DD'
                                    : column.key === 'rights'
                                        ? 'edit / manage / teacher'
                                        : 'Filter'"
                            [title]="column.key === 'modified'
                                ? 'Date: YYYY-MM-DD (e.g. 2026-10-29); <2026-10-29 = before October 29, 2026'
                                : 'Regex filter (case-insensitive) or <, >, <=, >= to compare'"
                            [(ngModel)]="filterValues[i]"
                            (ngModelChange)="applyFilters()"
                        />
                        <button
                            *ngIf="filterValues[i]"
                            type="button"
                            class="clear-filter"
                            title="Clear this filter"
                            aria-label="Clear this filter"
                            (click)="clearFilter(i)"
                        >×</button>
                    </div>  
                </th>
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
            <tr *ngFor="let item of filteredItemList">
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
                    <ng-container *ngIf="showAccessBadges">
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
                    {{ item.id }}
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
                <create-item itemType="document" itemLocation="{{ item.path }}"
                             [enable]="activeTab === 0"></create-item>
            </tab>
            <tab [active]="false" (selectTab)="tabSelection(1)">
                <ng-template tabHeading>
                    <span>Create a new folder</span>
                    <span class="glyphicon glyphicon-folder-open icon-inline" aria-hidden="true"></span>
                </ng-template>
                <create-item itemType="folder" itemLocation="{{ item.path }}" [enable]="activeTab === 1"></create-item>
            </tab>
        </tabset>
    `,
    styleUrls: ["directory-list.component.scss"],
})
export class DirectoryListComponent implements AfterViewInit {
    @ViewChild("directoryTable")
    directoryTable?: ElementRef<HTMLTableElement>;
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
    currentSortOrder: string;
    sortByNameAsc: boolean;
    sortByModifiedDesc: boolean;
    sortByIdAsc: boolean;
    sortByOtherAsc: boolean = true;

    // For filtering:
    filteredItemList: DocumentOrFolder[] = [];
    filterValues: string[] = [];
    filterColumns: {key: DirectoryFilterKey; label: string}[] = [];

    constructor(private http: HttpClient, private renderer: Renderer2) {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isRealUser();
        this.itemBadges = {};
        this.itemTags = {};
        this.currentSortOrder = "default";
        this.sortByNameAsc = true;
        this.sortByModifiedDesc = true;
        this.sortByIdAsc = true;
        this.sortByOtherAsc = true;

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
                this.applyFilters();
            });
        }
        if (this.displayDocumentTags) {
            this.getFolderItemTags(this.item).then((value) => {
                this.itemTags = value;
                this.applyFilters();
            });
        }

        if (
            isAdmin() ||
            Users.belongsToGroup(TEACHERS_GROUPNAME) ||
            this.item.rights.manage ||
            this.item.rights.owner
        ) {
            this.showId = true;
        }

        this.filterColumns = [
            {key: "title", label: "Name"},
            {key: "modified", label: "Last modified"},
        ];

        if (this.displayAccessBadges) {
            this.filterColumns.push({key: "access", label: "Access"});
        }

        this.filterColumns.push(
            {key: "owners", label: "Owners"},
            {key: "rights", label: "Rights"}
        );

        if (this.displayDocumentTags) {
            this.filterColumns.push({key: "tags", label: "Tags"});
        }

        if (this.showId) {
            this.filterColumns.push({key: "id", label: "Id"});
        }

        this.filterValues = this.filterColumns.map(() => "");
        this.applyFilters();
    }

    ngAfterViewInit() {
        const table = this.directoryTable?.nativeElement;
        if (!table) {
            return;
        }

        for (const header of Array.from(
            table.querySelectorAll<HTMLTableCellElement>("th.minimize")
        )) {
            const textNode = Array.from(header.childNodes).find(
                (node): node is Text =>
                    node.nodeType === Node.TEXT_NODE &&
                    !!node.textContent?.trim()
            );
            if (!textNode) {
                continue;
            }

            const label = textNode.textContent?.trim() ?? "";
            const button = this.renderer.createElement("button");
            this.renderer.setAttribute(button, "type", "button");
            this.renderer.addClass(button, "minimize-toggle");
            this.renderer.setAttribute(
                button,
                "title",
                "Narrow or expand column"
            );
            this.renderer.setProperty(button, "textContent", "‹");

            this.renderer.listen(button, "click", (event: Event) => {
                event.stopPropagation();
                const minimized = !header.classList.contains("minimized");

                for (const row of Array.from(table.rows)) {
                    const cell = row.cells[header.cellIndex];
                    cell?.classList.toggle("minimized", minimized);
                }

                this.renderer.setValue(textNode, minimized ? label[0] : label);
                this.renderer.setProperty(
                    button,
                    "textContent",
                    minimized ? "›" : "‹"
                );
            });

            this.renderer.setStyle(button, "margin-left", "0.25em");
            this.renderer.setStyle(button, "padding", "0");
            this.renderer.setStyle(button, "border", "0");
            this.renderer.setStyle(button, "background", "transparent");
            this.renderer.setStyle(button, "color", "#888");
            this.renderer.setStyle(button, "cursor", "pointer");
            this.renderer.appendChild(header, button);
        }
    }

    clearFilters() {
        this.filterValues.fill("");
        this.applyFilters();
    }

    clearFilter(index: number) {
        this.filterValues[index] = "";
        this.applyFilters();
    }

    applyFilters() {
        const hiddenRows = computeHiddenRowsFromFilters(
            this.itemList,
            () => true,
            [this.filterValues],
            false,
            (item, columnIndex) => {
                const column = this.filterColumns[columnIndex];
                switch (column.key) {
                    case "title":
                        return item.title;
                    case "modified":
                        return new Date(item.modifiedTimeFull).toISOString();
                    case "access":
                        return this.getItemBadgeName(item.id);
                    case "owners":
                        return this.listOwnerNames(item);
                    case "rights": {
                        const rights = [];
                        if (item.rights.editable && !item.isFolder) {
                            rights.push("edit");
                        }
                        if (item.rights.manage) {
                            rights.push("manage");
                        }
                        if (item.rights.teacher && !item.isFolder) {
                            rights.push("teacher");
                        }
                        return rights.join(" ");
                    }
                    case "tags":
                        return (this.getItemTags(item) ?? [])
                            .map((tag) => tag.name)
                            .join(" ");
                    case "id":
                        return item.id;
                }
            }
        );

        this.filteredItemList = this.itemList.filter(
            (_item, index) => !hiddenRows.has(index)
        );
    }

    sortListing(column: string) {
        let foldersArr = this.itemList.filter((item) => item.isFolder);
        let docsArr = this.itemList.filter((item) => !item.isFolder);
        this.currentSortOrder = column;

        if (column === "name") {
            foldersArr = foldersArr.sort((a, b) =>
                a.title.localeCompare(b.title, "fi")
            );
            docsArr = docsArr.sort((a, b) =>
                a.title.localeCompare(b.title, "fi")
            );

            if (!this.sortByNameAsc) {
                foldersArr.reverse();
                docsArr.reverse();
            }

            this.sortByModifiedDesc = true;
            this.sortByIdAsc = true;
            this.sortByOtherAsc = true;
            this.sortByNameAsc = !this.sortByNameAsc;
        } else if (column === "time") {
            foldersArr.sort((a, b) =>
                new Date(a.modifiedTimeFull).getTime() <
                new Date(b.modifiedTimeFull).getTime()
                    ? 1
                    : -1
            );
            docsArr.sort((a, b) =>
                new Date(a.modifiedTimeFull).getTime() <
                new Date(b.modifiedTimeFull).getTime()
                    ? 1
                    : -1
            );

            if (!this.sortByModifiedDesc) {
                foldersArr.reverse();
                docsArr.reverse();
            }

            this.sortByNameAsc = true;
            this.sortByIdAsc = true;
            this.sortByOtherAsc = true;
            this.sortByModifiedDesc = !this.sortByModifiedDesc;
        } else if (column === "id") {
            foldersArr.sort((a, b) => a.id - b.id);
            docsArr.sort((a, b) => a.id - b.id);

            if (!this.sortByIdAsc) {
                foldersArr.reverse();
                docsArr.reverse();
            }

            this.sortByNameAsc = true;
            this.sortByModifiedDesc = true;
            this.sortByOtherAsc = true;
            this.sortByIdAsc = !this.sortByIdAsc;
        } else {
            // For other columns just alphabetical sort based on the column value
            foldersArr.sort((a, b) => {
                const aValue = this.getColumnValue(a, column);
                const bValue = this.getColumnValue(b, column);
                return aValue.localeCompare(bValue, "fi");
            });
            docsArr.sort((a, b) => {
                const aValue = this.getColumnValue(a, column);
                const bValue = this.getColumnValue(b, column);
                return aValue.localeCompare(bValue, "fi");
            });
            if (!this.sortByOtherAsc) {
                foldersArr.reverse();
                docsArr.reverse();
            }
            this.sortByNameAsc = true;
            this.sortByModifiedDesc = true;
            this.sortByIdAsc = true;
            this.sortByOtherAsc = !this.sortByOtherAsc;
        }
        this.itemList = foldersArr.concat(docsArr);
        this.applyFilters();
    }

    getColumnValue(a: DocumentOrFolder, column: string) {
        switch (column) {
            case "owners":
                return this.listOwnerNames(a);
            case "rights": {
                const rights = [];
                if (a.rights.editable && !a.isFolder) {
                    rights.push("edit");
                }
                if (a.rights.manage) {
                    rights.push("manage");
                }
                if (a.rights.teacher && !a.isFolder) {
                    rights.push("teacher");
                }
                return rights.join(" ");
            }
            case "tags":
                return (this.getItemTags(a) ?? [])
                    .map((tag) => tag.name)
                    .join(" ");
            case "access":
                return this.getItemBadgeName(a.id);
            case "title":
                return a.title;
            case "modified":
                return a.modified;
            case "id":
                return a.id.toString();
            default:
                return "";
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
