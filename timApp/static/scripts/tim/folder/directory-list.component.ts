import {Component} from "@angular/core";
import type {DocumentOrFolder, IFolder, IItem, ITag} from "tim/item/IItem";
import {TagType} from "tim/item/IItem";
import {isAdmin, Users} from "tim/user/userService";
import {folderglobals, genericglobals} from "tim/util/globals";
import {toPromise} from "tim/util/utils";
import {HttpClient} from "@angular/common/http";
import {TEACHERS_GROUPNAME} from "tim/user/IUser";
import {computeHiddenRowsFromFilters} from "tim/plugin/timTable/filtering";

/**
 * Displays the contents of a TIM folder as a sortable and filterable directory
 * listing. DIRECTORY_COLUMNS defines column labels, values, visibility, sorting,
 * filtering, widths, and minimization. A simple text column only needs a
 * configuration entry; custom cell markup belongs in the table's ngSwitch.
 * A plain-text column needs only a DIRECTORY_COLUMNS entry. Add a boolean
 * component property and a ColumnVisibilityKey entry for a new visibility
 * condition. Add a template ngSwitch case only for custom cell content such as
 * links, badges, or icons; value, sorting, and filtering use the column's value
 * function.
 */
const MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives/";
const TIM_MESSAGES_FOLDER_PREFIX = "messages/tim-messages";
const DIRECTORY_COLUMN_STATE_STORAGE_KEY = "directoryColumns:v1";

function formatLocalDateTime(date: Date) {
    const pad = (value: number) => value.toString().padStart(2, "0");
    return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(
        date.getDate()
    )} ${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(
        date.getSeconds()
    )}`;
}

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

type DirectoryColumnKey = string;

type SortDirection = "asc" | "desc";
type MinimizeMode = "no" | "open" | "closed";
type DirectoryHeaderControl = "itemTypeFilter";
type ColumnVisibilityKey =
    | "displayAccessBadges"
    | "displayDocumentTags"
    | "showId";

interface DirectoryColumn {
    /** Stable internal identifier used for filtering, sorting, and rendering. */
    key: DirectoryColumnKey;
    /** Visible column heading; localize with $localize. */
    text: string;
    /** Tooltip shown for the heading; localize with $localize. */
    title: string;
    /** Value used for display, filtering, and sorting unless sortValue is set. */
    value: (
        item: DocumentOrFolder,
        component: DirectoryListComponent
    ) => string | number;
    /** Optional value used only for sorting when it differs from value. */
    sortValue?: (item: DocumentOrFolder) => string | number;
    /** Initial sort direction when this column is first selected. */
    initialOrder?: SortDirection;
    /** Component boolean property that controls whether the column is shown. */
    show?: ColumnVisibilityKey;
    /** Filter input hint; localize with $localize if users see translatable text. */
    filterPlaceholder?: string;
    /** Filter input tooltip; localize with $localize. */
    filterTitle?: string;
    /** Whether the column can be minimized and its default state. */
    minimize?: MinimizeMode;
    /** Stable, unique one-character key persisted in localStorage. */
    minimizeKey?: string;
    /** Label shown while minimized; localize with $localize. */
    minimizedText?: string;
    /** Optional control rendered in the heading, selected by its template type. */
    headerControl?: DirectoryHeaderControl;
    /** CSS width for the column when it is not minimized. */
    width?: string;
}

// Configure directory columns here. Simple text columns need no changes outside
// this array; custom cell markup is handled in the template's ngSwitch. Keep
// minimizeKey values stable and unique; bump the storage key version if their
// meaning changes.
const DIRECTORY_COLUMNS: DirectoryColumn[] = [
    {
        key: "name",
        text: $localize`Name`,
        title: $localize`Item name`,
        value: (item) => item.title,
        initialOrder: "asc",
        headerControl: "itemTypeFilter",
    },
    {
        key: "time",
        text: $localize`Last modified`,
        title: $localize`Date the item was last modified`,
        value: (item) => formatLocalDateTime(new Date(item.modifiedTimeFull)),
        sortValue: (item) => new Date(item.modifiedTimeFull).getTime(),
        initialOrder: "desc",
        minimize: "open",
        minimizeKey: "t",
        minimizedText: $localize`:column abbreviation|Clock symbol shown when the Last modified column is minimized:◷`,
        filterPlaceholder: $localize`Local date/time`,
        filterTitle: $localize`Local time: YYYY-MM-DD HH:mm:ss (e.g. 2026-10-29 15:00); partial values such as 2026-10-29 15 match that hour`,
    },
    {
        key: "access",
        text: $localize`Access`,
        title: $localize`Access status for item`,
        value: (item, component) => component.getItemBadgeName(item.id),
        show: "displayAccessBadges",
        minimize: "open",
        minimizeKey: "a",
        minimizedText: $localize`:column abbreviation|Short label shown when the Access column is minimized:A`,
        width: "7em",
    },
    {
        key: "owners",
        text: $localize`Owners`,
        title: $localize`Owners of item`,
        minimize: "open",
        minimizeKey: "o",
        minimizedText: $localize`:column abbreviation|Short label shown when the Owners column is minimized:O`,
        value: (item) => item.owners.map((owner) => owner.name).join(", "),
    },
    {
        key: "rights",
        text: $localize`Rights`,
        title: $localize`Available item rights`,
        value: (item) => {
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
        },
        width: "5em",
        minimize: "open",
        minimizeKey: "r",
        minimizedText: $localize`:column abbreviation|Short label shown when the Rights column is minimized:R`,
        filterPlaceholder: "edit / manage / teacher",
        filterTitle: $localize`Filter by edit, manage, or teacher`,
    },
    {
        key: "tags",
        text: $localize`Tags`,
        title: $localize`Tags on item`,
        value: (item, component) =>
            (component.getItemTags(item) ?? [])
                .map((tag) => tag.name)
                .join(" "),
        show: "displayDocumentTags",
        minimize: "open",
        minimizeKey: "g",
        minimizedText: $localize`:column abbreviation|Short label shown when the Tags column is minimized:T`,
    },
    {
        key: "id",
        text: $localize`Id`,
        title: $localize`Item ID`,
        value: (item) => item.id,
        sortValue: (item) => item.id,
        initialOrder: "asc",
        show: "showId",
        minimize: "open",
        minimizeKey: "i",
        minimizedText: $localize`:column abbreviation|Short label shown when the ID column is minimized:Id`,
        width: "4em",
    },
];

const DEFAULT_FILTER_PLACEHOLDER = $localize`Filter`;
const DEFAULT_FILTER_TITLE = $localize`Regex filter (case-insensitive) or <, >, <=, >= to compare`;

@Component({
    selector: "tim-index",
    template: `
            <table class="table directory-list-table" *ngIf="itemList.length > 0 || item.path">
            <thead>
            <tr>
                <th class="count-cell pointer" (click)="clearFilters()" 
                    title="Total number of items in this folder. Click to clear all filters.">
                    <span class="filter-count">
                        {{ itemList.length }}
                    </span>
                </th>
                <th
                    *ngFor="let column of visibleColumns"
                    class="sort-table-header"
                    [class.minimize]="isMinimizable(column)"
                    [class.minimized]="isMinimized(column)"
                    [style.width]="columnWidth(column)"
                    [title]="column.title"
                    (click)="handleColumnHeaderClick($event, column)"
                >
                    <span class="column-label">
                        {{ isMinimized(column) ? minimizedLabel(column) : column.text }}
                    </span>
                    <select
                        *ngIf="column.headerControl === 'itemTypeFilter' && !isMinimized(column)"
                        class="item-type-filter"
                        [(ngModel)]="itemTypeFilter"
                        (ngModelChange)="applyFilters()"
                        (click)="$event.stopPropagation()"
                        title="Show all items, docs, or folders"
                        aria-label="Show all items, docs, or folders"
                        i18n-title
                        i18n-aria-label
                    >
                        <option value="all" i18n>All</option>
                        <option value="docs" i18n>Docs</option>
                        <option value="folders" i18n>Folders</option>
                    </select>
                    <span *ngIf="sortState?.key === column.key" aria-hidden="true">
                        <span
                            class="glyphicon"
                            [class.glyphicon-triangle-top]="sortState?.direction === 'asc'"
                            [class.glyphicon-triangle-bottom]="sortState?.direction === 'desc'"
                        ></span>
                    </span>
                    <button
                        *ngIf="isMinimizable(column)"
                        type="button"
                        class="minimize-toggle"
                        [attr.aria-label]="isMinimized(column) ? 'Expand column' : 'Narrow column'"
                        [title]="isMinimized(column) ? 'Expand column' : 'Narrow column'"
                        (click)="toggleMinimized($event, column)"
                    >{{ isMinimized(column) ? "›" : "‹" }}</button>
                </th>
            </tr>
            <tr class="filters-row">
                <th class="count-cell" title="Number of items matching the filters">
                    <span class="filter-count" *ngIf="filteredItemList.length < itemList.length">
                        {{ filteredItemList.length }}
                    </span>
                </th>
                <th
                    *ngFor="let column of visibleColumns"
                    [class.minimized]="isMinimized(column)"
                    [style.width]="columnWidth(column)"
                >
                    <div class="filter-input-wrapper" *ngIf="!isMinimized(column)">
                        <input
                            type="text"
                            class="form-control input-sm"
                            [placeholder]="column.filterPlaceholder ?? defaultFilterPlaceholder"
                            [title]="column.filterTitle ?? defaultFilterTitle"
                            [(ngModel)]="filterValues[column.key]"
                            (ngModelChange)="applyFilters()"
                        />
                        <button
                            *ngIf="filterValues[column.key]"
                            type="button"
                            class="clear-filter"
                            title="Clear this filter"
                            aria-label="Clear this filter"
                            (click)="clearFilter(column.key)"
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
                <td
                    *ngFor="let column of visibleColumns"
                    [class.minimized]="isMinimized(column)"
                    [style.width]="columnWidth(column)"
                    [class.date-column]="column.key === 'time'"
                >
                    <a *ngIf="column.key === 'name'" href="/view/{{ item.location }}" i18n>Go to parent folder</a>
                </td>
            </tr>
            <tr *ngFor="let item of filteredItemList">
                <td>
                    <a *ngIf="item.isFolder" href="/view/{{ item.path }}">
                        <span class="glyphicon glyphicon-folder-open" aria-hidden="true"></span>
                    </a>
                </td>
                <td
                    *ngFor="let column of visibleColumns"
                    [class.minimized]="isMinimized(column)"
                    [style.width]="columnWidth(column)"
                    [class.date-column]="column.key === 'time'"
                    [ngSwitch]="column.key"
                >
                    <span class="column-content" *ngSwitchCase="'name'">
                        <a href="/view/{{ item.path }}">{{ item.title }}</a>&ngsp;
                    </span>
                    <span
                        class="column-content"
                        *ngSwitchCase="'time'"
                        [title]="getLocalModifiedDate(item)"
                    >{{ item.modified }}</span>
                    <span class="column-content col-access-badges" *ngSwitchCase="'access'">
                        <span class="accessbadge ab-{{ getItemBadgeName(item.id).toLowerCase() }}"
                              title="{{ AccessLevelBadgeInfo[getItemBadge(item.id)] }}">{{ getItemBadgeName(item.id) }}</span>
                    </span>
                    <span class="column-content" *ngSwitchCase="'owners'">
                        {{ listOwnerNames(item) }}
                    </span>
                    <span class="column-content" *ngSwitchCase="'rights'">
                        <a title="Edit" *ngIf="item.rights.editable && !item.isFolder"
                           href="/view/{{ item.path }}"><i class="glyphicon glyphicon-pencil"></i></a>
                        &ngsp;<a title="Manage" *ngIf="item.rights.manage" href="/manage/{{ item.path }}"><i
                            class="glyphicon glyphicon-cog"></i></a>
                        &ngsp;<a title="Teacher" *ngIf="item.rights.teacher && !item.isFolder"
                                 href="/teacher/{{ item.path }}"><i class="glyphicon glyphicon-education"></i></a>
                    </span>
                    <span class="column-content col-item-tags" *ngSwitchCase="'tags'">
                        <span class="itemtags" *ngFor="let tag of getItemTags(item)">
                            <span class="itemtag tagtype-{{ getTagTypeString(tag) }}"
                                  title="{{ tag.name }} {{ tag.expires ? '(expires on ' + tag.expires!.toDate() + ')' : '' }}">{{ tag.name }}</span>
                        </span>
                    </span>
                    <span class="column-content" *ngSwitchDefault>
                        {{ getColumnValue(item, column) }}
                    </span>
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
export class DirectoryListComponent {
    itemList: DocumentOrFolder[];
    itemBadges: Record<number, AccessLevelBadge>;
    itemTags: Record<number, ITag[]>;
    item: IFolder;
    canCreate: boolean;
    showId = false;
    displayAccessBadges: boolean;
    displayDocumentTags: boolean;
    activeTab: number = -1;
    sortState?: {key: DirectoryColumnKey; direction: SortDirection};

    filteredItemList: DocumentOrFolder[] = [];
    filterValues: Record<DirectoryColumnKey, string> = {};
    itemTypeFilter: "all" | "docs" | "folders" = "all";
    readonly columns = DIRECTORY_COLUMNS;

    get visibleColumns() {
        return this.columns.filter((column) => {
            return column.show ? this[column.show] : true;
        });
    }

    constructor(private http: HttpClient) {
        const fg = folderglobals();
        this.itemList = fg.items;
        this.item = fg.curr_item;
        this.canCreate = Users.isRealUser();
        this.itemBadges = {};
        this.itemTags = {};

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

        for (const column of this.columns) {
            if (column.minimize === "closed") {
                this.minimizedColumns.add(column.key);
            }
        }
        this.restoreMinimizedColumns();
        this.applyFilters();
    }

    get defaultFilterPlaceholder() {
        return DEFAULT_FILTER_PLACEHOLDER;
    }

    get defaultFilterTitle() {
        return DEFAULT_FILTER_TITLE;
    }

    getLocalModifiedDate(item: DocumentOrFolder) {
        return formatLocalDateTime(new Date(item.modifiedTimeFull));
    }

    isMinimizable(column: DirectoryColumn) {
        return (column.minimize ?? "no") !== "no";
    }

    isMinimized(column: DirectoryColumn) {
        return this.minimizedColumns.has(column.key);
    }

    columnWidth(column: DirectoryColumn) {
        return this.isMinimized(column)
            ? "1.8em"
            : column.width ?? "max-content";
    }

    private minimizedColumns = new Set<DirectoryColumnKey>();

    private restoreMinimizedColumns() {
        const savedKeys = window.localStorage.getItem(
            DIRECTORY_COLUMN_STATE_STORAGE_KEY
        );
        if (savedKeys === null) {
            return;
        }

        for (const column of this.columns) {
            if (
                this.isMinimizable(column) &&
                column.minimizeKey &&
                savedKeys.includes(column.minimizeKey)
            ) {
                if (this.minimizedColumns.has(column.key)) {
                    this.minimizedColumns.delete(column.key);
                } else {
                    this.minimizedColumns.add(column.key);
                }
            }
        }
    }

    private saveMinimizedColumns() {
        const changedKeys = this.columns
            .filter((column) => {
                const defaultMinimized = column.minimize === "closed";
                return (
                    this.isMinimizable(column) &&
                    this.isMinimized(column) !== defaultMinimized
                );
            })
            .map((column) => column.minimizeKey)
            .filter((key): key is string => key !== undefined);
        window.localStorage.setItem(
            DIRECTORY_COLUMN_STATE_STORAGE_KEY,
            changedKeys.join("")
        );
    }

    minimizedLabel(column: DirectoryColumn) {
        return column.minimizedText ?? column.text.slice(0, 1);
    }

    handleColumnHeaderClick(event: Event, column: DirectoryColumn) {
        if (this.isMinimized(column)) {
            this.toggleMinimized(event, column);
            return;
        }
        this.sortListing(column.key);
    }

    toggleMinimized(event: Event, column: DirectoryColumn) {
        event.stopPropagation();
        if (this.isMinimized(column)) {
            this.minimizedColumns.delete(column.key);
        } else {
            this.minimizedColumns.add(column.key);
            this.filterValues[column.key] = "";
            this.applyFilters();
        }
        this.saveMinimizedColumns();
    }

    clearFilters() {
        this.itemTypeFilter = "all";
        for (const column of this.columns) {
            this.filterValues[column.key] = "";
        }
        this.applyFilters();
    }

    clearFilter(key: DirectoryColumnKey) {
        this.filterValues[key] = "";
        this.applyFilters();
    }

    applyFilters() {
        const columns = this.visibleColumns;
        const hiddenRows = computeHiddenRowsFromFilters(
            this.itemList,
            (index) =>
                this.itemTypeFilter === "all" ||
                (this.itemTypeFilter === "folders"
                    ? this.itemList[index].isFolder
                    : !this.itemList[index].isFolder),
            [columns.map((column) => this.filterValues[column.key])],
            true,
            (item, columnIndex) =>
                this.getFilterValue(item, columns[columnIndex])
        );

        this.filteredItemList = this.itemList.filter(
            (_item, index) => !hiddenRows.has(index)
        );
    }

    private getFilterValue(
        item: DocumentOrFolder,
        column: DirectoryColumn
    ): string | number {
        return column.value(item, this);
    }

    sortListing(key: DirectoryColumnKey) {
        const column = this.columns.find((c) => c.key === key);
        if (!column) {
            return;
        }
        const direction =
            this.sortState?.key === key
                ? this.sortState.direction === "asc"
                    ? "desc"
                    : "asc"
                : column.initialOrder ?? "asc";
        this.sortState = {key, direction};

        const foldersArr = this.itemList.filter((item) => item.isFolder);
        const docsArr = this.itemList.filter((item) => !item.isFolder);
        const compare = (a: DocumentOrFolder, b: DocumentOrFolder) => {
            const result = this.compareColumnValues(a, b, column);
            return direction === "asc" ? result : -result;
        };
        foldersArr.sort(compare);
        docsArr.sort(compare);
        this.itemList = foldersArr.concat(docsArr);
        this.applyFilters();
    }

    private compareColumnValues(
        a: DocumentOrFolder,
        b: DocumentOrFolder,
        column: DirectoryColumn
    ) {
        const aValue = column.sortValue
            ? column.sortValue(a)
            : column.value(a, this);
        const bValue = column.sortValue
            ? column.sortValue(b)
            : column.value(b, this);
        if (typeof aValue === "number" && typeof bValue === "number") {
            return aValue - bValue;
        }
        return String(aValue).localeCompare(String(bValue), "fi");
    }

    getColumnValue(item: DocumentOrFolder, column: DirectoryColumn) {
        return column.value(item, this);
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
