import type {OnInit} from "@angular/core";
import {
    Component,
    ElementRef,
    Input,
    ViewChild,
    ChangeDetectionStrategy,
} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import moment from "moment";
import humanizeDuration from "humanize-duration";
import type {AngularError, Result} from "tim/util/utils";
import {
    capitalizeFirstLetter,
    getGroupDesc,
    timeout,
    toPromise,
} from "tim/util/utils";
import {withQueryParams} from "tim/util/http";
import type {DurationChoice} from "tim/ui/duration-picker.component";
import {durationTypes} from "tim/ui/duration-picker.component";
import type {IGroup} from "tim/user/IUser";
import {genericglobals, itemglobals} from "tim/util/globals";
import type {
    IAccessType,
    IItemWithRights,
    IRight,
} from "tim/item/access-role.service";
import {accessOrder} from "tim/item/access-role.service";

const tips: Record<string, string | undefined> = {
    teacher: "Teacher right does not give edit right by itself.",
    "see answers": "Seeing answers does not give edit right by itself.",
};

enum ActionOption {
    Add = "add",
    Confirm = "confirm",
    Expire = "expire",
    Remove = "remove",
}

interface IPermissionEditResponse {
    not_exist: string[];
}

/**
 * An item row in the table of mass mode.
 */
interface IItemRow extends IItemWithRights {
    relPath: string;
    rightsStr: string;
}

type GridField = "title" | "relPath" | "rightsStr";

let instanceCounter = 0;

/**
 * Converts an input value that may have been given as a plain HTML attribute, i.e. as a string.
 *
 * The editor can be put in a document as HTML with its options as attributes (e.g. `force-duration="4"`,
 * `barcode-mode="true"`, `restrict-rights="['view']"`), in which case every value arrives as a string.
 */
function attrValue<T>(value: T | string | undefined): T | undefined {
    if (typeof value !== "string") {
        return value;
    }
    const s = value.trim();
    if (s === "") {
        return undefined;
    }
    if (s === "true" || s === "false") {
        return (s === "true") as unknown as T;
    }
    if (/^-?\d+(\.\d+)?$/.test(s)) {
        return Number(s) as unknown as T;
    }
    if (s.startsWith("[") && s.endsWith("]")) {
        try {
            return JSON.parse(s.replace(/'/g, '"')) as T;
        } catch {
            return undefined;
        }
    }
    if (s.length >= 2 && /^(['"]).*\1$/.test(s)) {
        return s.slice(1, -1) as unknown as T;
    }
    return s as unknown as T;
}

type Sortable = string | null | undefined;

/**
 * Compares the values the same way as the AngularJS orderBy filter did:
 * strings case-insensitively, null values after strings and undefined values last.
 */
function compareLikeOrderBy(a: Sortable, b: Sortable): number {
    const rank = (v: Sortable) => (v === undefined ? 2 : v === null ? 1 : 0);
    const ra = rank(a);
    const rb = rank(b);
    if (ra !== rb) {
        return ra - rb;
    }
    if (typeof a !== "string" || typeof b !== "string") {
        return 0;
    }
    const la = a.toLowerCase();
    const lb = b.toLowerCase();
    return la < lb ? -1 : la > lb ? 1 : 0;
}

/**
 * A component for viewing and editing the rights of an item (or the rights of many items at once in mass mode).
 */
@Component({
    selector: "tim-rights-editor",
    templateUrl: "./rights-editor.component.html",
    // The layout relies on the whitespace between the inline elements (e.g. buttons).
    preserveWhitespaces: true,
    styles: [
        `
            .rights-item-table {
                max-height: 40em;
                overflow-y: auto;
                margin-bottom: 1em;
            }

            .rights-item-table table {
                margin-bottom: 0;
            }

            .rights-item-table thead th {
                position: sticky;
                top: 0;
                background-color: #f3f3f3;
                z-index: 1;
            }

            .rights-item-table thead tr:nth-child(2) th {
                top: 2.2em;
                font-weight: normal;
            }

            .rights-item-table .select-col {
                width: 2em;
                text-align: center;
            }

            .rights-item-table .sort-header {
                cursor: pointer;
                color: inherit;
            }

            .rights-item-table tbody tr {
                cursor: pointer;
            }

            .rights-item-table tbody tr.selected {
                background-color: #d9edf7;
            }

            .picker-disabled {
                pointer-events: none;
                opacity: 0.6;
            }
        `,
    ],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class RightsEditorComponent implements OnInit {
    @Input() accessTypes?: IAccessType[];
    @Input() action?: ActionOption;
    @Input() allowSelectAction?: boolean | string;
    @Input() barcodeMode?: boolean | string;
    @Input() clearInput?: boolean | string;
    @Input() confirmExpire?: boolean | string;
    @Input() defaultItem?: string;
    @Input() forceConfirm?: boolean | string;
    @Input() forceDuration?: number | string;
    @Input() forceDurationEnd?: string;
    @Input() forceDurationStart?: string;
    @Input() forceDurationAccessTo?: string;
    @Input() hideEdit?: boolean | string;
    @Input() hideExpire?: boolean | string;
    @Input() hideRemove?: boolean | string;
    @Input() itemId?: number | string;
    @Input() massMode?: boolean | string;
    @Input() orgs?: IGroup[];
    @Input() restrictRights?: string[] | string;

    @ViewChild("groupNameInput")
    private groupNameInput?: ElementRef<HTMLInputElement>;
    @ViewChild("groupNameArea")
    private groupNameArea?: ElementRef<HTMLTextAreaElement>;
    @ViewChild("editArea") private editArea?: ElementRef<HTMLElement>;

    durOpt: {
        durationType: DurationChoice;
        durationAmount: number;
        accessTo?: Date;
    };
    timeOpt: {
        type: string;
        to?: Date;
        from?: Date;
        durationTo?: Date;
        durationFrom?: Date;
    };
    grouprights?: IRight[];
    showActiveOnly = true;
    editVelpGroupPermissions = true;
    editTranslationPermissions = true;
    selectedRight: IRight | null = null;
    accessType: IAccessType | undefined;
    addingRight = false;
    listMode = false;
    groupName: string | undefined;
    actionOption = ActionOption.Add;
    gridReady = false;
    errMsg?: string;
    successMsg?: string;
    loading = false;
    selectedOrg?: IGroup;
    requireConfirm = false;
    lastEdited?: IRight;
    confirmingRight?: IRight;
    expiringRight?: IRight;
    removingRight?: IRight;
    activeBoxLoading = false;
    tips = tips;
    idPrefix = `rightsEditor${instanceCounter++}-`;

    // The rights of each access type that are currently shown, in display order.
    private shownRights = new Map<number, IRight[]>();

    // Mass mode
    gridColumns: {field: GridField; name: string}[] = [
        {field: "title", name: "Title"},
        {field: "relPath", name: "Relative path"},
        {field: "rightsStr", name: "Rights"},
    ];
    gridFilters: Record<GridField, string> = {
        title: "",
        relPath: "",
        rightsStr: "",
    };
    sortField: GridField = "relPath";
    sortAscending = true;
    private items: IItemRow[] = [];
    visibleItems: IItemRow[] = [];
    selectedIds = new Set<number>();

    constructor(
        private http: HttpClient,
        private element: ElementRef<HTMLElement>
    ) {
        // The start and end time of a range are the current time by default.
        const now = moment().milliseconds(0).toDate();
        this.timeOpt = {type: "always", from: now, to: new Date(now)};
        this.durOpt = {durationType: "hours", durationAmount: 4};
    }

    private normalizeInputs() {
        this.allowSelectAction = attrValue<boolean>(this.allowSelectAction);
        this.barcodeMode = attrValue<boolean>(this.barcodeMode);
        this.clearInput = attrValue<boolean>(this.clearInput);
        this.confirmExpire = attrValue<boolean>(this.confirmExpire);
        this.defaultItem = attrValue<string>(this.defaultItem);
        this.forceConfirm = attrValue<boolean>(this.forceConfirm);
        this.forceDuration = attrValue<number>(this.forceDuration);
        this.forceDurationEnd = attrValue<string>(this.forceDurationEnd);
        this.forceDurationStart = attrValue<string>(this.forceDurationStart);
        this.forceDurationAccessTo = attrValue<string>(
            this.forceDurationAccessTo
        );
        this.hideEdit = attrValue<boolean>(this.hideEdit);
        this.hideExpire = attrValue<boolean>(this.hideExpire);
        this.hideRemove = attrValue<boolean>(this.hideRemove);
        this.itemId = attrValue<number>(this.itemId);
        this.massMode = attrValue<boolean>(this.massMode);
        this.restrictRights = attrValue<string[]>(this.restrictRights);
        this.action = attrValue<ActionOption>(this.action);
    }

    relPath(item: IItemWithRights) {
        return item.path.slice(itemglobals().curr_item.path.length + 1);
    }

    async ngOnInit() {
        this.normalizeInputs();
        this.actionOption = this.action ?? ActionOption.Add;
        if (!this.accessTypes || !this.massMode) {
            await this.getPermissions();
        }
        if (this.forceDuration) {
            this.timeOpt.type = "duration";
            this.durOpt.durationAmount = this.forceDuration as number;
            this.durOpt.durationType = "hours";
            if (this.forceDurationStart) {
                this.timeOpt.durationFrom = moment(
                    this.forceDurationStart
                ).toDate();
            }
            if (this.forceDurationEnd) {
                this.timeOpt.durationTo = moment(
                    this.forceDurationEnd
                ).toDate();
            }
            if (this.forceDurationAccessTo) {
                this.durOpt.accessTo = moment(
                    this.forceDurationAccessTo
                ).toDate();
            }
        }
        if (this.forceConfirm != null) {
            this.requireConfirm = this.forceConfirm as boolean;
        }
        this.accessType = this.accessTypes?.[0];
        if (!this.orgs) {
            const r = await toPromise(
                this.http.get<IGroup[]>("/groups/getOrgs")
            );
            if (r.ok) {
                this.orgs = r.result;
            }
        }
        if (this.orgs) {
            this.selectedOrg = this.orgs.find(
                (o) => o.name === genericglobals().homeOrganization + " users"
            );
        }

        if (this.massMode) {
            this.addingRight = true;
            this.setItems(await this.getItemsAndPreprocess());
            this.gridReady = true;
        }
    }

    // region Mass mode item table

    private async getItemsAndPreprocess(): Promise<IItemRow[]> {
        return (await this.getItems()).map((i) => ({
            ...i,
            relPath: this.relPath(i),
            rightsStr: this.formatRights(i),
        }));
    }

    private setItems(items: IItemRow[]) {
        this.items = items;
        // The selection is cleared whenever the items are reloaded.
        this.selectedIds.clear();
        this.updateVisibleItems();
    }

    updateVisibleItems() {
        const filters = this.gridColumns
            .map((c) => ({
                field: c.field,
                text: this.gridFilters[c.field].trim().toLowerCase(),
            }))
            .filter((f) => f.text !== "");
        const shown = this.items.filter((i) =>
            filters.every((f) => i[f.field].toLowerCase().includes(f.text))
        );
        const field = this.sortField;
        const dir = this.sortAscending ? 1 : -1;
        shown.sort(
            (a, b) =>
                dir *
                a[field].localeCompare(b[field], undefined, {
                    sensitivity: "base",
                    numeric: true,
                })
        );
        this.visibleItems = shown;
    }

    sortBy(field: GridField) {
        if (this.sortField === field) {
            this.sortAscending = !this.sortAscending;
        } else {
            this.sortField = field;
            this.sortAscending = true;
        }
        this.updateVisibleItems();
    }

    trackItem(index: number, item: IItemRow) {
        return item.id;
    }

    toggleSelected(item: IItemRow) {
        if (!item.rights.manage) {
            return;
        }
        if (this.selectedIds.has(item.id)) {
            this.selectedIds.delete(item.id);
        } else {
            this.selectedIds.add(item.id);
        }
    }

    private selectableVisibleItems() {
        return this.visibleItems.filter((i) => i.rights.manage);
    }

    allVisibleSelected() {
        const selectable = this.selectableVisibleItems();
        return (
            selectable.length > 0 &&
            selectable.every((i) => this.selectedIds.has(i.id))
        );
    }

    toggleSelectAll(select: boolean) {
        for (const i of this.selectableVisibleItems()) {
            if (select) {
                this.selectedIds.add(i.id);
            } else {
                this.selectedIds.delete(i.id);
            }
        }
    }

    async activeBoxChanged() {
        this.updateShownRights();
        if (!this.massMode || !this.gridReady) {
            return;
        }
        this.activeBoxLoading = true;
        this.setItems(await this.getItemsAndPreprocess());
        this.activeBoxLoading = false;
    }

    // endregion

    /**
     * Returns the rights of the given type that should be shown, sorted by the name of the user or group.
     */
    rightsOfType(type: IAccessType): IRight[] {
        return this.shownRights.get(type.id) ?? [];
    }

    private updateShownRights() {
        this.shownRights.clear();
        for (const right of this.grouprights ?? []) {
            if (this.showActiveOnly && this.isObsolete(right)) {
                continue;
            }
            const list = this.shownRights.get(right.type);
            if (list) {
                list.push(right);
            } else {
                this.shownRights.set(right.type, [right]);
            }
        }
        for (const list of this.shownRights.values()) {
            list.sort(
                (a, b) =>
                    compareLikeOrderBy(
                        a.usergroup.personal_user?.real_name,
                        b.usergroup.personal_user?.real_name
                    ) || compareLikeOrderBy(a.usergroup.name, b.usergroup.name)
            );
        }
    }

    private focusEditor() {
        // The edit area becomes visible only after the next change detection.
        setTimeout(() => {
            const el = this.listMode
                ? this.groupNameArea?.nativeElement
                : this.groupNameInput?.nativeElement;
            el?.focus();
        });
    }

    showAddRightFn(type: IAccessType | undefined, e: Event) {
        this.accessType = type;
        this.selectedRight = null;
        this.addingRight = true;
        this.focusEditor();
        e.preventDefault();
    }

    async removeConfirm(group: IRight) {
        if (window.confirm(`Remove ${this.getConfirmDesc(group)}?`)) {
            this.removingRight = group;
            await this.removeRight(group);
            this.removingRight = undefined;
        }
    }

    private getConfirmDesc(group: IRight) {
        return `${
            this.findAccessTypeById(group.type)!.name
        } right from ${getGroupDesc(group.usergroup)}`;
    }

    get urlRootGet() {
        if (this.defaultItem) {
            return `defaultPermissions/${this.defaultItem}`;
        }
        return "permissions";
    }

    get urlRootModify() {
        if (this.defaultItem) {
            return `defaultPermissions`;
        }
        return "permissions";
    }

    async getPermissions() {
        if (!this.itemId) {
            return;
        }
        this.loading = true;
        const r = await toPromise(
            this.http.get<{grouprights: IRight[]; accesstypes: IAccessType[]}>(
                `/${this.urlRootGet}/get/${this.itemId}`
            )
        );
        this.loading = false;
        if (r.ok) {
            const data = r.result;
            this.grouprights = data.grouprights;
            this.updateShownRights();
            if (data.accesstypes) {
                let accessTypes = data.accesstypes;
                accessTypes.sort(
                    (a1, a2) => accessOrder[a1.name] - accessOrder[a2.name]
                );
                const restrict = this.restrictRights;
                if (Array.isArray(restrict)) {
                    accessTypes = accessTypes.filter((a) =>
                        restrict.includes(a.name)
                    );
                }
                this.accessTypes = accessTypes;
                if (!this.accessType) {
                    this.accessType = this.accessTypes[0];
                }
            }
        } else {
            this.reportError("Could not fetch permissions.");
        }
    }

    async removeRight(right: IRight, refresh = true) {
        const r = await toPromise(
            this.http.put(`/${this.urlRootModify}/remove`, {
                group: right.usergroup.id,
                id: this.itemId,
                item_type: this.defaultItem,
                type: right.type,
                edit_velp_group_perms: this.editVelpGroupPermissions,
                edit_translation_perms: this.editTranslationPermissions,
            })
        );
        return await this.handleResult(r, refresh);
    }

    private async handleResult(
        r: Result<unknown, AngularError>,
        refresh: boolean
    ) {
        let result;
        if (r.ok) {
            if (refresh) {
                await this.getPermissions();
            }
            result = true;
        } else {
            this.reportError(r.result.error.error);
            result = false;
        }
        this.selectedRight = null;
        return result;
    }

    cancel() {
        this.addingRight = false;
        this.selectedRight = null;
    }

    editingRight() {
        return this.selectedRight != null;
    }

    actionText(): string {
        switch (this.actionOption) {
            case ActionOption.Add:
                return "Add";
            case ActionOption.Confirm:
                return "Confirm";
            case ActionOption.Expire:
                return "Expire";
            case ActionOption.Remove:
                return "Remove";
        }
    }

    addDisabled() {
        return (
            this.loading ||
            (!!this.massMode && this.gridReady && this.selectedIds.size === 0)
        );
    }

    /**
     * Purpose for this is to use clipboard data to give rights
     * but could not read clipboard, so just clearInput
     */
    paste() {
        if (!this.confirmExpire) {
            return;
        } // clearInput did not work?
        this.groupName = "";
    }

    sameAccessType(a: IAccessType | undefined, b: IAccessType | undefined) {
        return a?.id === b?.id;
    }

    submitForm(e: Event) {
        e.preventDefault();
        void this.addOrEditPermission(this.groupName, this.accessType);
    }

    /**
     * The time options in the form that the server expects.
     */
    private getTimeOpt() {
        return {
            ...this.timeOpt,
            duration: moment.duration(
                this.durOpt.durationAmount,
                this.durOpt.durationType
            ),
        };
    }

    async addOrEditPermission(
        groupname: string | undefined,
        type: IAccessType | undefined
    ) {
        if (!groupname || !type) {
            return;
        }
        this.clearMessages();
        const timeOpt = this.getTimeOpt();
        if (this.durationSelected()) {
            timeOpt.to = this.durOpt.accessTo;
        }
        if (this.massMode) {
            if (!this.gridReady) {
                console.error("grid not initialized");
                return;
            }
            const ids = this.items
                .filter((i) => this.selectedIds.has(i.id))
                .map((i) => i.id);
            this.loading = true;
            const r = await toPromise(
                this.http.put<IPermissionEditResponse>(`/permissions/edit`, {
                    ids: ids,
                    time: timeOpt,
                    type: type.id,
                    action: this.actionOption,
                    groups: groupname.split(/[;\n]/),
                    confirm: this.getEffectiveConfirm(),
                    edit_velp_group_perms: this.editVelpGroupPermissions,
                    edit_translation_perms: this.editTranslationPermissions,
                })
            );
            if (r.ok) {
                this.showNotExistWarning(r.result.not_exist);
                this.setItems(await this.getItemsAndPreprocess());
                this.successMsg = "Rights updated.";
            } else {
                this.errMsg = r.result.error.error;
            }
            this.loading = false;
        } else {
            let groups = groupname.split(/[\n;]/).map((n) => n.trim());
            if (groups.every((g) => g.toUpperCase() === g)) {
                groups = groups.map((g) => g.toLowerCase());
            }
            if (this.barcodeMode) {
                groups = groups.map((g) => g.replace(/^[# ]+/, ""));
                groups = groups.map((g) => g.replace(/#/g, "@"));
                groups = groups.map((g) => g.replace(/\/c/g, "@"));
            }
            if (this.actionOption === ActionOption.Add) {
                this.loading = true;
                const r = await toPromise(
                    this.http.put<IPermissionEditResponse>(
                        `/${this.urlRootModify}/add`,
                        {
                            time: timeOpt,
                            id: this.itemId,
                            groups: groups,
                            type: type.id,
                            confirm: this.getEffectiveConfirm(),
                            item_type: this.defaultItem,
                            edit_velp_group_perms:
                                this.editVelpGroupPermissions,
                            edit_translation_perms:
                                this.editTranslationPermissions,
                        }
                    )
                );
                this.loading = false;
                if (r.ok) {
                    this.showNotExistWarning(r.result.not_exist);
                    await this.getPermissions();
                    if (this.barcodeMode) {
                        this.handleSuccessBarcode(
                            type,
                            groups,
                            r.result.not_exist
                        );
                    } else {
                        if (!this.errMsg) {
                            this.cancel();
                        }
                    }
                } else {
                    this.reportError(r.result.error.error);
                }
            } else {
                let func;
                switch (this.actionOption) {
                    case ActionOption.Confirm:
                        func = (right: IRight) =>
                            this.confirmRight(right, false);
                        break;
                    case ActionOption.Expire:
                        func = (right: IRight) =>
                            this.expireRight(right, false);
                        break;
                    case ActionOption.Remove:
                        func = (right: IRight) =>
                            this.removeRight(right, false);
                        break;
                    default:
                        throw Error("unreachable");
                }
                const notFound = [];
                const successes = [];
                for (const g of groups) {
                    const right = this.grouprights!.find(
                        (r) => r.usergroup.name === g && r.type === type.id
                    );
                    if (right) {
                        const success = await func(right);
                        if (success) {
                            successes.push(g);
                        }
                    } else {
                        notFound.push(g);
                    }
                }
                if (notFound.length > 0) {
                    this.reportError(
                        `Some usergroups were not in current ${
                            type.name
                        } rights list: ${notFound.join(", ")}`
                    );
                }
                if (successes.length > 0) {
                    await this.getPermissions();
                    if (this.barcodeMode) {
                        this.handleSuccessBarcode(type, groups, notFound);
                    }
                }
            }
            if (!this.barcodeMode && !this.errMsg) {
                await timeout();
                this.element.nativeElement
                    .querySelector<HTMLElement>(".rights-list a")
                    ?.focus();
            }
            if (this.editingRight()) {
                this.selectedRight = null;
            }
        }
    }

    private getEffectiveConfirm() {
        return (
            this.requireConfirm &&
            (this.durationSelected() || this.rangeSelected())
        );
    }

    private clearMessages() {
        this.errMsg = undefined;
        this.successMsg = undefined;
    }

    private handleSuccessBarcode(
        type: IAccessType,
        requestedGroups: string[],
        notFoundGroups: string[]
    ) {
        this.groupName = "";
        const notExistSet = new Set(notFoundGroups);
        const groups = requestedGroups.filter((g) => !notExistSet.has(g));
        if (groups.length === 0) {
            return;
        }
        const imperative = capitalizeFirstLetter(
            this.actionOption + (this.actionOption.endsWith("e") ? "d" : "ed")
        );
        this.successMsg = `${imperative} ${type.name} right for: ${groups.join(
            ", "
        )}`;
    }

    private showNotExistWarning(r: string[]) {
        if (r.length > 0) {
            this.reportError(`Some usergroups were not found: ${r.join(", ")}`);
        }
    }

    durationSelected() {
        return this.timeOpt.type == "duration";
    }

    rangeSelected() {
        return this.timeOpt.type == "range";
    }

    getPlaceholder() {
        return (
            "enter username(s)/group name(s) separated by semicolons" +
            (this.listMode ? " or newlines" : "")
        );
    }

    getGroupDesc(group: IRight) {
        return getGroupDesc(group.usergroup);
    }

    formatDate(date: moment.MomentInput) {
        return date ? moment(date).format("DD.MM.YYYY HH:mm:ss") : "";
    }

    formatRelative(date: moment.MomentInput) {
        return date ? moment(date).fromNow() : "";
    }

    formatDuration(duration: moment.Duration | string | null) {
        return moment.duration(duration).humanize();
    }

    formatPreciseDuration(duration: moment.Duration | string | null) {
        return humanizeDuration(moment.duration(duration).asMilliseconds());
    }

    shouldShowBeginTime(group: IRight) {
        // having -1 here (instead of 0) avoids "begins in a few seconds" right after adding a right
        return moment().diff(group.accessible_from, "seconds") < -1;
    }

    shouldShowEndTime(group: IRight) {
        return (
            group.accessible_to != null &&
            moment().diff(group.accessible_to) <= 0
        );
    }

    shouldShowEndedTime(group: IRight) {
        return (
            group.accessible_to != null &&
            moment().diff(group.accessible_to) > 0
        );
    }

    shouldShowDuration(group: IRight) {
        return group.duration != null && group.accessible_from == null;
    }

    shouldShowUnlockable(group: IRight) {
        return (
            group.duration != null &&
            group.duration_from != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_from) < 0
        );
    }

    shouldShowNotUnlockable(group: IRight) {
        return (
            group.duration != null &&
            group.duration_to != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_to) <= 0
        );
    }

    shouldShowNotUnlockableAnymore(group: IRight) {
        return (
            group.duration != null &&
            group.duration_to != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_to) > 0
        );
    }

    isObsolete(group: IRight) {
        return (
            this.shouldShowEndedTime(group) ||
            this.shouldShowNotUnlockableAnymore(group)
        );
    }

    async expireRight(group: IRight, refresh = true) {
        if (!this.accessType) {
            this.reportError("Access type not selected.");
            return false;
        }
        if (this.shouldShowEndedTime(group)) {
            this.reportError(
                `${this.findAccessTypeById(group.type)!.name} right for ${
                    group.usergroup.name
                } is already expired.`
            );
            return false;
        }
        if (refresh) {
            if (
                this.confirmExpire &&
                !window.confirm(`Expire ${this.getConfirmDesc(group)}?`)
            ) {
                return;
            }
        }
        this.loading = true;
        this.expiringRight = group;
        const r = await toPromise(
            this.http.put<IPermissionEditResponse>(
                `/${this.urlRootModify}/add`,
                {
                    time: {
                        ...this.getTimeOpt(),
                        to: moment(),
                        type: "range",
                    },
                    id: this.itemId,
                    groups: [group.usergroup.name],
                    type: group.type,
                    confirm: false,
                    item_type: this.defaultItem,
                    edit_velp_group_perms: this.editVelpGroupPermissions,
                    edit_translation_perms: this.editTranslationPermissions,
                }
            )
        );
        this.loading = false;
        this.expiringRight = undefined;
        return await this.handleResult(r, refresh);
    }

    async confirmRight(group: IRight, refresh = true) {
        this.confirmingRight = group;
        this.loading = true;
        const r = await toPromise(
            this.http.put("/permissions/confirm", {
                group: group.usergroup.id,
                id: this.itemId,
                type: group.type,
            })
        );
        const result = await this.handleResult(r, refresh);
        this.loading = false;
        this.confirmingRight = undefined;
        return result;
    }

    findAccessTypeById(id: number) {
        if (!this.accessTypes) {
            return;
        }
        return this.accessTypes.find((a) => a.id === id);
    }

    async editRight(group: IRight) {
        this.setEditFields(group);
        const section = this.editArea?.nativeElement;
        if (section) {
            await timeout();
            section.scrollIntoView({block: "nearest"});
        }
    }

    private setEditFields(group: IRight) {
        this.groupName = group.usergroup.name;
        this.accessType = this.findAccessTypeById(group.type);
        this.addingRight = false;
        this.selectedRight = group;
        this.lastEdited = group;

        const toDate = (m: moment.MomentInput) =>
            m ? moment(m).toDate() : undefined;
        this.timeOpt.durationFrom = toDate(group.duration_from);
        this.timeOpt.durationTo = toDate(group.duration_to);
        this.timeOpt.from = toDate(group.accessible_from);
        this.timeOpt.to = toDate(group.accessible_to);
        this.requireConfirm = group.require_confirm ?? false;

        if (group.duration && group.accessible_from == null) {
            const d = moment.duration(group.duration);
            this.timeOpt.type = "duration";
            for (let i = durationTypes.length - 1; i >= 0; --i) {
                const amount = d.as(durationTypes[i]);
                if (Math.floor(amount) === amount || i === 0) {
                    // preserve last duration type choice if the amount is zero
                    if (amount !== 0) {
                        this.durOpt.durationType = durationTypes[i];
                    }
                    this.durOpt.durationAmount = amount;
                    break;
                }
            }
            this.durOpt.accessTo = this.timeOpt.to;
        } else {
            this.timeOpt.type = "range";
        }
    }

    private async getItems() {
        const r = await toPromise(
            this.http.get<IItemWithRights[]>(
                withQueryParams("/getItems", {
                    folder_id: this.itemId,
                    recursive: true,
                    include_rights: true,
                })
            )
        );
        if (!r.ok) {
            throw Error("getItems failed");
        }
        return r.result;
    }

    private formatRights(i: IItemWithRights) {
        if (!i.rights.manage) {
            return "(you don't have manage right)";
        }
        let str = "";
        let currentRight;
        let typeSep = "";
        for (const r of i.grouprights) {
            if (this.isObsolete(r) && this.showActiveOnly) {
                continue;
            }
            let groupSep = ", ";
            // Assuming the rights list is ordered by access type.
            if (r.type != currentRight) {
                str += `${typeSep}${
                    this.findAccessTypeById(r.type)!.name[0]
                }: `;
                currentRight = r.type;
                groupSep = "";
                typeSep = " | ";
            }
            str += `${groupSep}${r.usergroup.name}`;
            if (this.isObsolete(r)) {
                str += " (e)"; // means "ended"
            } else if (this.shouldShowDuration(r)) {
                str += " (d)"; // means "duration"
            } else if (this.shouldShowEndTime(r)) {
                str += " (t)"; // means "timed"
            } else if (this.shouldShowBeginTime(r)) {
                str += " (f)"; // means "future"
            }
        }
        return str;
    }

    private reportError(s: string) {
        this.errMsg = s;
    }
}
