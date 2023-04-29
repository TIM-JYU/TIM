import type {IController, IHttpResponse, IScope} from "angular";
import moment from "moment";
import {timApp} from "tim/app";
import * as focusMe from "tim/ui/focusMe";
import type {Binding, Result} from "tim/util/utils";
import {
    capitalizeFirstLetter,
    dateFormat,
    getGroupDesc,
    markAsUsed,
    to,
} from "tim/util/utils";

import {durationTypes} from "tim/ui/duration-picker.component";
import type {IGroup} from "tim/user/IUser";
import {genericglobals, itemglobals} from "tim/util/globals";
import {$http, $timeout} from "tim/util/ngimport";
import type {
    IAccessType,
    IItemWithRights,
    IRight,
} from "tim/item/access-role.service";
import {accessOrder} from "tim/item/access-role.service";

markAsUsed(focusMe);

const tips = {
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

class RightsEditorController implements IController {
    static $inject = ["$scope", "$element"];
    private durOpt: {
        durationType: moment.unitOfTime.Base;
        durationAmount: number;
        accessTo?: moment.Moment;
    };
    private timeOpt: {
        type: string;
        duration?: moment.Duration;
        to?: moment.Moment;
        from?: moment.Moment;
        durationTo?: moment.Moment;
        durationFrom?: moment.Moment;
    };
    private grouprights?: IRight[];
    private showActiveOnly: boolean;
    private editVelpGroupPermissions: boolean;
    private editTranslationPermissions: boolean;
    private selectedRight: IRight | null;
    private datePickerOptionsFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationAccessTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private accessTypes!: IAccessType[];
    private massMode: Binding<boolean | undefined, "<">;
    private accessType: IAccessType | undefined;
    private addingRight: boolean = false;
    private focusEditor: boolean = false;
    private itemId: Binding<number | undefined, "<">;
    private listMode: boolean = false;
    private groupName: string | undefined;
    private gridOptions?: uiGrid.IGridOptionsOf<IItemWithRights>;
    private action?: ActionOption;
    private actionOption = ActionOption.Add;
    private grid?: uiGrid.IGridApiOf<IItemWithRights>;
    private gridReady = false;
    private errMsg?: string;
    private successMsg?: string;
    private loading = false;
    private orgs?: IGroup[];
    private selectedOrg?: IGroup;
    private requireConfirm = false;
    private defaultItem?: string;
    private barcodeMode?: boolean;
    private restrictRights?: string[];
    private hideRemove?: boolean;
    private forceDuration?: number;
    private forceDurationStart?: string;
    private forceDurationEnd?: string;
    private forceDurationAccessTo?: string;
    private forceConfirm?: boolean;
    private hideEdit?: boolean;
    private hideExpire?: boolean;
    private lastEdited?: IRight;
    private confirmingRight?: IRight;
    private expiringRight?: IRight;
    private removingRight?: IRight;
    private confirmExpire?: boolean;
    private clearInput?: boolean;
    private activeBoxLoading = false;
    private tips = tips;

    constructor(private scope: IScope, private element: JQLite) {
        this.timeOpt = {type: "always"};
        this.durOpt = {durationType: "hours", durationAmount: 4};
        this.selectedRight = null;
        this.showActiveOnly = true;
        this.editVelpGroupPermissions = true;
        this.editTranslationPermissions = true;
        this.datePickerOptionsFrom = {
            format: dateFormat,
            defaultDate: moment(),
            showTodayButton: true,
        };
        this.datePickerOptionsTo = {
            format: dateFormat,
            defaultDate: moment(),
            showTodayButton: true,
        };
        this.datePickerOptionsDurationFrom = {
            format: dateFormat,
            showTodayButton: true,
        };
        this.datePickerOptionsDurationTo = {
            format: dateFormat,
            showTodayButton: true,
        };
        this.datePickerOptionsDurationAccessTo = {
            format: dateFormat,
            showTodayButton: true,
        };
    }

    relPath(item: IItemWithRights) {
        return item.path.slice(itemglobals().curr_item.path.length + 1);
    }

    async $onInit() {
        this.actionOption = this.action ?? ActionOption.Add;
        if (!this.accessTypes || !this.massMode) {
            await this.getPermissions();
        }
        if (this.forceDuration) {
            this.timeOpt.type = "duration";
            this.durOpt.durationAmount = this.forceDuration;
            this.durOpt.durationType = "hours";
            if (this.forceDurationStart) {
                this.timeOpt.durationFrom = moment(this.forceDurationStart);
            }
            if (this.forceDurationEnd) {
                this.timeOpt.durationTo = moment(this.forceDurationEnd);
            }
            if (this.forceDurationAccessTo) {
                this.durOpt.accessTo = moment(this.forceDurationAccessTo);
            }
        }
        if (this.forceConfirm != null) {
            this.requireConfirm = this.forceConfirm;
        }
        this.accessType = this.accessTypes[0];
        if (!this.orgs) {
            const r = await to($http.get<IGroup[]>("/groups/getOrgs"));
            if (r.ok) {
                this.orgs = r.result.data;
            }
        }
        if (this.orgs) {
            this.selectedOrg = this.orgs.find(
                (o) => o.name === genericglobals().homeOrganization + " users"
            );
        }

        if (this.massMode) {
            this.addingRight = true;
            const data = await this.getItemsAndPreprocess();
            this.gridOptions = {
                onRegisterApi: (gridApi) => {
                    this.grid = gridApi;
                },
                data: data,
                enableSorting: true,
                enableFiltering: true,
                enableFullRowSelection: true,
                minRowsToShow: Math.min(data.length, 20),
                enableGridMenu: true,
                enableHorizontalScrollbar: false,
                isRowSelectable: (row) => {
                    const i = (
                        row as unknown as uiGrid.IGridRowOf<IItemWithRights>
                    ).entity;
                    return i.rights.manage;
                },
                columnDefs: [
                    {
                        field: "title",
                        name: "Title",
                        allowCellFocus: false,
                    },
                    {
                        name: "Relative path",
                        allowCellFocus: false,
                        field: "relPath",
                        sort: {direction: "asc"},
                        cellTemplate: `<div class="ui-grid-cell-contents"
                                            title="TOOLTIP">
                                            <i ng-if="row.entity.isFolder" class="glyphicon glyphicon-folder-open"></i>
                                            <a href="/manage/{{grid.appScope.$ctrl.manageLink(row)}}">
                                                {{row.entity.relPath}}
                                            </a>
                                       </div>`,
                    },
                    {
                        field: "rightsStr",
                        name: "Rights",
                        allowCellFocus: false,
                        cellTooltip: true,
                    },
                ],
            };
            this.gridReady = true;
        }

        this.scope.$watchGroup(
            [() => this.durOpt.durationAmount, () => this.durOpt.durationType],
            (newValues, oldValues, scope) => {
                this.timeOpt.duration = moment.duration(
                    this.durOpt.durationAmount,
                    this.durOpt.durationType
                );
            }
        );
    }

    private async getItemsAndPreprocess() {
        return (await this.getItems()).map((i) => ({
            ...i,
            relPath: this.relPath(i),
            rightsStr: this.formatRights(i),
        }));
    }

    async activeBoxChanged() {
        if (!this.massMode || !this.gridOptions) {
            return;
        }
        this.activeBoxLoading = true;
        this.gridOptions.data = await this.getItemsAndPreprocess();
        this.activeBoxLoading = false;
    }

    manageLink(row: uiGrid.IGridRowOf<IItemWithRights>) {
        return row.entity.path;
    }

    showAddRightFn(type: IAccessType, e: Event) {
        this.accessType = type;
        this.selectedRight = null;
        this.addingRight = true;
        this.focusEditor = true;
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
        const r = await to(
            $http.get<{grouprights: IRight[]; accesstypes: IAccessType[]}>(
                `/${this.urlRootGet}/get/${this.itemId}`
            )
        );
        this.loading = false;
        if (r.ok) {
            const data = r.result.data;
            this.grouprights = data.grouprights;
            if (data.accesstypes) {
                this.accessTypes = data.accesstypes;
                this.accessTypes.sort(
                    (a1, a2) => accessOrder[a1.name] - accessOrder[a2.name]
                );
                if (this.restrictRights) {
                    this.accessTypes = this.accessTypes.filter((a) =>
                        this.restrictRights!.includes(a.name)
                    );
                }
                if (!this.accessType) {
                    this.accessType = this.accessTypes[0];
                }
            }
        } else {
            this.reportError("Could not fetch permissions.");
        }
    }

    async removeRight(right: IRight, refresh = true) {
        const r = await to(
            $http.put(`/${this.urlRootModify}/remove`, {
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
        r: Result<IHttpResponse<unknown>, {data: {error: string}}>,
        refresh: boolean
    ) {
        let result;
        if (r.ok) {
            if (refresh) {
                await this.getPermissions();
            }
            result = true;
        } else {
            this.reportError(r.result.data.error);
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
            (this.massMode &&
                this.grid &&
                this.grid.selection.getSelectedRows().length === 0)
        );
    }

    /**
     * Purpose for this is to use clipboard data to give rights
     * but could not read clipboard, so just clearInput
     */
    paste() {
        // navigator.permissions.query({name: "clipboard-read"});  // does not compile
        if (!this.confirmExpire) {
            return;
        } // clearInput did not work?
        this.groupName = "";
        /*
        const input = this.element.filter("#groupName");
        input.focus();
        document.execCommand("selectAll");
        document.execCommand("paste");
        // if (!navigator.clipboard) { return; }
        // const text = await navigator.clipboard.readText();
        const text = this.groupName;
        if (!this.accessType || !text) { return; }
        // this.addOrEditPermission(text, this.accessType);
         */
    }

    async addOrEditPermission(groupname: string, type: IAccessType) {
        this.clearMessages();
        const timeOpt = {...this.timeOpt};
        if (this.durationSelected()) {
            timeOpt.to = this.durOpt.accessTo;
        }
        if (this.massMode) {
            if (!this.grid || !this.gridOptions) {
                console.error("grid not initialized");
                return;
            }
            const ids = this.grid.selection.getSelectedRows().map((i) => i.id);
            this.loading = true;
            const r = await to(
                $http.put<IPermissionEditResponse>(`/permissions/edit`, {
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
                this.showNotExistWarning(r.result.data.not_exist);
                this.gridOptions.data = await this.getItemsAndPreprocess();
                this.successMsg = "Rights updated.";
            } else {
                this.errMsg = r.result.data.error;
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
                const r = await to(
                    $http.put<IPermissionEditResponse>(
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
                    this.showNotExistWarning(r.result.data.not_exist);
                    await this.getPermissions();
                    if (this.barcodeMode) {
                        this.handleSuccessBarcode(
                            type,
                            groups,
                            r.result.data.not_exist
                        );
                    } else {
                        if (!this.errMsg) {
                            this.cancel();
                        }
                    }
                } else {
                    this.reportError(r.result.data.error);
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
                await $timeout();
                this.element.find(".rights-list a").first().focus();
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

    obsoleteFilterFn = (group: IRight) => {
        return !this.showActiveOnly || !this.isObsolete(group);
    };

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
        const r = await to(
            $http.put<IPermissionEditResponse>(`/${this.urlRootModify}/add`, {
                time: {
                    ...this.timeOpt,
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
            })
        );
        this.loading = false;
        this.expiringRight = undefined;
        return await this.handleResult(r, refresh);
    }

    async confirmRight(group: IRight, refresh = true) {
        this.confirmingRight = group;
        this.loading = true;
        const r = await to(
            $http.put("/permissions/confirm", {
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
        const section = this.element.find(".rights-edit-area")[0];
        if (section) {
            await $timeout();
            section.scrollIntoView({block: "nearest"});
        }
    }

    private setEditFields(group: IRight) {
        this.groupName = group.usergroup.name;
        this.accessType = this.findAccessTypeById(group.type);
        this.addingRight = false;
        this.selectedRight = group;
        this.lastEdited = group;

        if (group.duration_from) {
            this.timeOpt.durationFrom = moment(group.duration_from);
        } else {
            this.timeOpt.durationFrom = undefined;
        }
        if (group.duration_to) {
            this.timeOpt.durationTo = moment(group.duration_to);
        } else {
            this.timeOpt.durationTo = undefined;
        }

        if (group.accessible_from) {
            this.timeOpt.from = moment(group.accessible_from);
        } else {
            this.timeOpt.from = undefined;
        }
        if (group.accessible_to) {
            this.timeOpt.to = moment(group.accessible_to);
        } else {
            this.timeOpt.to = undefined;
        }
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
        const r = await to(
            $http.get<IItemWithRights[]>("/getItems", {
                params: {
                    folder_id: this.itemId,
                    recursive: true,
                    include_rights: true,
                },
            })
        );
        if (!r.ok) {
            throw Error("getItems failed");
        }
        return r.result.data;
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

timApp.component("timRightsEditor", {
    bindings: {
        accessTypes: "<?",
        action: "@?",
        allowSelectAction: "<?",
        barcodeMode: "<?",
        clearInput: "<?",
        confirmExpire: "<?",
        defaultItem: "@?",
        forceConfirm: "<?",
        forceDuration: "<?",
        forceDurationEnd: "<?",
        forceDurationStart: "<?",
        forceDurationAccessTo: "<?",
        hideEdit: "<?",
        hideExpire: "<?",
        hideRemove: "<?",
        itemId: "<?",
        massMode: "<?",
        orgs: "<?",
        restrictRights: "<?",
    },
    controller: RightsEditorController,
    templateUrl: "/static/templates/rightsEditor.html",
});
