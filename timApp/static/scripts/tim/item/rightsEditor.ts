import {IController, IScope} from "angular";
import moment, {Duration, Moment} from "moment";
import {timApp} from "tim/app";
import * as focusMe from "tim/ui/focusMe";
import {Binding, dateFormat, markAsUsed, to} from "tim/util/utils";
import {showMessageDialog} from "../ui/dialog";
import {durationTypes} from "../ui/durationPicker";
import {IGroup} from "../user/IUser";
import {itemglobals} from "../util/globals";
import {$http} from "../util/ngimport";
import {IItem} from "./IItem";

markAsUsed(focusMe);

export interface IRight {
    type: number;
    duration_to: Moment | null;
    duration_from: Moment | null;
    duration: null | Duration;
    accessible_to: Moment;
    accessible_from: Moment;
    usergroup: IGroup;
}

export interface IAccessType {
    id: number;
    name: string;
}

interface IItemWithRights extends IItem {
    grouprights: IRight[];
}

enum MassOption {
    Add,
    Remove,
}

interface IPermissionEditResponse {
    edited: number[];
}

function isVelpGroupItem(i: IItemWithRights) {
    return i.path.indexOf("/velp-groups/") >= 0 || i.path.endsWith("/velp-groups");
}

class RightsEditorController implements IController {
    static $inject = ["$scope"];
    private durOpt: {
        durationType: moment.unitOfTime.Base,
        durationAmount: number,
    };
    private timeOpt: {
        type: string,
        duration?: moment.Duration,
        to?: moment.Moment,
        from?: moment.Moment,
        durationTo?: moment.Moment,
        durationFrom?: moment.Moment,
    };
    private grouprights?: IRight[];
    private showActiveOnly: boolean;
    private selectedRight: IRight | null;
    private datePickerOptionsFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private accessTypes: Binding<IAccessType[] | undefined, "<">;
    private massMode: Binding<boolean | undefined, "<">;
    private accessType: IAccessType | undefined;
    private addingRight: boolean = false;
    private focusEditor: boolean = false;
    private urlRoot: Binding<string | undefined, "<">;
    private itemId: Binding<number | undefined, "<">;
    private listMode: boolean = false;
    private groupName: string | undefined;
    private scope: IScope;
    private gridOptions?: uiGrid.IGridOptionsOf<IItemWithRights>;
    private massOption = MassOption.Add;
    private grid?: uiGrid.IGridApiOf<IItemWithRights>;
    private gridReady = false;
    private msg?: string;
    private severity?: "danger" | "success";
    private loading = false;

    constructor(scope: IScope) {
        this.scope = scope;
        this.timeOpt = {type: "always"};
        this.durOpt = {durationType: "hours", durationAmount: 4};
        this.selectedRight = null;
        this.showActiveOnly = true;
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
    }

    hi() {
        return "hi";
    }

    relPath(item: IItemWithRights) {
        return item.path.slice(itemglobals().item.path.length + 1);
    }

    accessTypeById(id: number) {
        if (!this.accessTypes) {
            return;
        }
        return this.accessTypes.find((a) => a.id === id);
    }

    async $onInit() {
        if (this.accessTypes) {
            this.accessType = this.accessTypes[0];
        }
        if (!this.massMode) {
            this.getPermissions();
        } else {
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
                    const i = (row as unknown as uiGrid.IGridRowOf<IItemWithRights>).entity;
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

        this.scope.$watchGroup([() => this.durOpt.durationAmount, () => this.durOpt.durationType], (newValues, oldValues, scope) => {
            this.timeOpt.duration = moment.duration(this.durOpt.durationAmount, this.durOpt.durationType);
        });
    }

    private async getItemsAndPreprocess() {
        return (await this.getItems()).map((i) => ({
            ...i,
            relPath: this.relPath(i),
            rightsStr: this.formatRights(i),
        }));
    }

    manageLink(row: uiGrid.IGridRowOf<IItemWithRights>) {
        return row.entity.path;
    }

    showAddRightFn(type: IAccessType) {
        this.accessType = type;
        this.selectedRight = null;
        this.addingRight = true;
        this.focusEditor = true;
    }

    removeConfirm(group: IRight, type: string) {
        if (window.confirm("Remove " + type + " right from " + group.usergroup.name + "?")) {
            this.removePermission(group, type);
        }
    }

    async getPermissions() {
        if (!this.urlRoot || !this.itemId) {
            return;
        }
        const r = await to($http.get<{grouprights: IRight[], accesstypes: IAccessType[]}>(`/${this.urlRoot}/get/${this.itemId}`));
        if (r.ok) {
            const data = r.result.data;
            this.grouprights = data.grouprights;
            if (data.accesstypes) {
                this.accessTypes = data.accesstypes;
                if (!this.accessType) {
                    this.accessType = this.accessTypes[0];
                }
            }
        } else {
            await showMessageDialog("Could not fetch permissions.");
        }
    }

    async removePermission(right: IRight, type: string) {
        const r = await to($http.put(`/${this.urlRoot}/remove/${this.itemId}/${right.usergroup.id}/${type}`, {}));
        if (r.ok) {
            await this.getPermissions();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    cancel() {
        this.addingRight = false;
        this.selectedRight = null;
    }

    editingRight() {
        return this.selectedRight != null;
    }

    addOrRemove() {
        return this.massMode ? (this.massOption == MassOption.Add ? "Add" : "Remove") : "Add";
    }

    addDisabled() {
        return this.loading || (this.massMode && this.grid && this.grid.selection.getSelectedRows().length === 0);
    }

    async addOrEditPermission(groupname: string, type: IAccessType) {
        if (this.massMode) {
            if (!this.grid || !this.gridOptions) {
                console.error("grid not initialized");
                return;
            }
            const ids = this.grid.selection.getSelectedRows().map((i) => i.id);
            this.msg = undefined;
            this.loading = true;
            const r = await to($http.put<IPermissionEditResponse>(`/permissions/edit`, {
                ids: ids,
                time: this.timeOpt,
                type: type.name,
                action: this.massOption,
                groups: groupname.split(/[;\n]/),
            }));
            if (r.ok) {
                this.gridOptions.data = await this.getItemsAndPreprocess();
                this.msg = "Rights updated.";
                this.severity = "success";
            } else {
                this.msg = r.result.data.error;
                this.severity = "danger";
            }
            this.loading = false;
        } else {
            const groupstr = groupname.split("\n").join(";");
            const r = await to($http.put(`/${this.urlRoot}/add/${this.itemId}/${groupstr}/${type.name}`,
                this.timeOpt));
            if (r.ok) {
                this.getPermissions();
                this.cancel();
            } else {
                await showMessageDialog(r.result.data.error);
            }
        }

    }

    getPlaceholder() {
        return "enter username(s)/group name(s) separated by semicolons" + (this.listMode ? " or newlines" : "");
    }

    getGroupDesc(group: IRight) {
        return group.usergroup.personal_user ? group.usergroup.personal_user.real_name + " (" + group.usergroup.name + ")" : group.usergroup.name;
    }

    shouldShowBeginTime(group: IRight) {
        // having -1 here (instead of 0) avoids "begins in a few seconds" right after adding a right
        return moment().diff(group.accessible_from, "seconds") < -1;
    }

    shouldShowEndTime(group: IRight) {
        return group.accessible_to != null && moment().diff(group.accessible_to) <= 0;
    }

    shouldShowEndedTime(group: IRight) {
        return group.accessible_to != null && moment().diff(group.accessible_to) > 0;
    }

    shouldShowDuration(group: IRight) {
        return group.duration != null && group.accessible_from == null;
    }

    shouldShowUnlockable(group: IRight) {
        return group.duration != null &&
            group.duration_from != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_from) < 0;
    }

    shouldShowNotUnlockable(group: IRight) {
        return group.duration != null &&
            group.duration_to != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_to) <= 0;
    }

    shouldShowNotUnlockableAnymore(group: IRight) {
        return group.duration != null &&
            group.duration_to != null &&
            group.accessible_from == null &&
            moment().diff(group.duration_to) > 0;
    }

    isObsolete(group: IRight) {
        return this.shouldShowEndedTime(group) || this.shouldShowNotUnlockableAnymore(group);
    }

    obsoleteFilterFn = (group: IRight) => {
        return !this.showActiveOnly || !this.isObsolete(group);
    }

    showClock(group: IRight) {
        return group.duration != null || group.accessible_to != null;
    }

    expireRight(group: IRight) {
        if (!this.accessType) {
            void showMessageDialog("Access type not selected.");
            return;
        }
        this.editRight(group);
        this.timeOpt.to = moment();
        this.timeOpt.type = "range";
        this.addOrEditPermission(group.usergroup.name, this.accessType);
    }

    findAccessTypeById(id: number) {
        if (!this.accessTypes) {
            return;
        }
        return this.accessTypes.find((a) => a.id === id);
    }

    editRight(group: IRight) {
        this.groupName = group.usergroup.name;
        this.accessType = this.findAccessTypeById(group.type);
        this.addingRight = false;
        this.selectedRight = group;

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
        } else {
            this.timeOpt.type = "range";
        }
    }

    private async getItems() {
        const r = await $http.get<IItemWithRights[]>("/getItems", {
            params: {
                folder_id: this.itemId,
                recursive: true,
                include_rights: true,
            },
        });
        return r.data;
    }

    private formatRights(i: IItemWithRights) {
        if (!i.rights.manage) {
            return "(you don't have manage right)";
        }
        let str = "";
        let currentRight;
        let typeSep = "";
        for (const r of i.grouprights) {
            let groupSep = ", ";
            // Assuming the rights list is ordered by access type.
            if (r.type != currentRight) {
                str += `${typeSep}${this.accessTypeById(r.type)!.name[0]}: `;
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
}

timApp.component("rightsEditor", {
    bindings: {
        accessTypes: "<?",
        itemId: "<?",
        massMode: "<?",
        urlRoot: "@?",
    },
    controller: RightsEditorController,
    templateUrl: "/static/templates/rightsEditor.html",
});
