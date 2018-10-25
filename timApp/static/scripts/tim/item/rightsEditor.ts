import {IController, IScope} from "angular";
import moment, {Duration, Moment} from "moment";
import {timApp} from "tim/app";
import * as focusMe from "tim/ui/focusMe";
import {Binding, markAsUsed, to} from "tim/util/utils";
import {showMessageDialog} from "../ui/dialog";
import {durationTypes} from "../ui/durationPicker";
import {$http} from "../util/ngimport";

markAsUsed(focusMe);

export interface IRight {
    access_name: string;
    access_type: number;
    duration_to: Moment | null;
    duration_from: Moment | null;
    duration: null | Duration;
    accessible_to: Moment;
    accessible_from: Moment;
    fullname?: string;
    name: string;
    gid: number;
}

export interface IAccessType {
    id: number;
    name: string;
}

class RightsEditorController implements IController {
    private static $inject = ["$scope"];
    private timeOpt: {
        type: string,
        durationType: moment.unitOfTime.Base,
        durationAmount: number,
        duration?: moment.Duration,
        to?: moment.Moment,
        from?: moment.Moment,
        durationTo?: moment.Moment,
        durationFrom?: moment.Moment,
    };
    private grouprights: IRight[];
    private showActiveOnly: boolean;
    private selectedRight: IRight | null;
    private datePickerOptionsFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private accessTypes: Binding<IAccessType[] | undefined, "<">;
    private accessType: IAccessType | undefined;
    private addingRight: boolean = false;
    private focusEditor: boolean = false;
    private urlRoot: Binding<string | undefined, "<">;
    private itemId: Binding<number | undefined, "<">;
    private listMode: boolean = false;
    private groupName: string | undefined;
    private scope: IScope;

    constructor(scope: IScope) {
        this.scope = scope;
        this.grouprights = [];
        this.timeOpt = {type: "always", durationType: "hours", durationAmount: 4};
        this.selectedRight = null;
        this.showActiveOnly = true;
        this.datePickerOptionsFrom = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment(),
            showTodayButton: true,
        };
        this.datePickerOptionsTo = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: moment(),
            showTodayButton: true,
        };
        this.datePickerOptionsDurationFrom = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };
        this.datePickerOptionsDurationTo = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };
    }

    $onInit() {
        if (this.accessTypes) {
            this.accessType = this.accessTypes[0];
        }
        this.getPermissions();

        // TODO make duration editor its own component
        this.scope.$watchGroup([() => this.timeOpt.durationAmount, () => this.timeOpt.durationType], (newValues, oldValues, scope) => {
            this.timeOpt.duration = moment.duration(this.timeOpt.durationAmount, this.timeOpt.durationType);
        });
    }

    showAddRightFn(type: IAccessType) {
        this.accessType = type;
        this.selectedRight = null;
        this.addingRight = true;
        this.focusEditor = true;
    }

    removeConfirm(group: IRight, type: string) {
        if (window.confirm("Remove " + type + " right from " + group.name + "?")) {
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
        const r = await to($http.put(`/${this.urlRoot}/remove/${this.itemId}/${right.gid}/${type}`, {}));
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

    async addOrEditPermission(groupname: string, type: IAccessType) {
        const r = await to($http.put(`/${this.urlRoot}/add/${this.itemId}/${groupname.split("\n").join(";")}/${type.name}`,
            this.timeOpt));
        if (r.ok) {
            this.getPermissions();
            this.cancel();
        } else {
            await showMessageDialog(r.result.data.error);
        }
    }

    getPlaceholder() {
        return "enter username(s)/group name(s) separated by semicolons" + (this.listMode ? " or newlines" : "");
    }

    getGroupDesc(group: IRight) {
        return group.fullname ? group.fullname + " (" + group.name + ")" : group.name;
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
        this.addOrEditPermission(group.name, this.accessType);
    }

    editRight(group: IRight) {
        this.groupName = group.name;
        this.accessType = {id: group.access_type, name: group.access_name};
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
                        this.timeOpt.durationType = durationTypes[i];
                    }
                    this.timeOpt.durationAmount = amount;
                    break;
                }
            }
        } else {
            this.timeOpt.type = "range";
        }
    }
}

timApp.component("rightsEditor", {
    bindings: {
        accessTypes: "<?",
        itemId: "<?",
        urlRoot: "@?",
    },
    controller: RightsEditorController,
    templateUrl: "/static/templates/rightsEditor.html",
});
