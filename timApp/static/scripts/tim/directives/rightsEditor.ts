import {IScope, IController} from "angular";
import moment from "moment";
import {timApp} from "tim/app";
import * as focusMe from "tim/directives/focusMe";
import {markAsUsed} from "tim/utils";
import {$http, $window} from "../ngimport";

markAsUsed(focusMe);

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
    private grouprights: {}[];
    private showActiveOnly: boolean;
    private durationTypes: moment.unitOfTime.Base[];
    private selectedRight: {};
    private datePickerOptionsFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationFrom: EonasdanBootstrapDatetimepicker.SetOptions;
    private datePickerOptionsDurationTo: EonasdanBootstrapDatetimepicker.SetOptions;
    private accessTypes: {}[];
    private accessType: {};
    private addingRight: boolean;
    private focusEditor: boolean;
    private urlRoot: string;
    private itemId: number;
    private listMode: boolean;
    private groupName: string;
    private scope: IScope;

    constructor(scope: IScope) {
        this.scope = scope;
        this.grouprights = [];
        this.timeOpt = {type: "always", durationType: "hours", durationAmount: 4};
        this.selectedRight = null;
        this.showActiveOnly = true;
        this.durationTypes = ["seconds", "minutes", "hours", "days", "weeks", "months", "years"];
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
        if (this.accessTypes) {
            this.accessType = this.accessTypes[0];
        }
    }

    $onInit() {
        this.getPermissions();

        // TODO make duration editor its own component
        this.scope.$watchGroup([() => this.timeOpt.durationAmount, () => this.timeOpt.durationType], (newValues, oldValues, scope) => {
            this.timeOpt.duration = moment.duration(this.timeOpt.durationAmount, this.timeOpt.durationType);
        });
    }

    showAddRightFn(type) {
        this.accessType = type;
        this.selectedRight = null;
        this.addingRight = true;
        this.focusEditor = true;
    }

    removeConfirm(group, type) {
        if ($window.confirm("Remove " + type + " right from " + group.name + "?")) {
            this.removePermission(group, type);
        }
    }

    getPermissions() {
        if (!this.urlRoot || !this.itemId) {
            return;
        }
        $http.get<{grouprights, accesstypes}>("/" + this.urlRoot + "/get/" + this.itemId).then((response) => {
            const data = response.data;
            this.grouprights = data.grouprights;
            if (data.accesstypes) {
                this.accessTypes = data.accesstypes;
                if (!this.accessType) {
                    this.accessType = this.accessTypes[0];
                }
            }
        }, (response) => {
            $window.alert("Could not fetch permissions.");
        });
    }

    removePermission(right, type) {
        $http.put("/" + this.urlRoot + "/remove/" + this.itemId + "/" + right.gid + "/" + type, {}).then((response) => {
            this.getPermissions();
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    cancel() {
        this.addingRight = false;
        this.selectedRight = null;
    }

    editingRight() {
        return this.selectedRight !== null;
    }

    addOrEditPermission(groupname, type) {
        $http.put("/" + this.urlRoot + "/add/" + this.itemId + "/" + groupname.split("\n").join(";") + "/" + type.name,
            this.timeOpt).then((response) => {
            this.getPermissions();
            this.cancel();
        }, (response) => {
            $window.alert(response.data.error);
        });
    }

    getPlaceholder() {
        return "enter username(s)/group name(s) separated by semicolons" + (this.listMode ? " or newlines" : "");
    }

    getGroupDesc(group) {
        return group.fullname ? group.fullname + " (" + group.name + ")" : group.name;
    }

    shouldShowBeginTime(group) {
        // having -1 here (instead of 0) avoids "begins in a few seconds" right after adding a right
        return moment().diff(group.accessible_from, "seconds") < -1;
    }

    shouldShowEndTime(group) {
        return group.accessible_to !== null && moment().diff(group.accessible_to) <= 0;
    }

    shouldShowEndedTime(group) {
        return group.accessible_to !== null && moment().diff(group.accessible_to) > 0;
    }

    shouldShowDuration(group) {
        return group.duration !== null && group.accessible_from === null;
    }

    shouldShowUnlockable(group) {
        return group.duration !== null &&
            group.duration_from !== null &&
            group.accessible_from === null &&
            moment().diff(group.duration_from) < 0;
    }

    shouldShowNotUnlockable(group) {
        return group.duration !== null &&
            group.duration_to !== null &&
            group.accessible_from === null &&
            moment().diff(group.duration_to) <= 0;
    }

    shouldShowNotUnlockableAnymore(group) {
        return group.duration !== null &&
            group.duration_to !== null &&
            group.accessible_from === null &&
            moment().diff(group.duration_to) > 0;
    }

    isObsolete(group) {
        return this.shouldShowEndedTime(group) || this.shouldShowNotUnlockableAnymore(group);
    }

    obsoleteFilterFn = (group) => {
        return !this.showActiveOnly || !this.isObsolete(group);
    }

    showClock(group) {
        return group.duration !== null || group.accessible_to !== null;
    }

    expireRight(group) {
        this.editRight(group);
        this.timeOpt.to = moment();
        this.timeOpt.type = "range";
        this.addOrEditPermission(group.name, this.accessType);
    }

    editRight(group) {
        this.groupName = group.name;
        this.accessType = {id: group.access_type, name: group.access_name};
        this.addingRight = false;
        this.selectedRight = group;

        if (group.duration_from) {
            this.timeOpt.durationFrom = moment(group.duration_from);
        } else {
            this.timeOpt.durationFrom = null;
        }
        if (group.duration_to) {
            this.timeOpt.durationTo = moment(group.duration_to);
        } else {
            this.timeOpt.durationTo = null;
        }

        if (group.accessible_from) {
            this.timeOpt.from = moment(group.accessible_from);
        } else {
            this.timeOpt.from = null;
        }
        if (group.accessible_to) {
            this.timeOpt.to = moment(group.accessible_to);
        } else {
            this.timeOpt.to = null;
        }

        if (group.duration && group.accessible_from === null) {
            const d = moment.duration(group.duration);
            this.timeOpt.type = "duration";
            for (let i = this.durationTypes.length - 1; i >= 0; --i) {
                const amount = d.as(this.durationTypes[i]);
                if (Math.floor(amount) === amount || i === 0) {
                    // preserve last duration type choice if the amount is zero
                    if (amount !== 0) {
                        this.timeOpt.durationType = this.durationTypes[i];
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
