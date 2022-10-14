import {
    Component,
    EventEmitter,
    Input,
    NgModule,
    OnChanges,
    OnInit,
    Output,
    SimpleChanges,
} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {AccessRoleService} from "tim/item/access-role.service";
import {to2, toPromise} from "tim/util/utils";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {Users} from "tim/user/userService";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";

const ANONYMOUS_GROUPNAME = "Anonymous users";

interface GroupInfo {
    id: number;
    name: string;
    managed: boolean;
    selected?: boolean;
}

@Component({
    selector: "tim-group-select-list",
    template: `
        <div class="item-group">
            <div class="checkbox" *ngIf="hasMultipleGroups">
                <label>
                    <input type="checkbox" [indeterminate]="someSelected" [ngModel]="allSelected" (ngModelChange)="toggleAllSelected($event)">
                    {{name}}
                </label>
            </div>
            <tim-alert *ngIf="searchError" i18n>
                Could not find the group. Make sure the group exists and you have at least "edit" permissions to it.
                Details: {{searchError}}
            </tim-alert>
            <div class="with-searchbar" [class.input-group]="manualAdd" *ngIf="hasSearchBar">
                <input class="form-control" type="text" [placeholder]="searchPlaceholder" [ngModel]="searchFilter" (ngModelChange)="onSearchInput($event)">
                <span class="input-group-btn" *ngIf="manualAdd">
                    <button class="btn btn-default" [disabled]="searchFilter.trim().length == 0" (click)="tryAddGroup()" i18n>Add</button>
                </span>
            </div>
            <div [class.item-list]="hasMultipleGroups" [class.with-searchbar]="hasSearchBar">
                <div class="checkbox" *ngFor="let group of filteredGroups">
                    <label>
                        <input type="checkbox" [ngModel]="group.selected" (ngModelChange)="onSelectGroupChange(group, $event)"> {{group.name}}
                    </label>
                </div>
                <div *ngIf="selectableGroups.length == 0">
                    <p class="small" i18n>Add groups by using the search box above and pressing "Add".</p>
                </div>
            </div>
        </div>
    `,
    styleUrls: ["./active-group-lock-dialog.component.scss"],
})
export class GroupSelectListComponent implements OnInit, OnChanges {
    @Input() name: string = "";
    @Input() selectableGroups: GroupInfo[] = [];
    @Input() selectedGroups: number[] = [];
    @Input() manualAdd = false;
    @Output() selectedGroupsChange = new EventEmitter<number[]>();
    searchPlaceholder = $localize`Filter groups`;
    someSelected = false;
    allSelected = false;
    filteredGroups: GroupInfo[] = [];
    searchFilter: string = "";
    searchError?: string;

    constructor(private http: HttpClient) {}

    get hasSearchBar() {
        return this.manualAdd || this.selectableGroups.length > 10;
    }

    get hasMultipleGroups() {
        return this.hasSearchBar || this.selectableGroups.length > 1;
    }

    ngOnInit() {
        if (this.manualAdd) {
            this.searchPlaceholder = $localize`Search groups`;
        }

        this.updateSelectedState();
        this.filteredGroups = this.selectableGroups.slice();
    }

    private updateSelectedState() {
        this.allSelected =
            this.selectableGroups.length > 0 &&
            this.selectableGroups.every((g) => g.selected);
        this.someSelected =
            this.selectableGroups.some((g) => g.selected) && !this.allSelected;
    }

    toggleAllSelected(newState: boolean) {
        this.allSelected = newState;
        this.someSelected = false;
        const selected = new Set(this.selectedGroups);
        if (this.allSelected) {
            for (const group of this.selectableGroups) {
                group.selected = true;
                selected.add(group.id);
            }
        } else {
            for (const group of this.selectableGroups) {
                group.selected = false;
                selected.delete(group.id);
            }
        }
        this.selectedGroups = Array.from(selected);
        this.selectedGroupsChange.emit(this.selectedGroups);
    }

    onSelectGroupChange(group: GroupInfo, state: boolean) {
        group.selected = state;
        this.updateSelectedState();
        if (state) {
            const selected = new Set(this.selectedGroups);
            selected.add(group.id);
            this.selectedGroups = Array.from(selected);
        } else {
            this.selectedGroups = this.selectedGroups.filter(
                (id) => id !== group.id
            );
        }
        this.selectedGroupsChange.emit(this.selectedGroups);
    }

    onSearchInput(search: string) {
        this.searchFilter = search;
        if (this.manualAdd) {
            return;
        }
        this.filteredGroups = this.selectableGroups.filter((g) =>
            g.name.toLowerCase().includes(search.toLowerCase())
        );
    }

    async tryAddGroup() {
        this.searchError = undefined;
        const r = await toPromise(
            this.http.get<GroupInfo>(
                `/access/groups/editable/info/${this.searchFilter.trim()}`
            )
        );
        if (r.ok) {
            this.searchFilter = "";
            const groupInfo = r.result;
            groupInfo.selected = true;
            this.selectableGroups.push(groupInfo);
            this.filteredGroups = this.selectableGroups.slice();
            this.updateSelectedState();
            const selected = new Set(this.selectedGroups);
            selected.add(groupInfo.id);
            this.selectedGroups = Array.from(selected);
            this.selectedGroupsChange.emit(this.selectedGroups);
        } else {
            this.searchError = r.result.error.error;
        }
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.selectableGroups) {
            this.filteredGroups = this.selectableGroups.slice();
            this.updateSelectedState();
        }
        if (changes.selectedGroups) {
            const selected = new Set(this.selectedGroups);
            for (const group of this.selectableGroups) {
                group.selected = selected.has(group.id);
            }
            this.updateSelectedState();
        }
    }
}

@Component({
    selector: "tim-active-group-lock-dialog",
    template: `
        <tim-dialog-frame [minimizable]="false" size="lg">
            <ng-container header>
                <ng-container i18n>Switch active groups</ng-container>
            </ng-container>
            <ng-container body>
                <p i18n>
                    You can change active groups by selecting groups from the lists below.
                    Changing active groups allows you to preview the documents as a member of other groups.
                </p>
                <div *ngIf="loading" class="flex justify-center"><tim-loading></tim-loading></div>
                <tim-group-select-list [class.loading]="loading"
                        name="Special groups" i18n-name
                        [selectableGroups]="specialGroups"
                        [(selectedGroups)]="activeGroups"></tim-group-select-list>
                <tim-group-select-list [class.loading]="loading"
                        name="Managed groups you are a member of" i18n-name
                        [selectableGroups]="groupsWithMembershipsManaged"
                        [(selectedGroups)]="activeGroups"></tim-group-select-list>
                <tim-group-select-list [class.loading]="loading"
                        name="Unmanaged groups you are a member of" i18n-name
                        [selectableGroups]="groupsWithMembershipsUnmanaged"
                        [(selectedGroups)]="activeGroups"></tim-group-select-list>
                <tim-group-select-list [class.loading]="loading"
                        name="Other groups (manual search)" i18n-name
                        [selectableGroups]="groupsWithAccess"
                        [manualAdd]="true"
                        [(selectedGroups)]="activeGroups"></tim-group-select-list>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="reset()" i18n>Reset</button>
                <button class="timButton" (click)="apply()" i18n>Apply</button>
                <button class="timButton" (click)="dismiss()" i18n>Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["./active-group-lock-dialog.component.scss"],
})
export class ActiveGroupLockDialogComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    protected dialogName = "activeGroupLockDialog";
    loading = false;
    activeGroups: number[] = [];
    groupsWithMembershipsManaged: GroupInfo[] = [];
    groupsWithMembershipsUnmanaged: GroupInfo[] = [];
    groupsWithAccess: GroupInfo[] = [];
    specialGroups: GroupInfo[] = [];
    private defaultActiveGroups = new Set<number>();

    constructor(
        private accessRole: AccessRoleService,
        private http: HttpClient
    ) {
        super();
    }

    async ngOnInit() {
        this.loading = true;
        await Promise.all([
            this.initSpecialGroups(),
            this.initPersonalGroups(),
        ]);
        this.defaultActiveGroups = new Set([
            ...this.groupsWithMembershipsManaged.map((g) => g.id),
            ...this.groupsWithMembershipsUnmanaged.map((g) => g.id),
            ...this.specialGroups.map((g) => g.id),
        ]);
        await this.initSelectedAccessibleGroups();

        this.activeGroups =
            Users.getCurrent().locked_active_groups ??
            Array.from(this.defaultActiveGroups);
        this.loading = false;
    }

    private async initPersonalGroups() {
        const r = await toPromise(
            this.http.get<GroupInfo[]>("/groups/usergroups")
        );
        if (r.ok) {
            this.groupsWithMembershipsManaged = r.result.filter(
                (g) => g.managed
            );
            this.groupsWithMembershipsUnmanaged = r.result.filter(
                (g) => !g.managed
            );
        }
    }

    private async initSpecialGroups() {
        const r = await toPromise(
            this.http.get<GroupInfo[]>("/groups/special")
        );
        if (r.ok) {
            // Remove anonymous group since it's always active
            this.specialGroups = r.result.filter(
                (g) => g.name != ANONYMOUS_GROUPNAME
            );
        }
    }

    private async initSelectedAccessibleGroups() {
        const activeGroups = Users.getCurrent().locked_active_groups;
        if (activeGroups) {
            const nonDefaultGroups = activeGroups.filter(
                (g) => !this.defaultActiveGroups.has(g)
            );
            const er = await toPromise(
                this.http.get<GroupInfo[]>("/access/groups/editable/find", {
                    params: {
                        group_ids: nonDefaultGroups.join(","),
                    },
                })
            );
            if (er.ok) {
                this.groupsWithAccess = er.result;
            }
        }
    }

    private static areSetsSame(a: Set<number>, b: Set<number>) {
        if (a.size !== b.size) {
            return false;
        }
        for (const id of a) {
            if (!b.has(id)) {
                return false;
            }
        }
        return true;
    }

    async apply() {
        const activeGroups = new Set(this.activeGroups);
        if (
            ActiveGroupLockDialogComponent.areSetsSame(
                activeGroups,
                this.defaultActiveGroups
            )
        ) {
            await this.reset();
            return;
        }

        const r = await to2(this.accessRole.lockGroups(this.activeGroups));
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not change active groups: ${r.result.error.error}`
            );
        }
    }

    async reset() {
        const r = await to2(this.accessRole.lockGroups(null));
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not reset active groups: ${r.result.error.error}`
            );
        }
    }
}

@NgModule({
    declarations: [ActiveGroupLockDialogComponent, GroupSelectListComponent],
    imports: [
        CommonModule,
        DialogModule,
        FormsModule,
        HttpClientModule,
        TimUtilityModule,
    ],
})
export class ActiveGroupLockDialogModule {}
