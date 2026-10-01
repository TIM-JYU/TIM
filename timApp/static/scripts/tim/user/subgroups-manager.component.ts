import type {OnInit} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import type {IGroup} from "tim/user/IUser";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {splitItems, toPromise} from "tim/util/utils";

/**
 * A subgroup as returned by the /groups/subgroups routes.
 */
export interface ISubgroup extends IGroup {
    admin_doc_path?: string;
}

/**
 * Response of /groups/subgroups/add and /groups/subgroups/remove: the parent group and
 * its subgroups after the change.
 */
interface ISubgroupChangeResponse {
    parent: IGroup;
    subgroups: ISubgroup[];
}

/**
 * A subgroup name that could not be added or removed, with the reason given by the server.
 */
interface IFailedSubgroup {
    name: string;
    error: string;
}

/**
 * Manages the subgroups of a single user group.
 *
 * Lists the current subgroups, adds new ones by name (several at a time) and detaches
 * selected ones. Detaching only breaks the parent-subgroup link; neither group is
 * deleted, and members that the subgroup brought into the parent stay in the parent.
 *
 * Requires teacher access to the parent group to see its subgroups and edit access to
 * both groups to change the link; the server enforces this.
 */
@Component({
    selector: "tim-subgroups-manager",
    template: `
        <bootstrap-panel title="Subgroups" i18n-title>
            <tim-alert *ngIf="error" severity="danger">{{ error }}</tim-alert>

            <ng-container *ngIf="!group">
                <tim-alert severity="warning" i18n>
                    No group given. Set the group attribute to the name of the parent group.
                </tim-alert>
            </ng-container>

            <ng-container *ngIf="group">
                <p i18n>Subgroups of <strong>{{ group }}</strong>.</p>

                <table class="table table-condensed subgroup-table" *ngIf="subgroups.length > 0">
                    <thead>
                    <tr>
                        <th class="select-column">
                            <input type="checkbox"
                                   [checked]="allSelected()"
                                   [disabled]="loading"
                                   (change)="toggleAll($event)"
                                   title="Select all"
                                   i18n-title/>
                        </th>
                        <th i18n>Group name</th>
                        <th i18n>Group document</th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr *ngFor="let sg of subgroups">
                        <td class="select-column">
                            <input type="checkbox"
                                   [checked]="isSelected(sg)"
                                   [disabled]="loading"
                                   (change)="toggleSelected(sg)"/>
                        </td>
                        <td>{{ sg.name }}</td>
                        <td>
                            <a *ngIf="sg.admin_doc_path" href="/view/{{ sg.admin_doc_path }}">{{ sg.admin_doc_path }}</a>
                            <span *ngIf="!sg.admin_doc_path">-</span>
                        </td>
                    </tr>
                    </tbody>
                </table>

                <p *ngIf="subgroups.length === 0 && !loading" i18n>This group has no subgroups.</p>

                <div class="button-controls">
                    <button class="btn btn-danger"
                            [disabled]="loading || selected.size === 0"
                            (click)="removeSelected()"
                            i18n>
                        Remove selected
                    </button>
                    <span class="help-block" i18n>
                        Removing detaches the subgroup from this group. The group itself is not deleted, and members
                        that joined this group through the subgroup remain its members.
                    </span>
                </div>

                <div class="form-group add-subgroups">
                    <label for="subgroups-to-add" i18n>Add subgroups</label>
                    <textarea id="subgroups-to-add"
                              name="subgroups-to-add"
                              class="form-control"
                              rows="5"
                              [(ngModel)]="namesToAdd"
                              [disabled]="loading"
                              i18n-placeholder
                              placeholder="Enter the names of the groups to add as subgroups, one per line."></textarea>
                </div>

                <div class="button-controls">
                    <button class="timButton"
                            [disabled]="loading || namesToAdd.trim().length === 0"
                            (click)="addSubgroups()"
                            i18n>
                        Add
                    </button>
                    <button class="btn btn-default"
                            [disabled]="loading"
                            (click)="refresh()"
                            i18n>
                        Refresh
                    </button>
                    <tim-loading *ngIf="loading"></tim-loading>
                </div>

                <tim-alert *ngIf="added.length > 0" severity="success">
                    <ng-container i18n>Added as subgroups:</ng-container>
                    <ul>
                        <li *ngFor="let n of added">{{ n }}</li>
                    </ul>
                </tim-alert>

                <tim-alert *ngIf="removed.length > 0" severity="success">
                    <ng-container i18n>Removed from subgroups:</ng-container>
                    <ul>
                        <li *ngFor="let n of removed">{{ n }}</li>
                    </ul>
                </tim-alert>

                <tim-alert *ngIf="failed.length > 0" severity="danger">
                    <ng-container i18n>Failed:</ng-container>
                    <ul>
                        <li *ngFor="let f of failed">{{ f.name }}: {{ f.error }}</li>
                    </ul>
                </tim-alert>
            </ng-container>
        </bootstrap-panel>
    `,
    styleUrls: ["./subgroups-manager.component.scss"],
})
export class SubgroupsManagerComponent implements OnInit {
    /**
     * Name of the group whose subgroups are managed.
     */
    @Input() group?: string;

    subgroups: ISubgroup[] = [];

    /**
     * Names of the subgroups ticked in the table. Names rather than the objects
     * themselves, so that a selection survives a refresh of the list.
     */
    selected = new Set<string>();

    namesToAdd = "";
    loading = false;
    error?: string;
    added: string[] = [];
    removed: string[] = [];
    failed: IFailedSubgroup[] = [];

    constructor(private http: HttpClient) {}

    ngOnInit() {
        void this.refresh();
    }

    isSelected(sg: ISubgroup) {
        return this.selected.has(sg.name);
    }

    allSelected() {
        return (
            this.subgroups.length > 0 &&
            this.subgroups.every((sg) => this.selected.has(sg.name))
        );
    }

    toggleSelected(sg: ISubgroup) {
        if (this.selected.has(sg.name)) {
            this.selected.delete(sg.name);
        } else {
            this.selected.add(sg.name);
        }
    }

    toggleAll(event: Event) {
        if ((event.target as HTMLInputElement).checked) {
            this.selected = new Set(this.subgroups.map((sg) => sg.name));
        } else {
            this.selected.clear();
        }
    }

    /**
     * Reloads the subgroup list, dropping selections of groups that are no longer subgroups.
     */
    async refresh() {
        const group = this.group;
        if (!group) {
            return;
        }
        this.loading = true;
        this.error = undefined;
        const r = await toPromise(
            this.http.get<ISubgroup[]>(
                `/groups/subgroups/${encodeURIComponent(group)}`
            )
        );
        this.loading = false;
        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        this.setSubgroups(r.result);
    }

    /**
     * Adds every group named in the text area as a subgroup.
     */
    async addSubgroups() {
        const result = await this.applyToNames(
            this.uniqueNames(splitItems(this.namesToAdd)),
            "add"
        );
        if (!result) {
            return;
        }
        this.added = result.succeeded;
        // Keep the names that could not be added so they can be corrected and retried.
        this.namesToAdd = result.failed.map((f) => f.name).join("\n");
    }

    /**
     * Detaches the selected subgroups from the parent group.
     */
    async removeSelected() {
        const group = this.group;
        const names = this.subgroups
            .map((sg) => sg.name)
            .filter((n) => this.selected.has(n));
        if (!group || names.length === 0) {
            return;
        }
        const confirmed = await showConfirm(
            $localize`Remove subgroups`,
            $localize`Remove ${names.length} subgroup(s) from ${group}? The groups themselves are not deleted.`
        );
        if (!confirmed) {
            return;
        }
        const result = await this.applyToNames(names, "remove");
        if (result) {
            this.removed = result.succeeded;
        }
    }

    /**
     * Posts the add or remove route once per name and collects the results.
     *
     * The server takes one subgroup per request and commits each on its own, so a bad
     * name in the list does not hold up the rest and the outcome is reported per name.
     *
     * @param names Subgroup names to send.
     * @param action Route to use.
     * @returns The names that succeeded and the ones that did not, or undefined if
     *          there was nothing to send.
     */
    private async applyToNames(
        names: string[],
        action: "add" | "remove"
    ): Promise<{succeeded: string[]; failed: IFailedSubgroup[]} | undefined> {
        const group = this.group;
        if (!group || names.length === 0) {
            return undefined;
        }
        this.clearMessages();
        this.loading = true;
        const succeeded: string[] = [];
        const failed: IFailedSubgroup[] = [];
        for (const name of names) {
            const r = await toPromise(
                this.http.post<ISubgroupChangeResponse>(
                    `/groups/subgroups/${action}/${encodeURIComponent(
                        group
                    )}/${encodeURIComponent(name)}`,
                    {}
                )
            );
            if (r.ok) {
                succeeded.push(name);
            } else {
                failed.push({name: name, error: r.result.error.error});
            }
        }
        this.failed = failed;
        // The responses do carry the new subgroup list, but without the admin document
        // of each subgroup, so re-read the list instead of using them.
        if (succeeded.length > 0) {
            await this.refresh();
        } else {
            this.loading = false;
        }
        return {succeeded: succeeded, failed: failed};
    }

    private setSubgroups(subgroups: ISubgroup[]) {
        this.subgroups = subgroups;
        const names = new Set(subgroups.map((sg) => sg.name));
        for (const name of [...this.selected]) {
            if (!names.has(name)) {
                this.selected.delete(name);
            }
        }
    }

    /**
     * Drops empty entries and duplicates while keeping the order the user typed.
     */
    private uniqueNames(names: string[]) {
        return [...new Set(names.filter((n) => n.length > 0))];
    }

    private clearMessages() {
        this.error = undefined;
        this.added = [];
        this.removed = [];
        this.failed = [];
    }
}

@NgModule({
    declarations: [SubgroupsManagerComponent],
    exports: [SubgroupsManagerComponent],
    imports: [CommonModule, FormsModule, TimUtilityModule],
})
export class SubgroupsManagerModule {}
