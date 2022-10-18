import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {Users} from "tim/user/userService";
import type {AccessType} from "tim/item/access-role.service";
import {
    AccessRoleService,
    accessTypeDisplayNames,
} from "tim/item/access-role.service";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {TimStorage} from "tim/util/utils";
import * as t from "io-ts";

@Component({
    selector: "tim-role-info",
    template: `
        <tim-alert severity="info"
                    *ngIf="activeGroups !== undefined && activeGroups != null && !hideActiveGroupLockAlert"
                   (closing)="hideActiveGroupLockAlertMessage()"
                   [closeable]="true">
            <p i18n>You are previewing the page with limited group memberships. This affects your active permissions.</p>
            <p>
                <strong><a (click)="disableActiveGroupsLock()" i18n>Reset group lock</a></strong>
            </p>
        </tim-alert>
        <tim-alert severity="info"
                   *ngIf="lockedAccess && !hideAccessLockAlert"
                   (closing)="hideAccessLockAlertMessage()"
                   [closeable]="true">
            <p i18n>
                You are previewing the page with access limited to level "<strong>{{lockedAccessName}}</strong>".
            </p>
            <p>
                <strong><a (click)="disableAccessLock()" i18n>Turn off access limit</a></strong>
            </p>
        </tim-alert>
    `,
    styleUrls: ["./role-info.component.scss"],
})
export class RoleInfoComponent implements OnInit {
    activeGroups?: number[];
    lockedAccess?: AccessType;
    lockedAccessName?: string;
    private hideAccessLockAlertStorage = new TimStorage(
        "hideAccessLockAlert",
        t.boolean
    );
    private hideActiveGroupLockAlertStorage = new TimStorage(
        "hideActiveGroupLockAlert",
        t.boolean
    );

    constructor(private access: AccessRoleService) {}

    get hideAccessLockAlert() {
        return this.hideAccessLockAlertStorage.get() ?? false;
    }

    get hideActiveGroupLockAlert() {
        return this.hideActiveGroupLockAlertStorage.get() ?? false;
    }

    hideActiveGroupLockAlertMessage() {
        this.hideActiveGroupLockAlertStorage.set(true);
    }

    hideAccessLockAlertMessage() {
        this.hideAccessLockAlertStorage.set(true);
    }

    ngOnInit(): void {
        if (!Users.isLoggedIn()) {
            this.hideAccessLockAlertStorage.set(false);
            this.hideActiveGroupLockAlertStorage.set(false);
            return;
        }
        const currentUser = Users.getCurrent();
        this.lockedAccess = currentUser.locked_access;
        if (!this.lockedAccess) {
            this.hideAccessLockAlertStorage.set(false);
        }
        this.lockedAccessName = this.lockedAccess
            ? accessTypeDisplayNames[this.lockedAccess]
            : undefined;
        this.activeGroups = currentUser.locked_active_groups;
        if (this.activeGroups === undefined || this.activeGroups === null) {
            this.hideActiveGroupLockAlertStorage.set(false);
        }
    }

    async disableAccessLock() {
        const r = await this.access.lockAccess(null);
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not reset lock: ${r.result.error.error}`
            );
        }
    }

    async disableActiveGroupsLock() {
        const r = await this.access.lockGroups(null);
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not reset lock: ${r.result.error.error}`
            );
        }
    }
}
