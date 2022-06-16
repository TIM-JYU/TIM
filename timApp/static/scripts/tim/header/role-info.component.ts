import {Component, OnInit} from "@angular/core";
import {Users} from "tim/user/userService";
import {
    AccessRoleService,
    AccessType,
    accessTypeDisplayNames,
} from "tim/item/access-role.service";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {TimStorage} from "tim/util/utils";
import * as t from "io-ts";

@Component({
    selector: "tim-role-info",
    template: `
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
    lockedAccess?: AccessType;
    lockedAccessName?: string;
    private hideAlert = new TimStorage("hideAccessLockAlert", t.boolean);

    constructor(private access: AccessRoleService) {}

    get hideAccessLockAlert() {
        return this.hideAlert.get() ?? false;
    }

    hideAccessLockAlertMessage() {
        this.hideAlert.set(true);
    }

    ngOnInit(): void {
        if (!Users.isLoggedIn()) {
            this.hideAlert.set(false);
            return;
        }
        const currentUser = Users.getCurrent();
        this.lockedAccess = currentUser.locked_access;
        if (!this.lockedAccess) {
            this.hideAlert.set(false);
        }
        this.lockedAccessName = this.lockedAccess
            ? accessTypeDisplayNames[this.lockedAccess]
            : undefined;
    }

    async disableAccessLock() {
        const r = await this.access.lockAccess(null);
        if (!r.ok) {
            await showMessageDialog(
                $localize`Could not reset lock: ${r.result.error.error}`
            );
        }
    }
}
