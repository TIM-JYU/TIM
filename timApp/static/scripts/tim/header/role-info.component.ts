import {Component, OnInit} from "@angular/core";
import {Users} from "tim/user/userService";
import {
    AccessRoleService,
    AccessType,
    accessTypeDisplayNames,
} from "tim/item/access-role.service";
import {showMessageDialog} from "tim/ui/showMessageDialog";

@Component({
    selector: "tim-role-info",
    template: `
    <tim-alert severity="info" *ngIf="lockedAccess">
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

    constructor(private access: AccessRoleService) {}

    ngOnInit(): void {
        if (!Users.isLoggedIn()) {
            return;
        }
        const currentUser = Users.getCurrent();
        this.lockedAccess = currentUser.locked_access;
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
