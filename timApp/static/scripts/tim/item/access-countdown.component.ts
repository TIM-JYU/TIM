import {Component, Input, OnInit} from "@angular/core";
import {documentglobals, settingsglobals} from "tim/util/globals";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";


// NOTE: It's intentional that the two ICU expressions ({ item.isFolder, ... }) are slightly different in whitespace.
// We want two distinct entries in the translation file because the translations might be different.
@Component({
    selector: "tim-access-countdown",
    template: `
        <ng-container *ngIf="countDown; else goto" i18n>
            You will be able to access this { item.isFolder, select, true {folder} false {document} }
            in <tim-countdown [seconds]="waitTime"
                              [displayUnits]="['y', 'mo', 'w', 'd']"
                              (onFinish)="this.countDown = false"></tim-countdown>.
        </ng-container>
        <ng-template #goto i18n>
            You can access the { item.isFolder, select, true { folder } false { document } } now.
            <tim-goto-link href="" [maxWait]="waitOffset" [autoOpen]="true">Refresh the page</tim-goto-link>
        </ng-template>
    `,
})
export class AccessCountdownComponent {
    item = documentglobals().curr_item;
    @Input() waitTime!: number;
    waitOffset = Math.random() * 10;
    countDown = true;
}
