import {Component, Input, OnInit} from "@angular/core";
import {documentglobals, settingsglobals} from "tim/util/globals";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";


// NOTE: It's intentional that the two ICU expressions ({ item.isFolder, ... }) are slightly different in whitespace.
// We want two distinct entries in the translation file because the translations might be different.
@Component({
    selector: "tim-access-countdown",
    template: `
        <ng-container *ngIf="countDown >= 0" i18n>
            You will be able to access this { item.isFolder, select, true {folder} false {document} } in {{ humanizedCountdown }}.
        </ng-container>
        <ng-container *ngIf="countDown < 0" i18n>
            You can access the { item.isFolder, select, true { folder } false { document } } now. <tim-goto-link href="" [maxWait]="waitOffset">Refresh the page</tim-goto-link>.
        </ng-container>
    `,
})
export class AccessCountdownComponent implements OnInit {
    item = documentglobals().curr_item;
    @Input() waitTime!: number;
    countDown!: number;
    private timer!: number;
    waitOffset = Math.random() * 10;
    curLanguage = Users.getCurrentLanguage();

    get humanizedCountdown() {
        return humanizeDuration(this.countDown * 1000, {language: this.curLanguage});
    }

    ngOnInit() {
        this.countDown = Math.ceil(this.waitTime + this.waitOffset);
        this.timer = window.setInterval(() => {
            this.countDown -= 1;
            if (this.countDown < 0) {
                window.clearInterval(this.timer);
            }
        }, 1000);
    }
}
