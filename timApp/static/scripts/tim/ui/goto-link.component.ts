import {Component, Input, OnInit} from "@angular/core";
import moment from "moment";
import {$http} from "tim/util/ngimport";
import {time2String, to} from "tim/util/utils";

enum GotoLinkState {
    None,
    Countdown,
    Goto
}

@Component({
    selector: "tim-goto-link",
    template: `
        <a [class.disabled]="linkDisabled" [class.timButton]="isButton" (click)="handleClick()" [attr.aria-disabled]="linkDisabled">
            <ng-content></ng-content>
        </a>
        <div class="load-text" *ngIf="isCountdown">
            {{countdownText}}
        </div>
        <div class="load-text" *ngIf="isGoing">
            <tim-loading></tim-loading>
            <span>
                <ng-container *ngIf="waitText else defaultText">{{waitText}}</ng-container>
                <ng-template #defaultText i18n>Loading, please wait.</ng-template>
            </span>
        </div>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent implements OnInit {
    @Input() href = "";
    @Input() waitText?: string;
    @Input() resetTime = 15;
    @Input() maxWait = 0;
    @Input() isButton = false;
    @Input() target = "_self";
    @Input() openAt?: string;
    countDown = 0;
    linkDisabled = false;
    linkState = GotoLinkState.None;
    openTime?: moment.Moment;

    ngOnInit() {
        if (this.openAt) {
            this.openTime = moment.utc(this.openAt);
        }
    }

    get isCountdown() {
        return this.linkState == GotoLinkState.Countdown;
    }

    get isGoing() {
        return this.linkState == GotoLinkState.Goto;
    }

    get countdownText() {
        return time2String(this.countDown, true);
    }

    async handleClick() {
        // Allow user to click during countdown, but do nothing reasonable.
        if (this.isCountdown) { return; }

        this.linkDisabled = true;
        if (this.openTime && this.openTime.isValid()) {
            const serverTime = await to($http.get<{time: number}>("/time"));
            // Fail silently here and hope the user clicks again so it can retry
            if (!serverTime.ok) {
                this.linkDisabled = false;
                return;
            }
            const curTime = moment.utc(serverTime.result.data.time);
            this.countDown = this.openTime.diff(curTime, "seconds");
        }

        // No countdown needed, just start redirect normally
        if (this.countDown <= 0) {
            this.startGoto();
        } else {
            this.startCountdown();
        }
    }

    startCountdown() {
        // Allow clicking, but do nothing reasonable...
        this.linkDisabled = false;
        this.linkState = GotoLinkState.Countdown;

        const timer = setInterval(() => {
            this.countDown--;
            if (this.countDown <= 0) {
                clearInterval(timer);
                this.startGoto();
            }
        }, 1000);
    }

    startGoto() {
        if (this.isGoing) { return; }
        this.linkDisabled = true;
        this.linkState = GotoLinkState.Goto;
        const waitTime = Math.random() * Math.max(this.maxWait, 0) * 1000;
        const realResetTime = Math.max(this.resetTime * 1000, waitTime);

        setTimeout(() => {
            if (this.href == "") {
                window.location.reload();
            } else {
                window.open(this.href, this.target);
            }
        }, waitTime);

        setTimeout(() => {
           this.linkState = GotoLinkState.None;
           this.linkDisabled = false;
        }, realResetTime);
    }
}
