import {Component, Input, OnInit} from "@angular/core";
import moment from "moment";
import {$http} from "tim/util/ngimport";
import {secondsToHHMMSS, to} from "tim/util/utils";
import {IRight} from "tim/item/rightsEditor";

enum GotoLinkState {
    Uninitalized,
    Ready,
    Countdown,
    Goto
}

const VIEW_PATH = "/view/";
const OPEN_AT_WILDCARD = "*";

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
export class GotoLinkComponent {
    @Input() href = "";
    @Input() waitText?: string;
    @Input() resetTime = 15;
    @Input() maxWait = 0;
    @Input() isButton = false;
    @Input() target = "_self";
    @Input() openAt?: string;
    countDown = 0;
    linkDisabled = false;
    linkState = GotoLinkState.Uninitalized;
    openTime?: moment.Moment;

    get isCountdown() {
        return this.linkState == GotoLinkState.Countdown;
    }

    get isGoing() {
        return this.linkState == GotoLinkState.Goto;
    }

    get countdownText() {
        return secondsToHHMMSS(this.countDown);
    }

    async resolveOpenAtTime() {
        if (!this.openAt) { return; }

        if (this.openAt == OPEN_AT_WILDCARD) {
            const url = new URL(this.href, window.location.href);
            const path = url.pathname;
            // If the link goes to TIM, and it's a view, try to ask the opening time from the server
            if (url.hostname == window.location.hostname && path.startsWith(VIEW_PATH)) {
                const docPath = path.substring(VIEW_PATH.length);
                const accessInfo = await to($http.get<IRight | null>(`/doc_view_info/${docPath}`));
                if (accessInfo.ok) {
                    this.openTime = accessInfo.result.data?.accessible_from;
                }
            }
        } else {
            this.openTime = moment.utc(this.openAt);
        }
    }

    async handleClick() {
        // Allow user to click during countdown, but do nothing reasonable.
        if (this.isCountdown) { return; }

        this.linkDisabled = true;

        if (this.linkState == GotoLinkState.Uninitalized) {
            await this.resolveOpenAtTime();
            this.linkState = GotoLinkState.Ready;
        }

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
            // Special case: on empty href just reload the page to mimick the behaviour of <a>
            if (this.href == "") {
                window.location.reload();
            } else {
                window.open(this.href, this.target);
            }
        }, waitTime);

        setTimeout(() => {
           this.linkState = GotoLinkState.Ready;
           this.linkDisabled = false;
        }, realResetTime);
    }
}
