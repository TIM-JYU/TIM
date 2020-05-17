import {Component, Input} from "@angular/core";
import moment from "moment";
import {$http} from "tim/util/ngimport";
import {secondsToHHMMSS, to} from "tim/util/utils";
import {IRight} from "tim/item/rightsEditor";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";


interface IViewAccessStatus {
    can_access: boolean;
    right?: IRight;
}

enum GotoLinkState {
    Uninitialized,
    Ready,
    Countdown,
    Goto,
    Unauthorized,
    Expired
}

const VIEW_PATH = "/view/";
const OPEN_AT_WILDCARD = "*";

@Component({
    selector: "tim-goto-link",
    template: `
        <a [class.disabled]="linkDisabled"
           [attr.aria-disabled]="linkDisabled"
           [class.timButton]="isButton"
           [attr.role]="isButton ? 'button': null"
           (click)="handleClick()">
            <ng-content></ng-content>
        </a>
        <div class="load-text">
            <ng-container *ngIf="isExpired">
                <span class="error" i18n>Your access expired {{pastDueTime}} ago.</span>
            </ng-container>
            <ng-container *ngIf="isUnauthorized">
                <span class="error" i18n>You don't have permission to view that document.</span>
            </ng-container>
            <ng-container *ngIf="isCountdown" i18n>
                Opens in {{countdownText}}.
            </ng-container>
            <ng-container *ngIf="isGoing">
                <tim-loading></tim-loading>
                <span>
                    <ng-container *ngIf="waitText else defaultText">{{waitText}}</ng-container>
                    <ng-template #defaultText i18n>Loading, please wait.</ng-template>
                </span>
            </ng-container>
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
    pastDue = 0;
    linkDisabled = false;
    linkState = GotoLinkState.Uninitialized;
    openTime?: moment.Moment;
    closeTime?: moment.Moment;

    get isCountdown() {
        return this.linkState == GotoLinkState.Countdown;
    }

    get isGoing() {
        return this.linkState == GotoLinkState.Goto;
    }

    get isUnauthorized() {
        return this.linkState == GotoLinkState.Unauthorized;
    }

    get isExpired() {
        return this.linkState == GotoLinkState.Expired;
    }

    get countdownText() {
        return secondsToHHMMSS(this.countDown);
    }

    get pastDueTime() {
        return humanizeDuration(this.pastDue * 1000, {language: Users.getCurrentLanguage()});
    }

    async resolveOpenAtTime() {
        if (!this.openAt) { return; }

        if (this.openAt == OPEN_AT_WILDCARD) {
            const url = new URL(this.href, window.location.href);
            const path = url.pathname;
            // If the link goes to TIM, and it's a view, try to ask the opening time from the server
            // TODO: Separate open and close time checks into their own attributes
            if (url.hostname == window.location.hostname && path.startsWith(VIEW_PATH)) {
                const docPath = path.substring(VIEW_PATH.length);
                const accessInfo = await to($http.get<IViewAccessStatus>(`/doc_view_info/${docPath}`));
                if (accessInfo.ok && !accessInfo.result.data.can_access) {
                    if (accessInfo.result.data.right) {
                        this.openTime = accessInfo.result.data.right?.accessible_from;
                        this.closeTime = accessInfo.result.data.right?.accessible_to;
                        this.linkState = GotoLinkState.Ready;
                    } else {
                        this.linkState = GotoLinkState.Unauthorized;
                    }
                }
            }
        } else {
            this.openTime = moment.utc(this.openAt);
            this.linkState = GotoLinkState.Ready;
        }
    }

    async handleClick() {
        // Allow user to click during countdown or past expiration, but do nothing reasonable.
        if (this.isCountdown) { return; }

        this.linkDisabled = true;

        if (this.linkState == GotoLinkState.Uninitialized) {
            await this.resolveOpenAtTime();
        }

        if (this.isUnauthorized) { return; }

        let curTime = moment();
        if (this.closeTime || this.openTime) {
            const serverTime = await to($http.get<{time: number}>("/time"));
            // Fail silently here and hope the user clicks again so it can retry
            if (!serverTime.ok) {
                this.linkDisabled = false;
                return;
            }
            curTime = moment.utc(serverTime.result.data.time);
        }

        if (this.closeTime && this.closeTime.isValid() && this.closeTime.isBefore(curTime)) {
            this.pastDue = this.closeTime.diff(curTime, "seconds");
            this.linkState = GotoLinkState.Expired;
            return;
        }

        if (this.openTime && this.openTime.isValid()) {
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
