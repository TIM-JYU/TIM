import {Component, Input} from "@angular/core";
import moment, {Moment} from "moment";
import {formatString, to2} from "tim/util/utils";
import {IRight} from "tim/item/rightsEditor";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";
import {HttpClient} from "@angular/common/http";

interface IViewAccessStatus {
    can_access: boolean;
    right?: IRight;
}

enum GotoLinkState {
    Ready,
    Countdown,
    Goto,
    Unauthorized,
    Expired
}

const VIEW_PATH = "/view/";

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
                <span class="error">
                    <ng-container *ngIf="pastDueText else defaultPastDueText">{{formatString(pastDueText, pastDueTime)}}</ng-container>
                    <ng-template #defaultPastDueText i18n>Your access expired {{pastDueTime}} ago.</ng-template>
                </span>
            </ng-container>
            <ng-container *ngIf="isUnauthorized">
                <span class="error">
                    <ng-container *ngIf="unauthorizedText else defaultUnauthorizedText">{{unauthorizedText}}</ng-container>
                    <ng-template #defaultUnauthorizedText i18n>You don't have permission to view that document.</ng-template>
                </span>
            </ng-container>
            <ng-container *ngIf="isCountdown">
                <tim-countdown [template]="countdownText" [seconds]="countDown" (onFinish)="startGoto()"></tim-countdown>
                <ng-template i18n="@@gotoOpensIn">Opens in {{"{"}}0{{"}"}}.</ng-template>
            </ng-container>
            <ng-container *ngIf="isGoing">
                <tim-loading></tim-loading>
                <span>
                    <ng-container *ngIf="waitText else defaultWaitText">{{waitText}}</ng-container>
                    <ng-template #defaultWaitText i18n>Loading, please wait.</ng-template>
                </span>
            </ng-container>
        </div>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent {
    @Input() href = "";
    @Input() waitText?: string;
    @Input() countdownText: string = $localize `:@@gotoOpensIn:Opens in ${"{"}:INTERPOLATION:0${"}"}:INTERPOLATION_1:.`;
    @Input() unauthorizedText?: string;
    @Input() pastDueText?: string;
    @Input() timeLang?: string;
    @Input() resetTime = 15;
    @Input() maxWait = 0;
    @Input() isButton = false;
    @Input() target = "_self";
    @Input() openAt?: string;
    @Input() closeAt?: string;
    countDown = 0;
    pastDue = 0;
    linkDisabled = false;
    linkState = GotoLinkState.Ready;

    formatString = formatString;

    constructor(private http: HttpClient) {
    }

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

    get pastDueTime() {
        return humanizeDuration(this.pastDue * 1000, {language: this.timeLang ?? Users.getCurrentLanguage()});
    }

    async resolveAccess() {
        const url = new URL(this.href, window.location.href);
        const path = url.pathname;

        // If href points to a valid TIM document, check permissions
        if (url.hostname == window.location.hostname && path.startsWith(VIEW_PATH)) {
            const docPath = path.substring(VIEW_PATH.length);
            const accessInfo = await to2(this.http.get<IViewAccessStatus>(`/docViewInfo/${docPath}`).toPromise());
            if (accessInfo.ok) {
                return {unauthorized: !accessInfo.result.can_access, access: accessInfo.result.right};
            }
        }
        return {unauthorized: false, access: undefined};
    }

    parseTime(timeString?: string, wildcardValue?: Moment) {
        if (!timeString) {
            return wildcardValue;
        }
        const result = moment.utc(timeString);
        return result.isValid() ? result : wildcardValue;
    }

    async handleClick() {
        // Allow user to click during countdown or past expiration, but do nothing reasonable.
        if (this.isCountdown) { return; }

        this.linkDisabled = true;

        const {unauthorized, access} = await this.resolveAccess();

        if (unauthorized && !access) {
            this.linkState = GotoLinkState.Unauthorized;
            this.startReset(this.resetTime);
            return;
        }

        const openTime = this.parseTime(this.openAt, access?.accessible_from);
        const closeTime = this.parseTime(this.closeAt, access?.accessible_to);

        let curTime = moment();
        if (closeTime || openTime) {
            const serverTime = await to2(this.http.get<{time: Moment}>("/time").toPromise());
            // Fail silently here and hope the user clicks again so it can retry
            if (!serverTime.ok) {
                this.linkDisabled = false;
                return;
            }
            curTime = serverTime.result.time.utc();
        }

        if (closeTime?.isValid() && closeTime.isBefore(curTime)) {
            this.pastDue = closeTime.diff(curTime, "seconds");
            this.linkState = GotoLinkState.Expired;
            this.startReset(this.resetTime);
            return;
        }

        if (openTime?.isValid()) {
            this.countDown = openTime.diff(curTime, "seconds");
        }

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
    }

    startReset(resetTime: number) {
        setTimeout(() => {
           this.linkState = GotoLinkState.Ready;
           this.linkDisabled = false;
        }, resetTime * 1000);
    }

    startGoto() {
        if (this.isGoing) { return; }
        this.linkDisabled = true;
        this.linkState = GotoLinkState.Goto;
        const waitTime = Math.random() * Math.max(this.maxWait, 0);
        const realResetTime = Math.max(this.resetTime, waitTime);

        setTimeout(() => {
            // Special case: on empty href just reload the page to mimic the behaviour of <a>
            if (this.href == "") {
                // Note: the force-reload is deprecated: https://github.com/Microsoft/TypeScript/issues/28898
                // TODO: Do we need force reloading? There is no consensus on whether this is supported by all browsers
                //  anymore.
                window.location.reload(true);
            } else {
                window.open(this.href, this.target);
            }
        }, waitTime * 1000);

        this.startReset(realResetTime);
    }
}
