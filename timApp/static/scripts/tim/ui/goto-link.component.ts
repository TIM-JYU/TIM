import {Component, Input, OnInit} from "@angular/core";
import moment, {Moment} from "moment";
import {formatString, to2} from "tim/util/utils";
import {IRight} from "tim/item/rightsEditor";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";
import {HttpClient} from "@angular/common/http";
import {vctrlInstance} from "tim/document/viewctrlinstance";

interface IViewAccessStatus {
    can_access: boolean;
    right?: IRight;
}

interface GotoError {
    userMessage?: string;
    defaultMessage: string;
    displayClass?: string;
}

enum GotoLinkState {
    Ready,
    Countdown,
    Goto,
    Error,
}

const VIEW_PATH = "/view/";

@Component({
    selector: "tim-goto-link",
    template: `
        <a [class.disabled]="linkDisabled"
           [attr.aria-disabled]="linkDisabled"
           [class.timButton]="isButton"
           [attr.role]="isButton ? 'button': null"
           (click)="start()">
            <ng-content></ng-content>
        </a>
        <div class="load-text" *ngIf="hasStatus">
            <span *ngIf="this.error" [className]="this.error.displayClass ? this.error.displayClass : 'error'">
                {{this.error.userMessage ? this.error.userMessage : this.error.defaultMessage}}
            </span>
            <ng-container *ngIf="isCountdown">
                <tim-countdown [template]="countdownText" [endTime]="openTime" (onFinish)="countdownDone()"></tim-countdown>
            </ng-container>
            <ng-container *ngIf="isGoing">
                <tim-loading></tim-loading>
                <span>
                    <ng-container *ngIf="waitText else defaultWaitText">{{waitText}}</ng-container>
                    <ng-template #defaultWaitText i18n>Loading, please wait.</ng-template>
                </span>
            </ng-container>
        </div>

        <ng-template i18n="@@gotoErrorExpired">Your access expired {{0}} ago.</ng-template>
        <ng-template i18n="@@gotoErrorUnauthorized">You don't have permission to view that document.</ng-template>
        <ng-template i18n="@@gotoErrorUnsaved">You have unsaved changes. Save them or click the link again.</ng-template>
        <ng-template i18n="@@gotoCanAccess">You can access the link now.</ng-template>
        <ng-template i18n="@@gotoOpensIn">Opens in {{"{"}}0{{"}"}}.</ng-template>
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent implements OnInit {
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
    @Input() checkUnsaved: boolean = false;
    @Input() unsavedChangesText?: string;
    @Input() autoOpen: boolean = false;
    @Input() stopAfterCountdown: boolean = false;
    openTime?: string;
    linkDisabled = false;
    linkState = GotoLinkState.Ready;
    resetTimeout?: number;
    error?: GotoError;
    unsavedChangesChecked = false;

    constructor(private http: HttpClient) {
    }

    ngOnInit() {
        if (this.autoOpen) {
            void this.start();
        }
    }

    get hasStatus() {
        return this.linkState != GotoLinkState.Ready;
    }

    get isCountdown() {
        return this.linkState == GotoLinkState.Countdown;
    }

    get isGoing() {
        return this.linkState == GotoLinkState.Goto;
    }

    private getPastDueTime(pastDue: number) {
        return humanizeDuration(pastDue * 1000, {language: this.timeLang ?? Users.getCurrentLanguage()});
    }

    private showError(error: GotoError) {
        this.error = error;
        this.linkState = GotoLinkState.Error;
        this.startReset(this.resetTime);
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

    parseTime(timeString?: string, wildcardValue?: Moment | null) {
        if (!timeString) {
            return wildcardValue;
        }
        const result = moment(timeString);
        return result.isValid() ? result : wildcardValue;
    }

    async start() {
        // Allow user to click during countdown or past expiration, but do nothing reasonable.
        if (this.isCountdown) { return; }

        this.stopReset();
        this.error = undefined;
        this.linkDisabled = true;

        const {unauthorized, access} = await this.resolveAccess();
        if (unauthorized && !access) {
            this.showError({
                userMessage: this.unauthorizedText,
                defaultMessage: $localize`:@@gotoErrorUnauthorized:You don't have permission to view that document.`,
            });
            return;
        }

        const openTime = this.parseTime(this.openAt, access?.accessible_from ?? access?.duration_from);
        const closeTime = this.parseTime(this.closeAt, access?.accessible_to ?? access?.duration_to);

        let curTime = moment();
        if (closeTime || openTime) {
            const serverTime = await to2(this.http.get<{time: Moment}>("/time").toPromise());
            // Fail silently here and hope the user clicks again so it can retry
            if (!serverTime.ok) {
                this.linkDisabled = false;
                return;
            }
            curTime = serverTime.result.time;
        }

        if (closeTime?.isValid() && closeTime.isBefore(curTime)) {
            const pastDue = this.getPastDueTime(closeTime.diff(curTime, "seconds"));
            this.showError({
                userMessage: this.pastDueText ? formatString(this.pastDueText, pastDue) : undefined,
                defaultMessage: $localize`:@@gotoErrorExpired:Your access expired ${pastDue}:INTERPOLATION: ago.`,
            });
            return;
        }

        if (!this.unsavedChangesChecked && this.checkUnsaved && vctrlInstance?.checkUnSavedTimComponents()) {
            this.showError({
                userMessage: this.unsavedChangesText,
                defaultMessage: $localize`:@@gotoErrorUnsaved:You have unsaved changes. Save them or click the link again.`,
            });
            this.unsavedChangesChecked = true;
            this.linkDisabled = false;
            return;
        }

        if (openTime?.isValid()) {
            this.openTime = openTime?.toISOString();
        }

        if (openTime?.isValid() && openTime.diff(curTime, "seconds", true) <= 0) {
            this.startOpenLink();
        } else {
            this.startCountdown();
        }
    }

    startCountdown() {
        // Allow clicking, but do nothing reasonable...
        this.linkDisabled = false;
        this.linkState = GotoLinkState.Countdown;
    }

    countdownDone() {
        if (this.stopAfterCountdown) {
            this.showError({
                defaultMessage: $localize`:@@gotoCanAccess:You can access the link now.`,
                displayClass: "info",
            });
            this.linkDisabled = false;
            return;
        } else {
            this.startOpenLink();
        }
    }

    startReset(resetTime: number) {
         this.resetTimeout = window.setTimeout(() => this.reset(), resetTime * 1000);
    }

    private reset() {
        this.stopReset();
        this.linkState = GotoLinkState.Ready;
        this.linkDisabled = false;
        this.unsavedChangesChecked = false;
    }

    stopReset() {
        if (this.resetTimeout) {
            window.clearTimeout(this.resetTimeout);
            this.resetTimeout = undefined;
        }
    }

    startOpenLink() {
        if (this.isGoing) { return; }
        this.linkDisabled = true;
        this.linkState = GotoLinkState.Goto;
        const waitTime = Math.random() * Math.max(this.maxWait, 0);
        const realResetTime = Math.max(this.resetTime, waitTime);

        window.setTimeout(() => {
            // Special case: on empty href just reload the page to mimic the behaviour of <a>
            if (this.href == "") {
                // Note: the force-reload is deprecated: https://github.com/Microsoft/TypeScript/issues/28898
                // TODO: Do we need force reloading? There is no consensus on whether this is supported by all browsers
                //  anymore.
                window.location.reload(true);
            } else {
                window.open(this.href, this.target);
            }
            if (this.target != "_self") {
                this.reset();
            }
        }, waitTime * 1000);

        this.startReset(realResetTime);
    }
}
