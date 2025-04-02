import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import type {Moment} from "moment";
import moment from "moment";
import {formatString, toPromise} from "tim/util/utils";
import humanizeDuration from "humanize-duration";
import {Users} from "tim/user/userService";
import {HttpClient} from "@angular/common/http";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {IDocumentViewInfoStatus} from "tim/document/viewctrl";
import type {IRight} from "tim/item/access-role.service";

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
    `,
    styleUrls: ["./goto-link.component.scss"],
})
export class GotoLinkComponent implements OnInit {
    @Input() href = "";
    @Input() waitText?: string;
    @Input()
    countdownText: string = $localize`Opens in ${"{"}:INTERPOLATION:0${"}"}:INTERPOLATION_1:.`;
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
    @Input() noCountdown: boolean = false;
    openTime?: Moment;
    linkDisabled = false;
    linkState = GotoLinkState.Ready;
    resetTimeout?: number;
    error?: GotoError;
    unsavedChangesChecked = false;

    constructor(private http: HttpClient) {}

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
        return humanizeDuration(pastDue * 1000, {
            language: this.timeLang ?? Users.getCurrentLanguage(),
        });
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
        if (
            url.hostname == window.location.hostname &&
            path.startsWith(VIEW_PATH)
        ) {
            const docPath = path.substring(VIEW_PATH.length);
            const accessInfo = await toPromise<
                IDocumentViewInfoStatus,
                {error: IDocumentViewInfoStatus; status?: number}
            >(
                this.http.get<IDocumentViewInfoStatus>(
                    `/docViewInfo/${docPath}`
                )
            );
            if (accessInfo.ok) {
                const info = accessInfo.result;
                return {
                    unauthorized: !info.can_access,
                    access: info.right,
                };
            } else if (accessInfo.result.status === 403) {
                // docViewInfo returns info via the error when it's 403
                const info = accessInfo.result.error;
                return {
                    unauthorized: !info.can_access,
                    access: info.right,
                };
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
        if (this.isCountdown) {
            return;
        }

        this.stopReset();
        this.error = undefined;
        this.linkDisabled = true;

        const {unauthorized, access} = await this.resolveAccess();
        await this.startImpl(unauthorized, access);
    }

    private async startImpl(unauthorized: boolean, access: IRight | undefined) {
        if (unauthorized && (!access || this.noCountdown)) {
            this.showError({
                userMessage: this.unauthorizedText,
                defaultMessage: $localize`You don't have permission to view that document.`,
            });
            return;
        }

        const openTime = this.parseTime(
            this.openAt,
            access?.accessible_from ?? access?.duration_from
        );
        const closeTime = this.parseTime(
            this.closeAt,
            access?.accessible_to ?? access?.duration_to
        );

        let curTime = moment();
        if (closeTime || openTime) {
            const serverTime = await toPromise(
                this.http.get<{time: Moment}>("/time")
            );
            // Fail silently here and hope the user clicks again so it can retry
            if (!serverTime.ok) {
                this.linkDisabled = false;
                return;
            }
            curTime = serverTime.result.time;
        }

        if (closeTime?.isValid() && closeTime.isBefore(curTime)) {
            const pastDue = this.getPastDueTime(
                closeTime.diff(curTime, "seconds")
            );
            this.showError({
                userMessage: this.pastDueText
                    ? formatString(this.pastDueText, pastDue)
                    : undefined,
                defaultMessage: $localize`Your access expired ${pastDue} ago.`,
            });
            return;
        }

        if (
            !this.unsavedChangesChecked &&
            this.checkUnsaved &&
            vctrlInstance?.checkUnSavedComponents()
        ) {
            this.showError({
                userMessage: this.unsavedChangesText,
                defaultMessage: $localize`You have unsaved changes. Save them or click the link again.`,
            });
            this.unsavedChangesChecked = true;
            this.linkDisabled = false;
            return;
        }

        if (openTime?.isValid()) {
            this.openTime = openTime;
        }

        if (
            openTime?.isValid() &&
            openTime.diff(curTime, "seconds", true) <= 0
        ) {
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

    async countdownDone() {
        // re-check permissions before allowing the link to be used
        const {unauthorized, access} = await this.resolveAccess();

        // user is not (yet) authorized to access the document, but may have access permissions
        // that come into effect later
        if (unauthorized) {
            this.reset();
            void this.startImpl(unauthorized, access);
            return;
        }

        if (this.stopAfterCountdown) {
            this.showError({
                defaultMessage: $localize`You can access the link now.`,
                displayClass: "info",
            });
            this.linkDisabled = false;
            return;
        } else {
            this.startOpenLink();
        }
    }

    startReset(resetTime: number) {
        this.resetTimeout = window.setTimeout(
            () => this.reset(),
            resetTime * 1000
        );
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
        if (this.isGoing) {
            return;
        }
        this.linkDisabled = true;
        this.linkState = GotoLinkState.Goto;
        const waitTime = Math.random() * Math.max(this.maxWait, 0);
        const realResetTime = Math.max(this.resetTime, waitTime);

        window.setTimeout(() => {
            // Special case: on empty href just reload the page to mimic the behaviour of <a>
            if (this.href == "") {
                window.location.reload();
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
