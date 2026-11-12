import {
    Component,
    ElementRef,
    NgModule,
    ViewChild,
    ChangeDetectionStrategy,
} from "@angular/core";
import {
    HttpClient,
    provideHttpClient,
    withInterceptorsFromDi,
    withXhr,
} from "@angular/common/http";
import {BrowserQRCodeSvgWriter} from "@zxing/browser";
import {
    EncodeHintType,
    QRCodeDecoderErrorCorrectionLevel,
} from "@zxing/library";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {toPromise} from "tim/util/utils";
import {Users} from "tim/user/userService";
import type {IFullUser} from "tim/user/IUser";
import {CommonModule} from "@angular/common";

interface ISessionStatus {
    sessionId: string;
    valid: boolean;
}

const QR_CODE_SIZE = 350;

@Component({
    selector: "tim-session-check-dialog",
    template: `
        <div class="modal-bg"></div>
        <tim-dialog-frame [minimizable]="false" size="lg">
          <ng-container header i18n>
                Session check
            </ng-container>
          <ng-container body>
            @if (currentSession && !currentSession.valid && recheck && !isCheckingStatus) {
              <tim-alert i18n>
                    The session is expired. Ask the supervisor to scan the QR code below.
                </tim-alert>
            }
            <h2 i18n>Verify a new login</h2>
            <p i18n>This session was expired because you logged in too many times.</p>
            <p>
              <strong i18n>
                        Please ask the supervisor to scan the code below to continue.
                    </strong>
            </p>
            <div class="qr-container">
              <div class="qr-code" #qrCode></div>
              <div class="qr-info">{{currentUser?.name}} ({{currentUser?.real_name}})</div>
            </div>
            <p i18n>Once the code has been scanned, press "Check status" to re-check your verification.</p>
          </ng-container>
          <ng-container footer>
            <div class="footer">
              @if (isCheckingStatus) {
                <tim-loading></tim-loading>
              }
              <button class="timButton" [disabled]="isCheckingStatus" (click)="checkStatus(true)" i18n>
                        Check status
                    </button>
            </div>
          </ng-container>
        </tim-dialog-frame>
        `,
    styleUrls: ["./session-check-dialog.component.scss"],
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false,
})
export class SessionCheckDialogComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    protected dialogName = "SessionCheckDialog";
    isCheckingStatus = false;
    currentSession?: ISessionStatus;
    qrWriter: BrowserQRCodeSvgWriter = new BrowserQRCodeSvgWriter();
    recheck = false;
    currentUser?: IFullUser;

    @ViewChild("qrCode", {static: true})
    qrCode!: ElementRef<HTMLDivElement>;

    constructor(private http: HttpClient) {
        super();
    }

    async ngOnInit() {
        await this.checkStatus();
        if (!this.currentSession) {
            return;
        }
        this.currentUser = Users.getCurrent();
        this.qrWriter.writeToDom(
            this.qrCode.nativeElement,
            `${this.currentUser.name}#${this.currentSession.sessionId}`,
            QR_CODE_SIZE,
            QR_CODE_SIZE,
            new Map<EncodeHintType, unknown>([
                [
                    EncodeHintType.ERROR_CORRECTION,
                    QRCodeDecoderErrorCorrectionLevel.L,
                ],
                [EncodeHintType.MARGIN, 0],
            ])
        );
    }

    async checkStatus(recheck = false) {
        this.recheck = recheck;
        this.isCheckingStatus = true;
        const r = await toPromise(
            this.http.get<ISessionStatus>("/user/sessions/current")
        );
        if (r.ok) {
            this.currentSession = r.result;
            if (this.currentSession.valid) {
                this.close(true);
            }
        } else {
            return;
        }
        this.isCheckingStatus = false;
    }

    ngAfterViewInit(): void {
        super.ngAfterViewInit();
        this.frame.closeFn = undefined;
    }
}

@NgModule({
    declarations: [SessionCheckDialogComponent],
    imports: [CommonModule, TimUtilityModule, DialogModule],
    providers: [provideHttpClient(withXhr(), withInterceptorsFromDi())],
})
export class SessionCheckDialogModule {}
