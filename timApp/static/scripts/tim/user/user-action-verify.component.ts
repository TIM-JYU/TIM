import {
    AfterViewInit,
    ApplicationRef,
    Component,
    ComponentFactoryResolver,
    Directive,
    DoBootstrap,
    NgModule,
    OnInit,
    Type,
    ViewChild,
    ViewContainerRef,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {
    genericglobals,
    verificationglobals,
    VerificationType,
} from "tim/util/globals";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import * as t from "io-ts";
import {isRight} from "fp-ts/Either";
import {AlertSeverity} from "tim/ui/formErrorMessage";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {Channel} from "tim/messaging/listOptionTypes";
import {toPromise} from "tim/util/utils";

const GeneralInfoMarkup = t.type({
    type: t.string,
});

interface IGeneralInfoMarkup extends t.TypeOf<typeof GeneralInfoMarkup> {}

@Directive()
abstract class InfoComponentBase<A> implements OnInit {
    info!: Readonly<A & IGeneralInfoMarkup>;

    abstract get infoType(): t.Type<A>;

    ngOnInit(): void {
        const g = verificationglobals();
        const decoder = t.intersection([GeneralInfoMarkup, this.infoType]);
        const res = decoder.decode(g.verifyInfo);
        if (isRight(res)) {
            this.info = res.right;
        }
    }
}

const ContactInfoMarkup = t.type({
    channel: t.string,
    contact: t.string,
});

type ContactChannels = Exclude<
    Channel,
    Channel.EMAIL_LIST | Channel.TIM_MESSAGE
>;

const TYPE_NAMES: Record<ContactChannels, string> = {
    [Channel.EMAIL]: $localize`E-mail address`,
};

@Component({
    selector: "contact-ownership-info",
    template: `
        <p i18n>The following contact information will be added to your account:</p>
        <ul>
            <li i18n>Type: {{contactType}}</li>
            <li i18n>Contact information: <code>{{info.contact}}</code></li>
        </ul>
    `,
})
export class ContactOwnershipInfoComponent
    extends InfoComponentBase<t.TypeOf<typeof ContactInfoMarkup>>
    implements OnInit
{
    contactType!: string;

    get infoType() {
        return ContactInfoMarkup;
    }

    ngOnInit() {
        super.ngOnInit();

        this.contactType = TYPE_NAMES[this.info.channel as ContactChannels];
    }
}

@Component({
    selector: "set-primary-contact-info",
    template: `
        <p i18n><strong><code>{{info.contact}}</code></strong> will be made your primary {{contactType.toLowerCase()}}.</p>
    `,
})
export class SetPrimaryContactInfoComponent
    extends InfoComponentBase<t.TypeOf<typeof ContactInfoMarkup>>
    implements OnInit
{
    contactType!: string;

    get infoType() {
        return ContactInfoMarkup;
    }

    ngOnInit() {
        super.ngOnInit();

        this.contactType = TYPE_NAMES[this.info.channel as ContactChannels];
    }
}

const INFO_MAP: Record<VerificationType, Type<unknown> | undefined> = {
    [VerificationType.LIST_JOIN]: undefined,
    [VerificationType.CONTACT_OWNERSHIP]: ContactOwnershipInfoComponent,
    [VerificationType.SET_PRIMARY_CONTACT]: SetPrimaryContactInfoComponent,
};

const VERIFY_RETRUN_URLS: Record<VerificationType, string | undefined> = {
    [VerificationType.LIST_JOIN]: undefined,
    [VerificationType.CONTACT_OWNERSHIP]: $localize`Return to TIM settings.`,
    [VerificationType.SET_PRIMARY_CONTACT]: $localize`Return to TIM settings.`,
};

@Component({
    selector: "tim-user-action-verify",
    template: `
        <tim-alert *ngIf="verifyGlobals.verifyError" i18n>
            Cannot verify: {{verifyGlobals.verifyError}}
        </tim-alert>
        <tim-alert *ngIf="verificationResult" [severity]="verificationResult.severity">
            {{verificationResult.message}}
            <a *ngIf="verificationResult.returnUrl && verifyReturnUrl"
               [href]="verificationResult.returnUrl">
                {{verifyReturnUrl}}
            </a>
        </tim-alert>
        <bootstrap-panel [class.hidden]="verifyGlobals.verifyError || verificationResult" title="Verify action" i18n-title>
            <div>
                <ng-container #infoContainer></ng-container>
            </div>
            <p class="verify-prompt" i18n>
                Please verify or deny the action. You will not be able to access this page once you select either
                option.
            </p>
            <form>
                <fieldset [disabled]="processing || verifyComplete">
                    <button class="btn btn-danger" (click)="verify($event, false)" i18n>Decline</button>
                    <tim-loading *ngIf="processing"></tim-loading>
                    <button class="timButton" (click)="verify($event, true)" i18n>Accept</button>
                </fieldset>
            </form>
        </bootstrap-panel>
    `,
    styleUrls: ["user-action-verify.component.scss"],
})
export class UserActionVerifyComponent implements AfterViewInit {
    verifyGlobals = verificationglobals();
    @ViewChild("infoContainer", {read: ViewContainerRef})
    infoContainer!: ViewContainerRef;
    processing: boolean = false;
    verifyComplete: boolean = false;
    verifyReturnUrls = VERIFY_RETRUN_URLS;
    verificationResult?: {
        severity: AlertSeverity;
        message: string;
        returnUrl?: string;
    };
    verifyType?: VerificationType;

    constructor(
        private http: HttpClient,
        private cfr: ComponentFactoryResolver
    ) {
        if (this.verifyGlobals.verifyInfo) {
            this.verifyType = this.verifyGlobals.verifyInfo
                .type as VerificationType;
        }
    }

    get verifyReturnUrl() {
        if (!this.verifyType) {
            return "";
        }
        return this.verifyReturnUrls[this.verifyType] ?? "";
    }

    ngAfterViewInit(): void {
        if (!this.verifyType) {
            return;
        }
        const infoType = INFO_MAP[this.verifyType];
        if (!infoType) {
            this.verifyGlobals.verifyError = $localize`This verification type is not yet supported.`;
            return;
        }
        const factory = this.cfr.resolveComponentFactory(infoType);
        const componentRef = this.infoContainer.createComponent(factory);
        // Trigger ngOnInit on dynamic component
        componentRef.changeDetectorRef.detectChanges();
    }

    async verify(e: Event, verify: boolean) {
        e.preventDefault();
        if (!this.verifyType) {
            return;
        }
        this.processing = true;
        const res = await toPromise(
            // Use pathname to ensure relative URL so that CSRF token is passed along
            this.http.post<{returnUrl?: string}>(window.location.pathname, {
                verify,
            })
        );
        this.processing = false;
        if (res.ok) {
            if (verify) {
                this.verificationResult = {
                    severity: "success",
                    message: $localize`Verification successful.`,
                    returnUrl: res.result.returnUrl,
                };
            } else {
                this.verificationResult = {
                    severity: "warning",
                    message: $localize`Verification was successfully denied. If you believe you got this link by mistake, please report this to ${
                        genericglobals().config.helpEmail
                    }.`,
                    returnUrl: res.result.returnUrl,
                };
            }
            this.verifyComplete = true;
        } else {
            this.verificationResult = {
                severity: "danger",
                message: $localize`Could not verify action because of an error: ${
                    res.result.error.error
                }. Please report this to ${genericglobals().config.helpEmail}.`,
            };
        }
    }
}

@NgModule({
    declarations: [
        UserActionVerifyComponent,
        ContactOwnershipInfoComponent,
        SetPrimaryContactInfoComponent,
    ],
    exports: [UserActionVerifyComponent],
    imports: [BrowserModule, TimUtilityModule, HttpClientModule],
})
export class UserActionVerifyModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(UserActionVerifyModule);
});
doDowngrade(angularJsModule, "timUserActionVerify", UserActionVerifyComponent);
export const moduleDefs = [angularJsModule];
