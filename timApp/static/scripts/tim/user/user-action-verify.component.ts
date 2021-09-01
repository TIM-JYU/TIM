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
import {verificationglobals, VerificationType} from "tim/util/globals";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import * as t from "io-ts";
import {isRight} from "fp-ts/Either";
import {createDowngradedModule, doDowngrade} from "../downgrade";

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

const ConstactOwnershipMarkup = t.type({
    channel: t.string,
    contact: t.string,
});

@Component({
    selector: "contact-ownership-info",
    template: `
        <p>Contact ownership!</p>
    `,
})
export class ContactOwnershipInfoComponent
    extends InfoComponentBase<t.TypeOf<typeof ConstactOwnershipMarkup>>
    implements OnInit {
    get infoType() {
        return ConstactOwnershipMarkup;
    }

    ngOnInit() {
        super.ngOnInit();
        console.log(this.info);
    }
}

const INFO_MAP: Record<VerificationType, Type<unknown> | undefined> = {
    [VerificationType.LIST_JOIN]: undefined,
    [VerificationType.CONTACT_OWNERSHIP]: ContactOwnershipInfoComponent,
};

@Component({
    selector: "tim-user-action-verify",
    template: `
        <tim-alert *ngIf="verifyGlobals.error" i18n>
            Cannot verify because: {{verifyGlobals.error}}
        </tim-alert>
        <ng-container *ngIf="!verifyGlobals.error">
            <h1>Verification</h1>
            <bootstrap-panel title="Verify">
                <div>
                    <ng-container #infoContainer></ng-container>
                </div>
                <!--            <div *ngIf="!error && verificationToken">-->
                <!--                <p>Are you sure you wish to verify this contact information?</p>-->
                <!--                <p>Type: {{channel}}</p>-->
                <!--                <p>Information: {{contactInfo}} </p>-->
                <!--                <button class="timButton" (click)="callVerify()" [disabled]="verificationSucceeded">Verify</button>-->
                <!--            </div>-->
                <!--            <div *ngIf="error">-->
                <!--                <p>An error has occured.<span *ngIf="errorCode"> Details below:</span></p>-->
                <!--                <tim-alert severity="danger" *ngIf="errorCode == '01'">Invalid verification link. Check that the link in-->
                <!--                    the browser's address bar is the same as in the message you used to arrive to this site.-->
                <!--                </tim-alert>-->
                <!--                <tim-alert severity="danger" *ngIf="errorCode == '02'">This contact info is already verified.-->
                <!--                </tim-alert>-->
                <!--                <tim-alert severity="danger" *ngIf="errorCode == '03'">This contact info is already verified.-->
                <!--                </tim-alert>-->
                <!--            -->
                <!--            </div>-->
                <!--            <div *ngIf="verificationSucceeded !== undefined">-->
                <!--                <tim-alert severity="success" *ngIf="verificationSucceeded">Verification succeeded! Your new contact-->
                <!--                    information has been linked with your TIM profile.-->
                <!--                </tim-alert>-->
                <!--                <tim-alert severity="danger" *ngIf="!verificationSucceeded">Verification failed! If the problem-->
                <!--                    persists, please contact TIM's support (details in the site footer).-->
                <!--                </tim-alert>-->
                <!--            </div>-->
            </bootstrap-panel>
        </ng-container>
    `,
})
export class UserActionVerifyComponent implements AfterViewInit {
    verifyGlobals = verificationglobals();
    @ViewChild("infoContainer", {read: ViewContainerRef})
    infoContainer!: ViewContainerRef;

    // A flag if the verification succeeded. For the value of undefined assume that the verification POST call has not
    // yet been made.
    verificationSucceeded?: boolean = undefined;

    constructor(
        private http: HttpClient,
        private cfr: ComponentFactoryResolver
    ) {}

    ngAfterViewInit(): void {
        const infoType =
            INFO_MAP[this.verifyGlobals.verifyInfo.type as VerificationType];
        if (!infoType) {
            return;
        }
        const factory = this.cfr.resolveComponentFactory(infoType);
        const componentRef = this.infoContainer.createComponent(factory);
        // Trigger ngOnInit on dynamic component
        componentRef.changeDetectorRef.detectChanges();
    }

    // /**
    //  * HTTP POST to server to finalize verifying a user's new contact information.
    //  */
    // async callVerify() {
    //     // If verification would for some reason be undefined, then there is no reason to call the server with a POST
    //     // method. Also helps in typing.
    //     if (!this.verificationToken) {
    //         return;
    //     }
    //     // If any kind of error flag is up, then calling for verification doesn't make sense.
    //     if (this.error) {
    //         return;
    //     }
    //
    //     const r = await to2(
    //         this.http
    //             .post(`/verification/contact/${this.verificationToken}`, {})
    //             .toPromise()
    //     );
    //
    //     if (r.ok) {
    //         this.verificationSucceeded = true;
    //     } else {
    //         this.error = true;
    //         this.errorCode = r.result.error.error;
    //     }
    // }
}

@NgModule({
    declarations: [UserActionVerifyComponent, ContactOwnershipInfoComponent],
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
