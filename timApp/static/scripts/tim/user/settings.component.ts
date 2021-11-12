import {HttpClient, HttpClientModule} from "@angular/common/http";
import {
    ApplicationRef,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    DoCheck,
    Input,
    NgModule,
} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showAddContactDialog} from "tim/user/showAddContactDialog";
import {Channel} from "tim/messaging/listOptionTypes";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {ConsentType} from "../ui/consent";
import {
    ICssFile,
    INotification,
    ISettings,
    settingsglobals,
} from "../util/globals";
import {IOkResponse, timeout, to2} from "../util/utils";
import {ContactOrigin, IFullUser, IUserContact} from "./IUser";

@Component({
    selector: "settings-button-panel",
    template: `
        <tim-loading *ngIf="saving"></tim-loading>
        <button class="timButton" [disabled]="saving" (click)="save()" i18n>Save changes</button>
        <ng-content></ng-content>
    `,
    styleUrls: ["settings-button-panel.component.scss"],
})
export class SettingsButtonPanelComponent {
    saving = false;
    @Input() saved?: () => Promise<unknown>;

    async save() {
        this.saving = true;
        await this.saved?.();
        this.saving = false;
    }
}

@Component({
    selector: "user-settings",
    template: `
    `,
})
export class UserSettingsComponent {}

const EDITABLE_CONTACT_CHANNELS: Partial<Record<Channel, string>> = {
    [Channel.EMAIL]: $localize`Emails`,
};

interface ContactOriginInfo {
    bgColor: string;
    textColor: string;
    name: string;
}

const CONTACT_ORIGINS: Partial<Record<ContactOrigin, ContactOriginInfo>> = {
    [ContactOrigin.Haka]: {
        bgColor: "#9A0052",
        textColor: "#FFFFFF",
        name: "HAKA",
    },
    [ContactOrigin.Sisu]: {
        bgColor: "#3D3D3D",
        textColor: "#FFFFFF",
        name: "SISU",
    },
};

@Component({
    selector: "tim-settings",
    template: `
        <h1 i18n>TIM settings</h1>
        <div class="form">
            <bootstrap-form-panel [disabled]="saving" title="Preferred language" i18n-title>
                <div class="flex cl">
                    <tim-language-selector></tim-language-selector>
                    <settings-button-panel [saved]="submit"></settings-button-panel>
                </div>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Styles" i18n-title>
                <span *ngIf="cssFiles" i18n>Available themes:</span>
                <span *ngIf="!cssFiles" i18n>There are no available themes.</span>
                <div *ngFor="let css_file of cssFiles"
                     class="checkbox"><label>
                    <input type="checkbox"
                           name="settings.css_files[css_file.name]"
                           [(ngModel)]="settings.css_files[css_file.name]"
                           (change)="submit()"
                           [disabled]="saving">
                    <a href="/static/stylesheets/themes/{{ css_file.name }}.scss">
                        {{ css_file.name }}</a> - {{ css_file.desc }}
                </label></div>
                <div class="form-group">
                    <label for="customCssArea" i18n>Custom CSS:</label>
                    <textarea rows="15" id="customCssArea" class="form-control" name="custom_css"
                              [(ngModel)]="settings.custom_css"></textarea>
                </div>
                <settings-button-panel [saved]="submit">
                    <button class="btn btn-default" (click)="addPrintSettings()" i18n>Add Print Settings</button>
                </settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Editor" i18n-title>
                <div class="checkbox">
                    <label>
                        <input type="checkbox" name="use_document_word_list"
                               [(ngModel)]="settings.use_document_word_list">
                        <ng-container i18n>Use words from the document in ACE editor autocomplete</ng-container>
                    </label>
                </div>
                <label><ng-container i18n>ACE editor additional word list for autocomplete (1 word per line)</ng-container>
                    <textarea rows="15" class="form-control" name="word_list"
                              [(ngModel)]="settings.word_list"></textarea>
                </label>
                <settings-button-panel [saved]="submit"></settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Notifications" i18n-title>
                <h4 i18n>Subscribed items</h4>
                <p *ngIf="notifications.length > 0" i18n>You get emails from the following documents and folders:</p>
                <p *ngIf="notifications.length === 0" i18n>You haven't subscribed to any documents or folders.</p>
                <ul>
                    <li *ngFor="let n of notifications">
                        <a href="/manage/{{n.item.path}}">
                            <span *ngIf="n.item.isFolder" class="glyphicon glyphicon-folder-open"></span>
                            {{n.item.title}}</a>
                        <span *ngIf="n.email_doc_modify"
                              class="glyphicon glyphicon-pencil"
                              tooltip="Document modifications" i18n-tooltip></span>
                        <span *ngIf="n.email_comment_add"
                              class="glyphicon glyphicon-comment"
                              tooltip="New comments" i18n-tooltip></span>
                        <span *ngIf="n.email_comment_modify"
                              class="glyphicon glyphicon-comment"
                              tooltip="Comment modifications" i18n-tooltip></span>
                    </li>
                </ul>
                <button (click)="getAllNotifications()"
                        class="timButton"
                        *ngIf="showGetAllNotifications()" i18n>
                    Show all
                </button>
                <h4 i18n>Exclusion list</h4>
                <p i18n>
                    Sometimes you may want to subscribe to emails from a folder but exclude some documents within it.
                    Using the list below you can specify which folders and documents should be excluded from your email
                    subscriptions.
                </p>
                <p i18n>Type one regular expression per line that should match any part of the path of the folder or
                    document,
                    e.g. <code>/ht/</code> would match any path with <code>/ht/</code> in it.</p>
                <div class="form-group">
        <textarea class="form-control" name="email_exclude" rows="5" [(ngModel)]="settings.email_exclude">
        </textarea>
                </div>
                <settings-button-panel [saved]="submit"></settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Menus" i18n-title>
                <div class="form-view">
                    <label for="max-toc-size" class="input-group-label" i18n>
                        Collapse document index with more than
                    </label>
                    <div class="form-item">
                        <div class="input-group">
                            <input type="number" name="max_uncollapsed_toc_items" min="0" class="form-control"
                                   id="max-toc-size" placeholder="40 (default)" i18n-placeholder
                                   [(ngModel)]="settings.max_uncollapsed_toc_items">
                            <span class="input-group-addon" i18n>items</span>
                        </div>
                        <button class="timButton" (click)="settings.max_uncollapsed_toc_items = null" i18n>Clear</button>
                    </div>

                    <span i18n>Opening settings</span>
                    <div>
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="disable_menu_hover"
                                       [(ngModel)]="settings.disable_menu_hover"
                                       [disabled]="saving">
                                <ng-container i18n>Disable opening menus with mouse hover</ng-container>
                            </label>
                        </div>
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="remember_last_sidebar_menu_tab"
                                       [(ngModel)]="settings.remember_last_sidebar_menu_tab"
                                       [disabled]="saving">
                                <ng-container i18n>Side bar menu: remember the last selected tab</ng-container>
                            </label>
                        </div>
                        <div class="checkbox">
                            <label>
                                <input type="checkbox" name="remember_last_sidebar_menu_state"
                                       [(ngModel)]="settings.remember_last_sidebar_menu_state"
                                       [disabled]="saving">
                                <ng-container i18n>Side bar menu: remember the last open state</ng-container>
                            </label>
                        </div>
                    </div>
                </div>
                <settings-button-panel [saved]="submit"></settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Other settings" i18n-title>
                <div class="checkbox" id="othersettings"><label>
                    <input type="checkbox" name="auto_mark_all_read" [(ngModel)]="settings.auto_mark_all_read"
                           [disabled]="saving"> 
                    <ng-container i18n>Automatically mark document as read when opening it for the first time</ng-container>
                </label></div>
                <settings-button-panel [saved]="submit">
                    <button class="btn btn-default" (click)="clearLocalStorage()" i18n>Clear local settings storage</button>
                    <span *ngIf="storageClear" i18n>Local storage cleared.</span>
                </settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Your account information" i18n-title>
                <div class="account-info">
                    <label for="account-name" i18n>Username</label>
                    <div><input id="account-name" class="form-control" type="text" [value]="user.name" disabled>
                    </div>
                    <label for="account-id" i18n>Account ID</label>
                    <div><input id="account-id" class="form-control" type="text" [value]="user.id" disabled></div>
                    <label for="account-real-name" i18n>Full name</label>
                    <div><input id="account-real-name" class="form-control" type="text" [value]="user.real_name"
                                disabled></div>
                    <label for="account-email-primary" i18n>Primary email</label>
                    <div>
                        <select id="account-email-primary" name="primary-email-select" class="form-control"
                                [(ngModel)]="primaryEmail">
                            <ng-container *ngFor="let contact of userEmailContacts">
                                <option *ngIf="contact.verified"
                                        [ngValue]="contact">{{contact.contact}}</option>
                            </ng-container>
                        </select>
                        <div class="small">
                            <strong class="text-success" *ngIf="primaryChangeVerificationSent" i18n>
                                A verification email was sent to the current primary email.
                            </strong>
                            <p i18n>
                                Primary email is used to send you notifications and message list messages.
                            </p>
                            <p *ngIf="contactOrigins[primaryEmail.origin]; let originInfo" i18n>
                                This email is managed by <strong [style.color]="originInfo.bgColor">{{originInfo.name}}</strong>.
                                The address will be automatically updated by the system.
                            </p>
                        </div>
                    </div>
                    <ng-container *ngFor="let entry of userContactEntries">
                        <span class="form-label">{{channelNames[entry[0]]}}</span>
                        <div class="contact-collection">
                            <div class="contact-info" *ngFor="let contact of entry[1]">
                                <input type="text" class="form-control" [value]="contact.contact" disabled>
                                <span title="You will receive any TIM mail to this address." i18n-title *ngIf="contact.primary"
                                      class="primary-badge" i18n>Primary</span>
                                <span *ngIf="contactOrigins[contact.origin]; let originInfo"
                                      title="This email is managed by {{originInfo.name}}. You cannot delete managed contacts yourself."
                                      i18n-title
                                      [style.backgroundColor]="originInfo.bgColor"
                                      [style.color]="originInfo.textColor">{{originInfo.name}}</span>
                                <span title="This email is verified to be owned by you." i18n-title
                                      *ngIf="contact.verified"
                                      class="verified-badge" i18n>Verified</span>
                                <button *ngIf="!contact.verified" class="btn btn-default"
                                        (click)="resendVerification(contact)"
                                        [disabled]="verificationSentSet.has(contact)">
                                    <ng-container
                                            *ngIf="!verificationSentSet.has(contact); else verificationSentMessage" i18n>
                                        Resend verification
                                    </ng-container>
                                    <ng-template #verificationSentMessage i18n>
                                        Verification sent!
                                    </ng-template>
                                </button>
                                <button class="btn btn-danger" type="button"
                                        [disabled]="!canRemoveContact(contact)"
                                        (click)="deleteContact(contact)">
                                    <i class="glyphicon glyphicon-trash"></i>
                                </button>
                            </div>
                        </div>
                    </ng-container>
                </div>
                <settings-button-panel [saved]="saveUserAccountInfo">
                    <button class="timButton" (click)="openContactInfoDialog()" i18n>Add new contact</button>
                </settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Delete your account" i18n-title>
                <button class="timButton btn-danger"
                        (click)="beginDeleteAccount()"
                        *ngIf="!deletingAccount" i18n>Delete account...
                </button>
                <div *ngIf="deletingAccount">
                    <ng-container i18n>
                        To delete your account, please type your username ({{ user.name }}) in the field below
                        and then click "Delete account now".
                    </ng-container>
                    <div class="form-inline">
                        <input class="form-control"
                               type="text"
                               name="deleteConfirmName"
                               [(ngModel)]="deleteConfirmName">
                        <button [disabled]="user.name !== deleteConfirmName"
                                class="timButton btn-danger"
                                (click)="deleteAccount()" i18n>Delete account now
                        </button>
                        <button class="timButton btn-default" (click)="deletingAccount = false" i18n>Cancel</button>
                    </div>
                </div>
            </bootstrap-form-panel>
        </div>
    `,
    styleUrls: ["settings.component.scss"],
})
export class SettingsComponent implements DoCheck {
    saving = false;
    settings: ISettings;
    cssFiles: Array<ICssFile>;
    notifications: INotification[];
    storageClear = false;
    user: IFullUser;
    deletingAccount = false;
    deleteConfirmName = "";
    contacts: IUserContact[];
    userContacts = new Map<Channel, IUserContact[]>();
    userEmailContacts: IUserContact[] = [];
    canRemoveCustomContacts = true;
    primaryEmail!: IUserContact;
    channelNames = EDITABLE_CONTACT_CHANNELS;
    verificationSentSet = new Set<IUserContact>();
    contactOrigins = CONTACT_ORIGINS;
    primaryChangeVerificationSent = false;
    private readonly style: HTMLStyleElement;
    private readonly consent: ConsentType | undefined;
    private allNotificationsFetched = false;

    constructor(private http: HttpClient, private cdr: ChangeDetectorRef) {
        this.user = settingsglobals().current_user;
        this.consent = this.user.consent;
        this.settings = settingsglobals().userPrefs;
        this.cssFiles = settingsglobals().css_files;
        this.notifications = settingsglobals().notifications;
        this.contacts = settingsglobals().contacts;
        this.collectUserContacts();
        this.primaryEmail = this.getContactsFor(Channel.EMAIL).find(
            (c) => c.primary
        )!;
        this.updateCss();
        this.style = document.createElement("style");
        this.style.type = "text/css";
        document.getElementsByTagName("head")[0].appendChild(this.style);
    }

    get userContactEntries() {
        return [...this.userContacts.entries()];
    }

    canRemoveContact(contact: IUserContact): boolean {
        if (contact.primary) {
            return false;
        }
        if (contact.origin == ContactOrigin.Custom) {
            return this.canRemoveCustomContacts || !contact.verified;
        }
        return false;
    }

    ngDoCheck() {
        this.style.innerHTML = this.settings.custom_css;
    }

    async resendVerification(contact: IUserContact) {
        this.saving = true;
        await to2(
            this.http
                .post("/contacts/add", {
                    contact_info_type: contact.channel,
                    contact_info: contact.contact,
                    resend_if_exists: true,
                })
                .toPromise()
        );
        this.saving = false;
        this.verificationSentSet.add(contact);
        // TODO: Figure out why this is needed for change detection
        this.cdr.detectChanges();
    }

    async deleteContact(contact: IUserContact) {
        this.saving = true;
        const r = await to2(
            this.http
                .post("/contacts/remove", {
                    contact_info_type: contact.channel,
                    contact_info: contact.contact,
                })
                .toPromise()
        );
        this.saving = false;
        if (r.ok) {
            this.updateContacts(contact.channel, (contacts) =>
                contacts.filter((c) => c != contact)
            );
        } else {
            await showMessageDialog(r.result.error.error);
        }
        // TODO: Figure out why this is needed for change detection
        this.cdr.detectChanges();
    }

    submit = async () => {
        this.saving = true;
        const r = await to2(
            this.http
                .post<ISettings>("/settings/save", this.settings)
                .toPromise()
        );
        this.saving = false;
        if (r.ok) {
            this.settings = r.result;
            this.updateCss();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    };

    saveUserAccountInfo = async () => {
        this.primaryChangeVerificationSent = false;
        if (this.user.email == this.primaryEmail.contact) {
            return;
        }

        this.saving = true;
        const r = await to2(
            this.http
                .post<{verify: boolean}>("/contacts/primary", {
                    contact: this.primaryEmail.contact,
                    channel: this.primaryEmail.channel,
                })
                .toPromise()
        );
        this.saving = false;

        const currentPrimary = this.getContactsFor(Channel.EMAIL).find(
            (p) => p.primary
        )!;
        if (r.ok && r.result.verify) {
            this.user.email = this.primaryEmail.contact;
            currentPrimary.primary = false;
            this.primaryEmail.primary = true;
            this.primaryChangeVerificationSent = true;
        } else if (!r.ok) {
            this.primaryEmail = currentPrimary;
            await showMessageDialog(
                $localize`Failed to change the primary email: ${r.result.error.error}`
            );
        }
    };

    async updateConsent() {
        if (this.consent == null) {
            return;
        }
        this.saving = true;
        await this.setConsent(this.consent);
        this.saving = false;
    }

    updateCss() {
        document
            .querySelector(
                // There are two stylesheets in production (the other is the Angular-generated /js/styles.<hash>.css),
                // so we need the href match too.
                'link[rel="stylesheet"][href*="/static/generated/"]'
            )!
            .setAttribute(
                "href",
                `/static/generated/${this.settings.css_combined}.css`
            );
    }

    async clearLocalStorage() {
        window.localStorage.clear();
        this.storageClear = true;
        await timeout(3000);
        this.storageClear = false;
    }

    async addPrintSettings() {
        const resp = await to2(
            this.http
                .get("/static/stylesheets/userPrintSettings.css", {
                    responseType: "text",
                })
                .toPromise()
        );
        if (!resp.ok) {
            return;
        }
        this.settings.custom_css += resp.result;
    }

    async getAllNotifications() {
        const resp = await to2(
            this.http.get<INotification[]>("/notify/all").toPromise()
        );
        if (resp.ok) {
            this.notifications = resp.result;
            this.allNotificationsFetched = true;
        } else {
            await showMessageDialog(resp.result.error.error);
        }
    }

    showGetAllNotifications() {
        return (
            this.notifications.length === settingsglobals().notificationLimit &&
            !this.allNotificationsFetched
        );
    }

    beginDeleteAccount() {
        this.deleteConfirmName = "";
        this.deletingAccount = true;
    }

    async deleteAccount() {
        if (
            window.confirm(
                $localize`Are you sure you want to delete your account (${this.user.name})?`
            )
        ) {
            const r = await to2(
                this.http.post("/settings/account/delete", {}).toPromise()
            );
            if (r.ok) {
                location.href = "/";
            } else {
                await showMessageDialog(r.result.error.error);
            }
        }
    }

    /**
     * Open a dialog for user to add additional contact information.
     */
    async openContactInfoDialog() {
        return await showAddContactDialog((contact) => {
            this.updateContacts(contact.channel, (contacts) => [
                ...contacts,
                contact,
            ]);
            this.cdr.detectChanges();
        });
    }

    private getContactsFor(channel: Channel): IUserContact[] {
        if (!EDITABLE_CONTACT_CHANNELS[channel]) {
            return [];
        }
        let result = this.userContacts.get(channel);
        if (!result) {
            result = [];
            this.userContacts.set(channel, result);
        }
        return result;
    }

    private updateContacts(
        channel: Channel,
        update: (contacts: IUserContact[]) => IUserContact[]
    ) {
        const newContacts = update(this.getContactsFor(channel)).sort((a, b) =>
            a.contact.localeCompare(b.contact)
        );
        if (channel == Channel.EMAIL) {
            this.userEmailContacts = newContacts;
            this.canRemoveCustomContacts =
                newContacts.filter(
                    (c) => c.origin == ContactOrigin.Custom && c.verified
                ).length > 1;
        }
        this.userContacts.set(channel, newContacts);
    }

    private collectUserContacts() {
        this.userContacts.clear();
        this.userEmailContacts = this.getContactsFor(Channel.EMAIL);
        for (const contactInfo of this.contacts) {
            if (!EDITABLE_CONTACT_CHANNELS[contactInfo.channel]) {
                continue;
            }
            this.getContactsFor(contactInfo.channel).push(contactInfo);
        }
        for (const ch of this.userContacts.keys()) {
            this.updateContacts(ch, (c) => c);
        }
    }

    private async setConsent(c: ConsentType) {
        const r = await to2(
            this.http
                .post<IOkResponse>("/settings/updateConsent", {consent: c})
                .toPromise()
        );
        if (r.ok) {
            // Nothing to do.
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }
}

@NgModule({
    declarations: [
        UserSettingsComponent,
        SettingsComponent,
        SettingsButtonPanelComponent,
    ],
    exports: [SettingsComponent],
    imports: [
        BrowserModule,
        TimUtilityModule,
        HttpClientModule,
        FormsModule,
        TooltipModule.forRoot(),
    ],
})
export class SettingsModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(SettingsModule);
});
doDowngrade(angularJsModule, "timSettings", SettingsComponent);
export const moduleDefs = [angularJsModule];
