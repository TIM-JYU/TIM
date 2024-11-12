import {HttpClient, HttpClientModule} from "@angular/common/http";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    DoCheck,
} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    Input,
    NgModule,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showAddContactDialog} from "tim/user/showAddContactDialog";
import {showAddAPIKeyDialog} from "tim/user/showAddAPIDialog";
import {Channel} from "tim/messaging/listOptionTypes";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {TabsetComponent, TabsModule} from "ngx-bootstrap/tabs";
import type {DndDropEvent} from "ngx-drag-drop";
import {DndModule} from "ngx-drag-drop";
import {polyfill} from "mobile-drag-drop";
import {scrollBehaviourDragImageTranslateOverride} from "mobile-drag-drop/scroll-behaviour";
import {ProgressbarModule} from "ngx-bootstrap/progressbar";
import type {ConsentType} from "tim/ui/consent";
import type {INotification, ISettings} from "tim/util/globals";
import {NotificationType, settingsglobals} from "tim/util/globals";
import type {IOkResponse} from "tim/util/utils";
import {isIOS, replaceStyle, timeout, to2, toPromise} from "tim/util/utils";
import type {DocumentOrFolder, ITranslatorUsage} from "tim/item/IItem";
import type {IFullUser, IUserApiKey, IUserContact} from "tim/user/IUser";
import {ContactOrigin} from "tim/user/IUser";
import type {TimTable} from "tim/plugin/timTable/tim-table.component";
import {
    TimTableComponent,
    TimTableModule,
} from "tim/plugin/timTable/tim-table.component";
import {ParMenuHandlePosition} from "tim/document/editing/editing";

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
    @Input() reloadAfterSave = false;

    async save() {
        this.saving = true;
        await this.saved?.();
        this.saving = false;
        if (this.reloadAfterSave) {
            location.reload();
        }
    }
}

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

const STYLE_TABLE_HEADERS = [
    "docId",
    $localize`Style`,
    $localize`Description`,
    $localize`Type`,
];

const HIDDEN_COLUMNS_OFFICIAL_USERS = [0];
const HIDDEN_COLUMNS_OFFICIAL_ONLY = [0, 3];
type SettingsWithStylePath = ISettings & {style_path?: string};

interface StyleDocumentInfo {
    docId: number;
    name?: string;
    path: string;
    description?: string;
    type?: string;
}

interface GroupedNotification {
    item: DocumentOrFolder;
    notificationTypes: Set<NotificationType>;
}

type StyleDocumentInfoAll = Required<StyleDocumentInfo>;

// TODO: Figure out why this requires manual cdr.detectChanges() in many places
@Component({
    selector: "tim-settings",
    template: `
        <h1 i18n>TIM settings</h1>
        <div class="form">
            <bootstrap-form-panel [disabled]="saving" title="Preferred language" i18n-title anchorId="prefs_language" [showHeadingAnchors]="true">
                <div class="flex cl">
                    <tim-language-selector [(language)]="settings.language"></tim-language-selector>
                    <settings-button-panel [saved]="submit" [reloadAfterSave]="true"></settings-button-panel>
                </div>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Styles" i18n-title anchorId="prefs_styles" [showHeadingAnchors]="true">
                <tim-alert *ngIf="styleError">
                    <strong>{{styleError.title}}</strong>
                    <p>{{styleError.message}}</p>
                </tim-alert>
                <tabset #stylesTabs>
                    <tab heading="Selected styles" i18n-heading>
                        <div class="info-box">
                            <p i18n>
                                Styles that you currently use are listed here.
                                You can review and remove styles that you use.
                            </p>
                            <p i18n>
                                You can also reorder styles by dragging them in the list below.
                                Reordering the styles will change their priority which might affect the final generated theme for TIM.
                            </p>
                        </div>
                        <div class="style-loader" *ngIf="currentStyles === undefined">
                            <tim-loading></tim-loading>
                            <ng-container i18n>Loading active styles, please wait</ng-container>
                        </div>
                        <ng-container *ngIf="currentStyles !== undefined">
                            <div class="info-box" *ngIf="currentStyles.length == 0" >
                                <small i18n>
                                    You have no styles selected. You can add styles via the <a (click)="changeStyleTab(1)">Available styles</a> tab.
                                </small>
                            </div>
                            <div *ngIf="currentStyles.length > 0" class="current-styles" [dndDropzone]="['userStyle']"
                                 [dndEffectAllowed]="'move'"
                                 (dndDrop)="onStyleDrop($event)">
                                <div class="drag-placeholder" dndPlaceholderRef></div>
                                <div *ngFor="let style of currentStyles"
                                     [dndDraggable]="style.docId"
                                     [dndEffectAllowed]="'move'"
                                     [dndDisableDragIf]="saving"
                                     dndType="userStyle">
                                    <i class="glyphicon glyphicon-sort sort-handle" [class.disabled]="saving" dndHandle></i>
                                    <div class="style-info">
                                        <h4>
                                            <a class="style-header" href="/view/{{style.path}}">{{style.name}}</a>
                                            <span *ngIf="style.type == 'official'" class="label label-primary" i18n>Official</span>
                                            <span *ngIf="style.type == 'deleted'" class="label label-default" i18n>Deleted</span>
                                        </h4>
                                        <p class="style-description">{{style.description}}</p>
                                    </div>
                                    <div class="style-actions">
                                        <button class="btn btn-danger" title="Delete style" (click)="deleteSelectedStyle(style)" i18n-title>
                                            <i class="glyphicon glyphicon-trash"></i>
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </ng-container>
                    </tab>
                    <tab heading="Available styles" #availableTab="tab" (selectTab)="reloadStyleTable()" (deselect)="resetSelectedStyles()" i18n-heading>
                        <div class="info-box">
                            <p i18n>
                                The table below lists all styles available on TIM.
                                You can preview and add new styles using the table below.
                            </p>
                            <ul>
                                <li i18n><strong>Official styles</strong> are maintained and supported by TIM
                                    maintainers.
                                </li>
                                <li i18n><strong>User-made styles</strong> are made and maintained by TIM's users. For more information, see <a href="/view/tim/ohjeita/styles">style authoring guide</a>.</li>
                            </ul>
                            <p i18n>To remove styles or edit their ordering, use the <a (click)="changeStyleTab(0)">Selected
                                styles</a> tab.</p>
                            <p i18n><strong>Note:</strong> Your current styles will be temporarily disabled while previewing.</p>
                        </div>
                        <div class="style-loader" *ngIf="tableData.table.rows === undefined">
                            <tim-loading></tim-loading> <ng-container i18n>Loading available styles, please wait</ng-container>
                        </div>
                        <ng-container *ngIf="tableData.table.rows !== undefined">
                            <div class="info-box" *ngIf="tableData.table.rows.length == 0" >
                                <small i18n>
                                    No styles available.
                                </small>
                            </div>
                            <ng-container *ngIf="tableData.table.rows.length > 0">
                                <div class="checkbox">
                                    <label>
                                        <input type="checkbox" id="style-show-user-made" name="style-show-user-made"
                                               [ngModel]="showUserStyles" (ngModelChange)="changeUserStyleVisibility($event)">
                                        <ng-container i18n>Show user-made styles</ng-container>
                                    </label>
                                </div>
                                <tim-table class="style-listing" *ngIf="availableTab.active" [data]="tableData"></tim-table>
                                <div class="button-panel">
                                    <button class="timButton" [disabled]="!cbCount" (click)="activateSelectedStyles()" i18n>
                                        Add to Selected styles
                                    </button>
                                    <button class="timButton" [disabled]="!cbCount" (click)="activateSelectedStyles(true)" i18n>
                                        Add to Quick style
                                    </button>
                                    <button class="timButton" [disabled]="!cbCount" (click)="resetSelectedStyles()" i18n>
                                        Clear preview
                                    </button>
                                </div>
                            </ng-container>
                        </ng-container>
                    </tab>
                    <tab heading="Custom CSS" i18n-heading>
                        <div class="info-box">
                            <p i18n>
                                You can define your personal custom styles using CSS.
                                The styles are applied to all TIM pages and all devices you use.
                            </p>
                            <p i18n>
                                For extended styling using SCSS, refer to <a href="/view/tim/ohjeita/styles">style authoring guide</a>.
                            </p>
                        </div>
                        <div class="form-group">
                            <label for="custom_css" i18n>Custom CSS</label>
                            <textarea id="custom_css" name="custom_css" class="form-control" [(ngModel)]="settings.custom_css"></textarea>
                        </div>
                        <settings-button-panel [saved]="submit">
                            <button class="btn btn-default" (click)="addPrintSettings()" i18n>Add Print Settings</button>
                        </settings-button-panel>
                    </tab>
                </tabset>
                <div class="style-loader" *ngIf="loadingUserStyle">
                    <tim-loading></tim-loading>
                    <ng-container i18n>Compiling style, please wait</ng-container>
                </div>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Editor" i18n-title anchorId="prefs_editor" [showHeadingAnchors]="true">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" name="use_document_word_list"
                               [(ngModel)]="settings.use_document_word_list">
                        <ng-container i18n>Use words from the document in ACE editor autocomplete</ng-container>
                    </label>
                </div>
                <label>
                    <ng-container i18n>ACE editor additional word list for autocomplete (1 word per line)</ng-container>
                    <textarea rows="15" class="form-control" name="word_list"
                              [(ngModel)]="settings.word_list"></textarea>
                </label>
                <settings-button-panel [saved]="submit"></settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Notifications" i18n-title anchorId="prefs_notifications" [showHeadingAnchors]="true">
                <h4 i18n>Subscribed items</h4>
                <p *ngIf="notifications.length > 0" i18n>You get emails from the following documents and folders:</p>
                <p *ngIf="notifications.length === 0" i18n>You haven't subscribed to any documents or folders.</p>
                <ul>
                    <li *ngFor="let n of notifications">
                        <a href="/manage/{{n.item.path}}">
                            <span *ngIf="n.item.isFolder" class="glyphicon glyphicon-folder-open"></span>
                            {{n.item.title}}</a>
                        <i *ngIf="isDocModify(n)"
                              class="notification-icon glyphicon glyphicon-pencil"
                              title="Document modifications" i18n-title></i>
                        <i *ngIf="isCommentAdd(n)"
                              class="notification-icon glyphicon glyphicon-send"
                              title="New comments" i18n-title></i>
                        <i *ngIf="isCommentModify(n)"
                              class="notification-icon glyphicon glyphicon-comment"
                              title="Comment modifications" i18n-title></i>
                        <i *ngIf="isAnswerAdd(n)"
                              class="notification-icon glyphicon glyphicon-open-file"
                              title="Answer posts" i18n-title></i>
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
            <bootstrap-form-panel [disabled]="saving" title="Menus" i18n-title anchorId="prefs_menus" [showHeadingAnchors]="true">
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
                        <button class="timButton" (click)="settings.max_uncollapsed_toc_items = null" i18n>
                            Clear
                        </button>
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
                    <label for="par-menu-position" i18n>Paragraph menu position</label>
                    <div class="select-option" id="par-menu-pos-option">
                        <div>
                            <select [(ngModel)]="settings.parmenu_position" id="par-menu-position">
                                <option [ngValue]="ParMenuHandlePosition.Left">
                                    <ng-container i18n>Left</ng-container>
                                </option>
                                <!-- Current default setting is on the right of the paragraph -->
                                <option [ngValue]="ParMenuHandlePosition.Right">
                                    <ng-container i18n>Right</ng-container>
                                </option>
                            </select>
                        </div>
                        <span style="padding-left: 1em;" [ngSwitch]="settings.parmenu_position">
                            <ng-container *ngSwitchCase="ParMenuHandlePosition.Left" i18n>
                                Left of the active paragraph, marked with a vertical blue bar.</ng-container> 
                            <ng-container *ngSwitchCase="ParMenuHandlePosition.Right" i18n>
                                Right of the active paragraph, marked with a pencil icon.</ng-container>
                        </span>
                    </div>
                </div>
                <settings-button-panel [saved]="submit"></settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Other settings" i18n-title anchorId="prefs_misc" [showHeadingAnchors]="true">
                <div class="checkbox" id="othersettings"><label>
                    <input type="checkbox" name="auto_mark_all_read" [(ngModel)]="settings.auto_mark_all_read"
                           [disabled]="saving">
                    <ng-container i18n>Automatically mark document as read when opening it for the first time
                    </ng-container>
                </label></div>
                <settings-button-panel [saved]="submit">
                    <button class="btn btn-default" (click)="clearLocalStorage()" i18n>Clear local settings storage
                    </button>
                    <span *ngIf="storageClear" i18n>Local storage cleared.</span>
                </settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Your account information" i18n-title anchorId="prefs_account" [showHeadingAnchors]="true">
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
                                This email is managed by <strong
                                    [style.color]="originInfo.bgColor">{{originInfo.name}}</strong>.
                                The address will be automatically updated by the system.
                            </p>
                        </div>
                    </div>
                    <ng-container *ngFor="let entry of userContactEntries">
                        <span class="form-label">{{channelNames[entry[0]]}}</span>
                        <div class="contact-collection">
                            <div class="contact-info" *ngFor="let contact of entry[1]">
                                <input type="text" class="form-control" [value]="contact.contact" disabled>
                                <span title="You will receive any TIM mail to this address." i18n-title
                                      *ngIf="contact.primary"
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
                                            *ngIf="!verificationSentSet.has(contact); else verificationSentMessage"
                                            i18n>
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
                    <ng-container>
                        <span class="form-label">
                            <ng-container i18n>Machine translator API Keys</ng-container>
                            <a href="https://tim.jyu.fi/view/tim/ohjeita/dokumenttien-konekaantaminen/en-GB#adding-a-translator-authentication-key">
                                <span class="glyphicon glyphicon-question-sign" style="margin-left: 0.2em;" title="Help with machine translation setup" i18n-title></span>
                            </a>
                        </span>
                        <div class="contact-collection">
                            <div class="contact-info" *ngFor="let APIkey of userAPIKeys">
                                <input type="text" class="form-control" [value]="APIkey.APIkey" disabled>
                                <input type="text" class="form-control buttonBorder" [value]="APIkey.translator" disabled>
                                <div *ngIf="APIkey.quotaChecked" class="stacked quotaProgressBar">
                                    <progressbar tooltip="{{APIkey.usedQuota}} / {{APIkey.availableQuota}}" [value]="APIkey.usedQuota" [max]="APIkey.availableQuota"></progressbar>
                                </div>
                                <button *ngIf="!APIkey.quotaChecked" class="btn" type="button" (click)="checkQuota(APIkey)" i18n>
                                    Check key's quota 
                                </button>
                                    <button *ngIf="APIkey.quotaChecked" class="btn" type="button" (click)="checkQuota(APIkey)">
                                        <i class="glyphicon glyphicon-refresh"></i>
                                    </button>
                                <button class="btn btn-danger" type="button"
                                        (click)="deleteKey(APIkey)">
                                    <i class="glyphicon glyphicon-trash"></i>
                                </button>
                            </div>
                        </div>
                    </ng-container>
                </div>
                <settings-button-panel [saved]="saveUserAccountInfo">
                    <button class="timButton" (click)="openContactInfoDialog()" i18n>Add new contact</button>
                    <button class="timButton" (click)="openAPIKeyDialog()" i18n>Add new API key</button>
                </settings-button-panel>
            </bootstrap-form-panel>
            <bootstrap-form-panel [disabled]="saving" title="Delete your account" i18n-title anchorId="prefs_delAcc" [showHeadingAnchors]="true">
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
export class SettingsComponent implements DoCheck, AfterViewInit {
    saving = false;
    settings: SettingsWithStylePath;
    notifications: GroupedNotification[];
    storageClear = false;
    user: IFullUser;
    deletingAccount = false;
    deleteConfirmName = "";
    contacts: IUserContact[];
    userContacts = new Map<Channel, IUserContact[]>();
    userAPIKeys: IUserApiKey[] = [];
    userEmailContacts: IUserContact[] = [];
    canRemoveCustomContacts = true;
    primaryEmail!: IUserContact;
    channelNames = EDITABLE_CONTACT_CHANNELS;
    verificationSentSet = new Set<IUserContact>();
    contactOrigins = CONTACT_ORIGINS;
    currentStyles?: StyleDocumentInfo[] = undefined;
    primaryChangeVerificationSent = false;
    showUserStyles = false;
    private readonly style: HTMLStyleElement;
    private readonly consent: ConsentType | undefined;
    private allNotificationsFetched = false;
    cbCount = 0;
    tableData: TimTable = {
        hide: {edit: false, insertMenu: true, editMenu: true},
        hideSaveButton: true,
        isPreview: false,
        table: {},
        filterRow: true,
        cbColumn: true,
        maxRows: "800px",
        maxWidth: "100%",
        maxCols: "100%",
        headers: STYLE_TABLE_HEADERS,
        hiddenColumns: HIDDEN_COLUMNS_OFFICIAL_ONLY,
    };
    @ViewChildren(TimTableComponent)
    timTable!: QueryList<TimTableComponent>;
    userStyles: number[] = [];
    loadingUserStyle = false;
    @ViewChild("stylesTabs", {static: false}) stylesTabs!: TabsetComponent;
    styleError?: {title: string; message: string};

    constructor(private http: HttpClient, private cdr: ChangeDetectorRef) {
        this.getKeys();
        this.user = settingsglobals().current_user;
        this.consent = this.user.consent;
        this.settings = settingsglobals().userPrefs;
        this.settings.style_path = this.currentStyle;
        this.notifications = this.groupNotifications(
            settingsglobals().notifications
        );
        this.contacts = settingsglobals().contacts;

        const currentCustom = document.querySelector<HTMLStyleElement>(
            "style[data-style-origin='user-prefs-custom']"
        );
        if (currentCustom) {
            this.style = currentCustom;
        } else {
            this.style = document.createElement("style");
            document.getElementsByTagName("head")[0].appendChild(this.style);
        }

        this.collectUserContacts();
        this.primaryEmail = this.getContactsFor(Channel.EMAIL).find(
            (c) => c.primary
        )!;
        this.tableData.cbCallBack = this.cbChanged;
    }

    // region Styles tab

    /**
     * Fetches the user's API keys from the server.
     */
    async getKeys() {
        const r = await toPromise(
            this.http.get<IUserApiKey[]>("/translations/apiKeys")
        );
        if (r.ok) {
            this.userAPIKeys = r.result;
        }
    }

    async deleteSelectedStyle(style: StyleDocumentInfo) {
        if (!this.currentStyles) {
            return;
        }

        const styleIndex = this.currentStyles.indexOf(style);
        this.currentStyles.splice(styleIndex, 1);
        this.settings.style_doc_ids = this.currentStyles.map((s) => s.docId);
        this.loadingUserStyle = true;
        this.cdr.detectChanges();
        await this.submit(false);
        this.loadingUserStyle = false;
        this.cdr.detectChanges();
    }

    async onStyleDrop(event: DndDropEvent) {
        if (!this.currentStyles) {
            return;
        }

        const docId = event.data as number;
        const oldIndex = this.currentStyles.findIndex((d) => d.docId == docId);
        let newIndex = event.index ?? 0;
        // Don't move if we're inserting directly above or below current item (i.e. not actually moving)
        if (newIndex == oldIndex || newIndex == oldIndex + 1) {
            return;
        }
        // If we're moving down the list, first splice will move all items after oldIndex down by one index
        if (newIndex > oldIndex) {
            newIndex--;
        }
        const item = this.currentStyles[oldIndex];
        this.currentStyles.splice(oldIndex, 1);
        this.currentStyles.splice(newIndex, 0, item);
        this.settings.style_doc_ids = this.currentStyles.map((s) => s.docId);
        this.loadingUserStyle = true;
        this.cdr.detectChanges();
        await this.submit();
        this.loadingUserStyle = false;
        this.cdr.detectChanges();
    }

    changeStyleTab(index: number) {
        const tab = this.stylesTabs.tabs[index];
        if (tab) {
            tab.active = true;
            this.cdr.detectChanges();
        }
    }

    get styleTable() {
        if (this.timTable.length == 0) {
            return undefined;
        }
        return this.timTable.first;
    }

    activateSelectedStyles(quickStyle: boolean = false) {
        const table = this.timTable.first;

        const currentStyles = new Set(
            quickStyle
                ? this.settings.quick_select_style_doc_ids
                : this.settings.style_doc_ids
        );
        const newStyles = table
            .getCheckedRows(0, true)
            .map((s) => +s[0])
            .filter((d) => !currentStyles.has(d));
        if (quickStyle) {
            this.settings.quick_select_style_doc_ids = [
                ...this.settings.quick_select_style_doc_ids,
                ...newStyles,
            ];
        } else {
            this.settings.style_doc_ids = [
                ...this.settings.style_doc_ids,
                ...newStyles,
            ];
        }
        void this.submit();
    }

    resetSelectedStyles() {
        if (this.settings.style_path) {
            this.currentStyle = this.settings.style_path;
        }
        const tab = this.styleTable;
        if (tab) {
            tab.clearChecked();
        }
    }

    get userContactEntries() {
        return [...this.userContacts.entries()];
    }

    cbChanged = (cbs: boolean[], n: number, index: number) => {
        this.cbCount = n;
        if (n == 0) {
            this.resetSelectedStyles();
            this.cdr.detectChanges();
            return;
        }
        this.saving = true;
        this.loadingUserStyle = true;
        this.cdr.detectChanges();
        (async () => {
            await this.previewSelectedStyles();
            this.saving = false;
            this.loadingUserStyle = false;
            this.cdr.detectChanges();
        })();
    };

    async previewSelectedStyles() {
        this.styleError = undefined;
        const table = this.timTable.first;
        const selStyles = table.getCheckedRows(0, true).map((s) => s[0]);

        const r = await toPromise<string, {error: string}>(
            this.http.get("/styles/path", {
                params: {
                    docs: selStyles.join(","),
                },
                responseType: "text",
            })
        );

        if (r.ok) {
            this.currentStyle = r.result;
        } else {
            this.styleError = {
                title: $localize`Could not preview the style`,
                message: $localize`There was an error loading the style. Please report this to the style's maintainer.`,
            };
        }
    }

    set currentStyle(path: string) {
        replaceStyle("user-prefs-style", path);
    }

    get currentStyle() {
        return document
            .querySelector(
                'link[rel="stylesheet"][data-style-origin="user-prefs-style"]'
            )!
            .getAttribute("href")!
            .substring(1);
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

    private updateShowSelected() {
        this.tableData.hiddenRows = this.showUserStyles ? [] : this.userStyles;
        this.tableData.hiddenColumns = this.showUserStyles
            ? HIDDEN_COLUMNS_OFFICIAL_USERS
            : HIDDEN_COLUMNS_OFFICIAL_ONLY;
    }

    async changeUserStyleVisibility(newVal: boolean) {
        this.resetSelectedStyles();
        this.showUserStyles = newVal;
        this.updateShowSelected();
        const tab = this.styleTable;
        if (tab) {
            await tab.updateFilter();
            tab.c();
        }
    }

    async reloadStyleTable() {
        this.showUserStyles = false;
        this.updateShowSelected();
        // Ensure table is loaded
        await timeout();
        const tab = this.styleTable;
        if (tab) {
            tab.reInitialize();
            tab.filterRow = true;
            tab.c();
        }
    }

    private async updateSelectedStyles() {
        this.styleError = undefined;
        if (this.settings.style_doc_ids.length == 0) {
            this.currentStyles = [];
            this.cdr.detectChanges();
            return;
        }
        const r = await toPromise(
            this.http.get<StyleDocumentInfo[]>("/styles", {
                params: {
                    all: true,
                    docs: this.settings.style_doc_ids.join(","),
                },
            })
        );

        if (r.ok) {
            const allStyleDocs = r.result;
            const userStyles = new Set(this.settings.style_doc_ids);
            const orderMap: Record<number, number> = this.settings.style_doc_ids
                .map((s, i) => ({[s]: i}))
                .reduce((prev, cur) => ({...prev, ...cur}), {});

            for (const styleDoc of allStyleDocs) {
                userStyles.delete(styleDoc.docId);
            }

            for (const missingStyleDoc of userStyles) {
                allStyleDocs.push({
                    docId: missingStyleDoc,
                    description: $localize`This style document was deleted`,
                    path: `${missingStyleDoc}`,
                    name: $localize`Deleted style (id: ${missingStyleDoc})`,
                    type: $localize`Deleted style`,
                });
            }

            for (const styleDoc of allStyleDocs) {
                if (styleDoc.type === undefined) {
                    styleDoc.type = "private";
                }
                if (styleDoc.name === undefined) {
                    styleDoc.name = $localize`Private style`;
                }
                if (styleDoc.description === undefined) {
                    styleDoc.description = $localize`This style is private or has no description`;
                }
            }

            // Reorder styles back in the correct order because ordering matters
            const allStyleDocsOrdered = [];
            for (const style of allStyleDocs) {
                allStyleDocsOrdered[orderMap[style.docId]] = style;
            }

            this.currentStyles = allStyleDocsOrdered;
            this.cdr.detectChanges();
        } else {
            this.styleError = {
                title: $localize`Could not load selected styles`,
                message: r.result.error.error,
            };
            this.cdr.detectChanges();
        }
    }

    async updateAvailableStyles() {
        this.styleError = undefined;
        this.tableData.table = {};
        const r = await toPromise(
            this.http.get<StyleDocumentInfoAll[]>("/styles")
        );
        if (r.ok) {
            this.tableData.table = {
                columns: [{}, {whiteSpace: "nowrap", width: "100%"}, {}, {}],
                rows: [
                    ...r.result.map((s) => ({
                        row: [
                            `${s.docId}`,
                            `<a href="/view/${s.path}">${s.name}</a>`,
                            s.description,
                            s.type,
                        ],
                    })),
                ],
            };

            this.userStyles = r.result
                .map((s, i) => ({
                    i,
                    userStyle: s.path.startsWith("styles/user"),
                }))
                .filter((s) => s.userStyle)
                .map((s) => s.i);

            this.tableData.hiddenRows = this.userStyles;
        } else {
            this.styleError = {
                title: $localize`Could not load available styles`,
                message: r.result.error.error,
            };
        }
    }

    async ngOnInit() {
        polyfill({
            // Use this to make use of the scroll behaviour.
            dragImageTranslateOverride:
                scrollBehaviourDragImageTranslateOverride,
            forceApply: isIOS(),
        });

        window.addEventListener("touchmove", () => {}, {passive: false});

        await this.updateSelectedStyles();
    }

    async ngAfterViewInit() {
        await this.updateAvailableStyles();
    }

    ngDoCheck() {
        this.style.innerHTML = this.settings.custom_css;
    }

    async addPrintSettings() {
        const resp = await toPromise(
            this.http.get("/static/stylesheets/userPrintSettings.css", {
                responseType: "text",
            })
        );
        if (!resp.ok) {
            return;
        }
        this.settings.custom_css += resp.result;
    }

    // endregion

    // region Account and contacts

    /**
     * Open a dialog for user to add additional contact information.
     */
    openContactInfoDialog() {
        void to2(
            showAddContactDialog((contact) => {
                this.updateContacts(contact.channel, (contacts) => [
                    ...contacts,
                    contact,
                ]);
                this.cdr.detectChanges();
            })
        );
    }

    /**
     * Opens a dialog for user to add API keys for translators.
     */
    openAPIKeyDialog() {
        void to2(
            showAddAPIKeyDialog((key) => {
                this.userAPIKeys.push(key);
                this.cdr.detectChanges();
            })
        );
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

    async resendVerification(contact: IUserContact) {
        this.saving = true;
        await toPromise(
            this.http.post("/contacts/add", {
                contact_info_type: contact.channel,
                contact_info: contact.contact,
                resend_if_exists: true,
            })
        );
        this.saving = false;
        this.verificationSentSet.add(contact);
        // TODO: Figure out why this is needed for change detection
        this.cdr.detectChanges();
    }

    async deleteContact(contact: IUserContact) {
        this.saving = true;
        const r = await toPromise(
            this.http.post("/contacts/remove", {
                contact_info_type: contact.channel,
                contact_info: contact.contact,
            })
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

    /**
     * Removes the user's key.
     * @param key the key to be removed
     */
    async deleteKey(key: IUserApiKey) {
        this.saving = true;
        const r = await toPromise(
            this.http.delete("/translations/apiKeys", {
                body: {
                    translator: key.translator,
                    apikey: key.APIkey,
                },
            })
        );
        this.saving = false;
        if (r.ok) {
            this.userAPIKeys.splice(this.userAPIKeys.indexOf(key), 1);
        } else {
            await showMessageDialog(r.result.error.error);
        }
        // TODO: Figure out why this is needed for change detection
        this.cdr.detectChanges();
    }

    /**
     * Checks the quota left for the chosen key and updates it to the progress bar.
     * @param key the key the quota of which is checked
     */
    async checkQuota(key: IUserApiKey) {
        const r = await toPromise(
            this.http.post<ITranslatorUsage>("/translations/apiKeys/quota", {
                translator: key.translator,
                apikey: key.APIkey,
            })
        );
        if (r.ok) {
            key.quotaChecked = true;
            key.availableQuota = r.result.character_limit;
            key.usedQuota = r.result.character_count;

            // TODO: Figure out why this is needed for change detection
            this.cdr.detectChanges();
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    saveUserAccountInfo = async () => {
        this.primaryChangeVerificationSent = false;
        if (this.user.email == this.primaryEmail.contact) {
            return;
        }

        this.saving = true;
        const r = await toPromise(
            this.http.post<{verify: boolean}>("/contacts/primary", {
                contact: this.primaryEmail.contact,
                channel: this.primaryEmail.channel,
            })
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

    // endregion

    // region Notifications

    private groupNotifications(notifications: INotification[]) {
        // Group notifications by the item
        const grouped = new Map<number, GroupedNotification>();
        for (const notification of notifications) {
            const key = notification.item.id;
            if (!grouped.has(key)) {
                grouped.set(key, {
                    item: notification.item,
                    notificationTypes: new Set(),
                });
            }
            grouped.get(key)!.notificationTypes.add(notification.type);
        }
        return Array.from(grouped.values());
    }

    isDocModify(n: GroupedNotification) {
        return (
            n.notificationTypes.has(NotificationType.DocModified) ||
            n.notificationTypes.has(NotificationType.ParAdded) ||
            n.notificationTypes.has(NotificationType.ParDeleted) ||
            n.notificationTypes.has(NotificationType.ParModified)
        );
    }

    isCommentAdd(n: GroupedNotification) {
        return n.notificationTypes.has(NotificationType.CommentAdded);
    }

    isCommentModify(n: GroupedNotification) {
        return (
            n.notificationTypes.has(NotificationType.CommentModified) ||
            n.notificationTypes.has(NotificationType.CommentDeleted)
        );
    }

    isAnswerAdd(n: GroupedNotification) {
        return n.notificationTypes.has(NotificationType.AnswerAdded);
    }

    async getAllNotifications() {
        const resp = await toPromise(
            this.http.get<INotification[]>("/notify/all")
        );
        if (resp.ok) {
            this.notifications = this.groupNotifications(resp.result);
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

    // endregion

    // region Account delete

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
            const r = await toPromise(
                this.http.post("/settings/account/delete", {})
            );
            if (r.ok) {
                location.href = "/";
            } else {
                await showMessageDialog(r.result.error.error);
            }
        }
    }

    // endregion

    submit = async (updateSelectedStylesList: boolean = true) => {
        this.saving = true;
        const cleanSettings = {...this.settings};
        delete cleanSettings.style_path;
        const r = await toPromise(
            this.http.post<SettingsWithStylePath>(
                "/settings/save",
                cleanSettings
            )
        );
        this.saving = false;
        if (r.ok) {
            this.settings = r.result;
            this.resetSelectedStyles();

            if (updateSelectedStylesList) {
                const oldStyles = this.currentStyles?.map((s) => s.docId) ?? [];
                const newStyles = this.settings.style_doc_ids;

                if (
                    oldStyles.length != newStyles.length ||
                    oldStyles.some(
                        (v, i) => v != this.settings.style_doc_ids[i]
                    )
                ) {
                    await this.updateSelectedStyles();
                }
            }
        } else {
            await showMessageDialog(r.result.error.error);
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

    async clearLocalStorage() {
        window.localStorage.clear();
        this.storageClear = true;
        await timeout(3000);
        this.storageClear = false;
    }

    private async setConsent(c: ConsentType) {
        const r = await toPromise(
            this.http.post<IOkResponse>("/settings/updateConsent", {consent: c})
        );
        if (r.ok) {
            // Nothing to do.
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    protected readonly ParMenuHandlePosition = ParMenuHandlePosition;
}

@NgModule({
    declarations: [SettingsComponent, SettingsButtonPanelComponent],
    exports: [SettingsComponent],
    imports: [
        TimTableModule,
        BrowserModule,
        TimUtilityModule,
        HttpClientModule,
        FormsModule,
        DndModule,
        TooltipModule.forRoot(),
        TabsModule.forRoot(),
        ProgressbarModule.forRoot(),
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
