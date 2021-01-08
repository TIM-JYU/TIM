import {HttpClient} from "@angular/common/http";
import {Component, DoCheck, Input} from "@angular/core";
import {DocumentOrFolder} from "tim/item/IItem";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {ConsentType} from "../ui/consent";
import {settingsglobals} from "../util/globals";
import {IOkResponse, timeout, to2} from "../util/utils";
import {IFullUser} from "./IUser";

export interface ISettings {
    css_combined: string;
    css_files: Record<string, boolean>;
    custom_css: string;
    disable_menu_hover: boolean;
    remember_last_sidebar_menu_tab: boolean;
    remember_last_sidebar_menu_state: boolean;
    email_exclude: string;
    language: string | null;
    use_document_word_list: boolean;
    word_list: string;
    auto_mark_all_read: boolean;
}

export async function setConsent(http: HttpClient, c: ConsentType) {
    const r = await to2(
        http
            .post<IOkResponse>("/settings/updateConsent", {consent: c})
            .toPromise()
    );
    if (r.ok) {
        // Nothing to do.
    } else {
        void showMessageDialog(r.result.error.error);
    }
}

export interface INotification {
    email_comment_add: boolean;
    email_comment_modify: boolean;
    email_doc_modify: boolean;
    item: DocumentOrFolder;
}

export interface ICssFile {
    name: string;
    desc: string;
}

@Component({
    selector: "tim-save-button",
    template: `
        <button class="timButton" [disabled]="saving" (click)="save()">Save changes</button>
        <tim-loading *ngIf="saving"></tim-loading>
    `,
})
export class SaveButtonComponent {
    saving = false;
    @Input() saved?: () => Promise<unknown>;

    async save() {
        this.saving = true;
        await this.saved?.();
        this.saving = false;
    }
}

@Component({
    selector: "tim-settings",
    template: `
        <h1>TIM settings</h1>
        <div class="form">
            <bootstrap-panel title="Preferred language">
                <div class="flex cl">
                    <tim-language-selector></tim-language-selector>
                    <tim-save-button [saved]="submit"></tim-save-button>
                </div>
            </bootstrap-panel>
            <bootstrap-panel title="Styles">
                <span *ngIf="cssFiles">Available themes:</span>
                <span *ngIf="!cssFiles">There are no available themes.</span>
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
                    <label for="customCssArea">Custom CSS:</label>
                    <textarea rows="15" id="customCssArea" class="form-control"
                              [(ngModel)]="settings.custom_css"></textarea>
                </div>
                <tim-save-button [saved]="submit"></tim-save-button>
                <button class="btn btn-default" (click)="addPrintSettings()">Add Print Settings</button>

            </bootstrap-panel>
            <bootstrap-panel title="Editor">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" [(ngModel)]="settings.use_document_word_list">
                        Use words from the document in ACE editor autocomplete
                    </label>
                </div>
                <label>ACE editor additional word list for autocomplete (1 word per line)
                    <textarea rows="15" class="form-control" [(ngModel)]="settings.word_list"></textarea>
                </label>
                <div>
                    <tim-save-button [saved]="submit"></tim-save-button>
                </div>
            </bootstrap-panel>
            <bootstrap-panel title="Notifications">
                <h4>Subscribed items</h4>
                <p *ngIf="notifications.length > 0">You get emails from the following documents and folders:</p>
                <p *ngIf="notifications.length === 0">You haven't subscribed to any documents or folders.</p>
                <ul>
                    <li *ngFor="let n of notifications">
                        <a href="/manage/{{n.item.path}}">
                            <span *ngIf="n.item.isFolder" class="glyphicon glyphicon-folder-open"></span>
                            {{n.item.title}}</a>
                        <span *ngIf="n.email_doc_modify"
                              class="glyphicon glyphicon-pencil"
                              tooltip="Document modifications"></span>
                        <span *ngIf="n.email_comment_add"
                              class="glyphicon glyphicon-comment"
                              tooltip="New comments"></span>
                        <span *ngIf="n.email_comment_modify"
                              class="glyphicon glyphicon-comment"
                              tooltip="Comment modifications"></span>
                    </li>
                </ul>
                <button (click)="getAllNotifications()"
                        class="timButton"
                        *ngIf="showGetAllNotifications()">
                    Show all
                </button>
                <h4>Exclusion list</h4>
                <p>
                    Sometimes you may want to subscribe to emails from a folder but exclude some documents within it.
                    Using the list below you can specify which folders and documents should be excluded from your email
                    subscriptions.
                </p>
                <p>Type one regular expression per line that should match any part of the path of the folder or
                    document,
                    e.g. <code>/ht/</code> would match any path with <code>/ht/</code> in it.</p>
                <div class="form-group">
        <textarea class="form-control" rows="5" [(ngModel)]="settings.email_exclude">
        </textarea>
                </div>

                <div>
                    <tim-save-button [saved]="submit"></tim-save-button>
                </div>

            </bootstrap-panel>

            <bootstrap-panel title="Menus">
                <div class="checkbox"><label>
                    <input type="checkbox" name="disable_menu_hover" [(ngModel)]="settings.disable_menu_hover"
                           [disabled]="saving"> Disable opening menus with mouse hover
                </label></div>
                <div class="checkbox"><label>
                    <input type="checkbox" name="remember_last_sidebar_menu_tab" [(ngModel)]="settings.remember_last_sidebar_menu_tab"
                           [disabled]="saving"> Side bar menu: remember the last selected tab
                </label></div>
                <div class="checkbox"><label>
                    <input type="checkbox" name="remember_last_sidebar_menu_state" [(ngModel)]="settings.remember_last_sidebar_menu_state"
                           [disabled]="saving"> Side bar menu: remember the last open state
                </label></div>
                <tim-save-button [saved]="submit"></tim-save-button>
            </bootstrap-panel>

            <bootstrap-panel title="Other settings">
                <div class="checkbox" id="othersettings"><label>
                    <input type="checkbox" name="auto_mark_all_read" [(ngModel)]="settings.auto_mark_all_read"
                           [disabled]="saving"> Automatically mark document as read when opening it for the first time
                </label></div>
                <span class="space-right"><tim-save-button [saved]="submit"></tim-save-button></span>
                <button class="btn btn-default" (click)="clearLocalStorage()">Clear local settings storage</button>
                <span *ngIf="storageClear">Local storage cleared.</span>
            </bootstrap-panel>
            <bootstrap-panel title="Your account information">
                <p>Id: {{user.id}}</p>
                <p>Username: {{user.name}}</p>
                <p>Full name: {{user.real_name}}</p>
                <p>Email: {{user.email}}</p>
            </bootstrap-panel>
            <bootstrap-panel title="Delete your account">
                <button class="timButton btn-danger"
                        (click)="beginDeleteAccount()"
                        *ngIf="!deletingAccount">Delete account...
                </button>
                <div *ngIf="deletingAccount">
                    To delete your account, please type your username ({{ user.name }}) in the field below
                    and then click "Delete account now".
                    <div class="form-inline">
                        <input class="form-control"
                               type="text"
                               [(ngModel)]="deleteConfirmName">
                        <button [disabled]="user.name !== deleteConfirmName"
                                class="timButton btn-danger"
                                (click)="deleteAccount()">Delete account now
                        </button>
                        <button class="timButton btn-default" (click)="deletingAccount = false">Cancel</button>
                    </div>
                </div>
            </bootstrap-panel>
        </div>
    `,
})
export class SettingsComponent implements DoCheck {
    saving = false;
    private style: HTMLStyleElement;
    settings: ISettings;
    cssFiles: Array<ICssFile>;
    notifications: INotification[];
    private consent: ConsentType | undefined;
    storageClear = false;
    private allNotificationsFetched = false;
    user: IFullUser;
    deletingAccount = false;
    deleteConfirmName = "";

    constructor(private http: HttpClient) {
        this.user = settingsglobals().current_user;
        this.consent = this.user.consent;
        this.settings = settingsglobals().userPrefs;
        this.cssFiles = settingsglobals().css_files;
        this.notifications = settingsglobals().notifications;
        this.updateCss();
        this.style = document.createElement("style");
        this.style.type = "text/css";
        document.getElementsByTagName("head")[0].appendChild(this.style);
    }

    ngDoCheck() {
        this.style.innerHTML = this.settings.custom_css;
    }

    submit = async () => {
        this.saving = true;
        const r = await to2(
            this.http
                .post<ISettings>("/settings/save", this.settings)
                .toPromise()
        );
        if (r.ok) {
            this.settings = r.result;
            this.updateCss();
        } else {
            void showMessageDialog(r.result.error.error);
        }
        this.saving = false;
    };

    async updateConsent() {
        if (this.consent == null) {
            return;
        }
        this.saving = true;
        await setConsent(this.http, this.consent);
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
                .get<string>("/static/stylesheets/userPrintSettings.css")
                .toPromise()
        );
        if (!resp.ok) {
            return;
        }
        this.settings.custom_css = resp.result;
    }

    async getAllNotifications() {
        const resp = await to2(
            this.http.get<INotification[]>("/notify/all").toPromise()
        );
        if (resp.ok) {
            this.notifications = resp.result;
            this.allNotificationsFetched = true;
        } else {
            void showMessageDialog(resp.result.error.error);
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
                `Are you sure you want to delete your account (${this.user.name})?`
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
}
