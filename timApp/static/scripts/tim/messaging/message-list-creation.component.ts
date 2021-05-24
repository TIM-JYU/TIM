import {BrowserModule} from "@angular/platform-browser";
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Users} from "../user/userService";
import {IDocument, redirectToItem} from "../item/IItem";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {archivePolicyNames, ArchiveType, ListOptions} from "./listOptionTypes";

@Component({
    selector: "message-list-creation",
    template: `
        <tim-dialog-frame class="form-horizontal">
            <ng-container header>
                Message list creation
            </ng-container>
            <ng-container body>
                <div *ngIf="errorMessage.length > 0" class="alert alert-danger">
                    <ul>
                        <li *ngFor="let error of errorMessage">{{error}}</li>
                    </ul>
                </div>
                <div class="form-group">
                    <label for="list-name" class="list-name text-left control-label col-sm-3">List name: </label>
                    <div class="col-sm-8">
                        <div class="input-group">
                            <input type="text" class="form-control" name="list-name" id="list-name"
                                   [(ngModel)]="listname"
                                   (keyup)="checkNameRequirementsLocally()"/>
                            <div class="input-group-addon">@</div>
                            <select id="domain-select" class="form-control" name="domain-select" [(ngModel)]="domain">
                                <option [disabled]="domains.length" *ngFor="let domain of domains">{{domain}}</option>
                            </select>
                        </div>
                    </div>
                </div>
                <div class="archive-options">
                    <p class="list-name">List archive policy: </p>
                    <ul class="archive-list">
                        <li *ngFor="let option of archiveOptions">
                            <label class="radio" for="archive-{{option.archiveType}}">
                            <input
                                    name="items-radio"
                                    type="radio"
                                    id="archive-{{option.archiveType}}"
                                    [value]="option.archiveType"
                                    [(ngModel)]="archive"
                            />
                            {{option.policyName}}
                            </label>
                        </li>
                    </ul>
                </div>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="disableCreate"></tim-loading>
                <button [disabled]="disableCreate" class="timButton" type="button" (click)="newList() ">Create</button>
            </ng-container>
        </tim-dialog-frame>
    `,
    styleUrls: ["message-list-creation.component.scss"],
})
export class MessageListComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    disableCreate: boolean = false;
    protected dialogName = "MessageList";
    listname: string = "";
    errorMessage: string[] = [];

    urlPrefix: string = "/messagelist";

    domains: string[] = [];
    domain: string = "";

    // List has a public archive by default.
    archive: ArchiveType = ArchiveType.PUBLIC;
    archiveOptions = archivePolicyNames;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
            void this.getDomains();
        }
    }

    /**
     * Fetch possible domains to be used for email lists.
     * @private
     */
    private async getDomains() {
        const result = await to2(
            this.http.get<string[]>(`${this.urlPrefix}/domains`).toPromise()
        );
        if (result.ok) {
            this.domains = result.result;

            // Set default domain.
            this.domain = this.domains[0];
        } else {
            this.errorMessage = [
                `Failed to load domains, list creation can't continue. The following error provides details: ${result.result.error.error}`,
            ];
            // Creating a message list isn't possible at this time if domains are not given. Therefore disable creation
            // button.
            this.disableCreate = true;
        }
    }

    /**
     * Launching the creation of a new list. Verifies the basic name rules in the client before involving the server.
     */
    async newList() {
        if (!this.checkNameRequirementsLocally()) {
            return;
        }

        this.errorMessage = [];
        this.disableCreate = true;
        const result = await this.createList({
            name: this.listname,
            domain: this.domain,
            archive: this.archive,
        });
        if (!result.ok) {
            this.errorMessage = [result.result.error.error];
            this.disableCreate = false;
        } else {
            redirectToItem(result.result);
        }
    }

    /**
     * The call to create new list.
     * @param options Required amount of options to create a new message list. Here the necessary arguments are list's
     * name and archive policy.
     * @private
     */
    private createList(options: ListOptions) {
        return to2(
            this.http
                .post<IDocument>(`${this.urlPrefix}/createlist`, {options})
                .toPromise()
        );
    }

    /**
     * Check list name requirements locally.
     *
     * If you make changes here, make sure to check that the server checks the same things. Otherwise there will
     * inconsistant name checking and a confused user.
     *
     * @returns {boolean} Returns true if name requirements are met. Otherwise returns false.
     */
    checkNameRequirementsLocally(): boolean {
        // Clear old error messages.
        this.errorMessage = [];

        // Name length is within length boundaries.
        if (this.listname.length <= 5 || 36 <= this.listname.length) {
            this.errorMessage.push("Name not in length boundaries");
        }

        // Name starts with a character that is a letter a - z.
        const regExpStartCharacter: RegExp = /^[a-z]/;
        if (!regExpStartCharacter.test(this.listname)) {
            this.errorMessage.push("Name should start with a lowercase letter");
        }

        // Name contains at least one digit.
        const regExpAtLeastOneDigit: RegExp = /\d/;
        if (!regExpAtLeastOneDigit.test(this.listname)) {
            this.errorMessage.push("Name should contain at least one digit");
        }

        // Name can't contain sequential dots.
        const regExpMultipleDots: RegExp = /\.\.+/;
        if (regExpMultipleDots.test(this.listname)) {
            this.errorMessage.push("Name shouldn´t contain multiple dots");
        }

        // Name doesn't end in a dot.
        // ESLint prefers to not use regex for this. And by "prefer" we mean this won't transpile with a regular
        // expression.
        if (this.listname.endsWith(".")) {
            this.errorMessage.push("Name shouldn´t end in a dot");
        }

        // Name contains only acceptable characters, which are:
        //     letters                  a - z
        //     numbers                  0 - 9
        //     dot                      '.'
        //     underscore               '_'
        //     hyphen (or "minus sign") '-'
        // The following regular expression searches for characters that are *not* one of the above. If those are not
        // found the name is of correct form. Notice that hyphen is in two different roles and one hyphen has
        // to be escaped. The dot does not have to be escaped here.
        const regExpNonAllowedCharacters: RegExp = /[^a-z0-9.\-_]/;
        if (regExpNonAllowedCharacters.test(this.listname)) {
            this.errorMessage.push("Name has forbidden characters");
        }
        return this.errorMessage.length == 0;
    }
}

@NgModule({
    declarations: [MessageListComponent],
    imports: [BrowserModule, DialogModule, FormsModule, TimUtilityModule],
})
export class MessageListModule {}
