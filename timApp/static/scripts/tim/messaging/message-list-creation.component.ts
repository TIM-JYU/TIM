import {BrowserModule} from "@angular/platform-browser";
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Users} from "../user/userService";
import {
    archivePolicyNames,
    ArchiveType,
    CreateListOptions,
} from "./listOptionTypes";

@Component({
    selector: "message-list-creation",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Message list creation
            </ng-container>
            <ng-container body>
                <div>
                    <label for="list-name">List name: </label>
                    <input type="text" name="list-name" id="list-name"
                           [(ngModel)]="listname"
                           (keyup)="checkNameRequirementsLocally()"/>
                    <span>@</span>
                    <select id="domain-select" name="domain-select" [(ngModel)]="domain">
                        <option [disabled]="domains.length" *ngFor="let domain of domains">{{domain}}</option>
                    </select>
                </div>
                <div>
                    <b>List archive policy:</b>
                    <ul style="list-style-type: none">
                        <li *ngFor="let option of archiveOptions">
                            <input
                                    name="items-radio"
                                    type="radio"
                                    id="archive-{{option.archiveType}}"
                                    [value]="option.archiveType"
                                    [(ngModel)]="archive"
                            />
                            <label for="archive-{{option}}">{{option.policyName}}</label>
                        </li>
                    </ul>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" type="button" (click)="newList() ">Create</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MessageListComponent extends AngularDialogComponent<
    unknown,
    unknown
> {
    protected dialogName = "MessageList";
    listname: string = "";

    urlPrefix: string = "/messagelist";

    domains: string[] = [];
    domain: string = "";

    // List has a private members only archive by default.
    archive: ArchiveType = ArchiveType.GROUPONLY;
    archiveOptions = archivePolicyNames;

    // For name check
    timeoutID?: number;
    ownerEmail: string = "totalund@student.jyu.fi";
    notifyOwnerOnListChange: boolean = true;

    listDescription: string = "";
    listInfo: string = "";

    emails?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
            void this.getDomains();
        }
    }

    private async getDomains() {
        const result = await to2(
            this.http.get<string[]>(`${this.urlPrefix}/domains`).toPromise()
        );
        if (result.ok) {
            // Add '@' in front of domain names for display purposes.
            // const tempDomains: string[] = result.result;

            // for (let i = 0; i < tempDomains.length; i++) {
            //    tempDomains[i] = "@" + tempDomains[i];
            // }
            this.domains = result.result;

            // Set default domain.
            this.domain = this.domains[0];
        } else {
            console.error(result.result.error.error);
        }
    }

    async newList() {
        // Somanyduplicate
        const result = await this.createList({
            // VIESTIM These fields have to match with interface CreateListOptions, otherwise a type error happens.
            // TODO: Validate input values before sending, e.g. this list has a unique name.
            listname: this.listname,
            // We added '@' in domain name for display purposes, remove it when sending domain to the server.
            domain: this.domain.startsWith("@")
                ? this.domain.slice(1)
                : this.domain,
            archive: this.archive,
            emails: this.parseEmails(),
            ownerEmail: this.ownerEmail,
            notifyOwnerOnListChange: this.notifyOwnerOnListChange,
            listInfo: this.listInfo,
            listDescription: this.listDescription,
        });
        if (!result.ok) {
            console.error(result.result.error.error);
        } else {
            // VIESTIM Helps see that data was sent succesfully after clicking the button.
            console.log("List options sent successfully.");
        }
    }

    // VIESTIM this helper function helps keeping types in check.
    private createList(options: CreateListOptions) {
        return to2(
            this.http.post("/messagelist/createlist", {options}).toPromise()
        );
    }

    /**
     * Compile email addresses separated by line breaks into a list
     * @private
     */
    private parseEmails(): string[] {
        if (!this.emails) {
            return [];
        }
        return this.emails.split("\n").filter((e) => e);
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
        // VIESTIM: Since the server has the final say for allowed names, sync these rules with the server. Maybe they
        //  could be imported from the server?
        // TODO: Replace console.logs with a better feedback system for the user.
        console.log(`start check on listname: ${this.listname}`);

        // Cancel previous timed call to server name checks.
        if (this.timeoutID) {
            clearTimeout(this.timeoutID);
        }
        this.timeoutID = undefined;

        // Name length is within length boundaries.
        if (this.listname.length < 5 || 36 < this.listname.length) {
            console.log("Name not in length boundaries");
            return false;
        }

        // Name starts with a character that is a letter a - z.
        // Notice that ^ serves two different purposes in the following regular expression.
        // The first one checks at the beginning of the string, the second is a negation.
        const regExpStartCharacter: RegExp = /^[a-z]/;
        if (!regExpStartCharacter.test(this.listname)) {
            console.log("name doesn't start with a lowercase letter");
            return false;
        }

        // Name contains at least one digit.
        const regExpAtLeastOneDigit: RegExp = /\d/;
        if (!regExpAtLeastOneDigit.test(this.listname)) {
            console.error("name doesn't contain at least one digit.");
            return false;
        }

        // Name can't contain multiple sequential dots.
        const regExpMultipleDots: RegExp = /\.\.+/;
        if (regExpMultipleDots.test(this.listname)) {
            console.log("name contains multiple dots");
            return false;
        }

        // Name doesn't end in a dot.
        // ESLint prefers to not use regex for this. And by "prefer" we mean this won't transpile with a regular
        // expression.
        if (this.listname.endsWith(".")) {
            console.log("name ends in a dot");
            return false;
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
            console.log("Name had forbidden characters");
            return false;
        }
        console.log("name has passed all local tests");
        console.log(
            "start server side tests in 5 seconds after last key down."
        );
        // Local tests have been passed. Now launch server side checks.
        this.timeoutID = window.setTimeout(
            () => this.checkListNameAvailability(),
            5 * 1000
        );

        return true;
    }

    /**
     * Helper to check if this list name exists.
     * VIESTIM: This is a demo function, will only probably need this when we have implemented the creation dialoque?
     */
    async checkListNameAvailability() {
        // Name candidate depends on whether domains are configured for TIM.
        const nameCandidate: string = this.domain
            ? `${this.listname}@${this.domain}`
            : this.listname;

        const result = await to2(
            this.http
                .get(`${this.urlPrefix}/checkname/${nameCandidate}`)
                .toPromise()
        );
        if (result.ok) {
            // VIESTIM: we need a better indication that the name is available to the user.
            console.log("Name check done. Name is available.");
        } else {
            // VIESTIM: We need a better indication that the name is not available to the user.
            console.error(result.result.error.error);
        }
    }
}

@NgModule({
    declarations: [MessageListComponent],
    imports: [BrowserModule, DialogModule, FormsModule],
})
export class MessageListModule {}
