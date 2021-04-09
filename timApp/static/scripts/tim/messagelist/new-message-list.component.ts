import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to2} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {Users} from "../user/userService";

interface CreateListOptions {
    // VIESTIM Keep this updated with ListOptions class (at the Python side of things)
    listname: string;
    domain: string;
    archive: string;
    emails: string[];
    ownerEmail: string;
    notifyOwnerOnListChange: boolean;
    listDescription: string;
    listInfo: string;
}

@Component({
    selector: "tim-new-message-list",
    template: `
        <form name="list-options-form">
            <h1>Create new message list</h1>
            <div>
                <label for="list-name">List name: </label>
                <input type="text" name="list-name" id="list-name"
                       [(ngModel)]="listname"
                       (keyup)="checkNameRequirementsLocally()"/>
                <select id="domain-select" name="domain-select" [(ngModel)]="domain">
                    <option [disabled]="domains.length < 2" *ngFor="let domain of domains">{{domain}}</option>
                </select>
                <!-- VIESTIM: For testing name checking. -->
                <button (click)="checkListNameAvailability()">Tarkasta nimi</button>
            </div>
            <div>
                <!-- VIESTIM: For testing list adding with owner email address. -->
                <label for="owner-address">List owner's adress</label>
                <input type="text" name="owner-address" id="owner-adress" [(ngModel)]="ownerEmail"/>
            </div>
            <div>
                <label for="list-description">Short description</label>
                <input type="text" name="list-description" id="list-description" [(ngModel)]="listDescription"/>
            </div>
            <div>
                <label for="list-info">Long description</label>
                <textarea name="list-info" [(ngModel)]="listInfo">A more detailed information thingy for this list.</textarea>
            </div>
            <div>
            </div>
            <div>
                <input type="checkbox" name="if-archived" id="if-archived" [(ngModel)]="archive"/> <label
                    for="if-archived">Archive
                messages?</label>
            </div>
            <div>
                <p>List archive policy:</p>
                <label *ngFor="let option of archiveOptions">
                    <input
                            name="items-radio"
                            type="radio"
                            [value]="option"
                            [(ngModel)]="archive"
                    />
                    {{ option }}
                </label>
            </div>
            <div>
                <input type="checkbox" name="notify-owner-on-list-change" id="notify-owner-on-list-change"
                       [(ngModel)]="notifyOwnerOnListChange"/>
                <label for="notify-owner-on-list-change">Notify me on list changes (e.g. user subscribes)</label>
            </div>
            <div>
                <label for="add-multiple-emails">Add multiple emails</label> <br/>
                <textarea id="add-multiple-emails" name="add-multiple-emails" [(ngModel)]="emails"></textarea>
            </div>

            <div>
                <select id="search-groups" multiple>
                    <option value="1">Lundberg Tomi</option>
                    <option value="15">ViesTIM</option>
                    <option value="17">ViesTIM-opetus</option>
                    <option value="18">ViesTIM-ohjaajat</option>
                </select>
            </div>
            <button (click)="newList()">Create List</button>
        </form>
    `,
})
export class NewMessageListComponent implements OnInit {
    listname: string = "";

    archiveOptions: string[] = ["none", "private", "public"];
    // List has a private archive by default.
    archive: string = this.archiveOptions[1];

    domain: string = "";
    domains: string[] = [];

    emails?: string;

    urlPrefix: string = "/messagelist";

    ownerEmail: string = "";

    notifyOwnerOnListChange: boolean = false;

    listInfo: string = "";
    listDescription: string = "";

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
            const tempDomains: string[] = result.result;

            for (let i = 0; i < tempDomains.length; i++) {
                tempDomains[i] = "@" + tempDomains[i];
            }
            this.domains = tempDomains;

            // Set default domain.
            this.domain = this.domains[0];
        } else {
            console.error(result.result.error.error);
        }
    }

    constructor(private http: HttpClient) {}

    async newList() {
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
     * Helper to check if this list name exists.
     * VIESTIM: This is a demo function, will only probably need this when we have implemented the creation dialoque?
     */
    async checkListNameAvailability() {
        const nameCandidate: string = this.listname + this.domain; // this.domain, if specified, already contains '@'.
        const result = await to2(
            this.http
                .get<{nameOK: boolean; explanation: string}>(
                    `${this.urlPrefix}/checkname/${nameCandidate}`
                )
                .toPromise()
        );
        if (result.ok) {
            console.log("Name check done. Result:");
            const temp = result.result;
            if (temp.nameOK) {
                // TODO: Indicate somehow that name is usable as a new list name.
                console.log(temp.explanation);
            } else {
                // TODO: Indicate somehow that name is not usable as a new list name.
                console.log(temp.explanation);
            }
        } else {
            console.error(result.result.error.error);
        }
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

        // TODO: Local tests have been passed. Now launch server side checks.
        return true;
    }

    /**
     * Helper for list deletion.
     */
    async deleteList() {
        // const result =
        const result = await to2(
            this.http
                .delete(`/messagelist/deletelist`, {
                    params: {
                        listname: `${this.listname}${this.domain}`,
                    },
                })
                .toPromise()
        );
        if (result.ok) {
            // TODO: Inform the user deletion was succesfull.
            console.log(result.result);
        } else {
            // TODO: Inform the user deletion was not succesfull.
            console.log(result.result);
        }
    }
}

@NgModule({
    declarations: [NewMessageListComponent],
    exports: [NewMessageListComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMsgListModule {}
