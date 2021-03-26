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
    archive: boolean;
    archiveType: string;
    emails: string[];
}

@Component({
    selector: "tim-new-message-list",
    template: `
        <form>
            <h1>Create new message list</h1>
            <div>
                <label for="list-name">List name: </label><input type="text" name="list-name" id="list-name"
                                                                 [(ngModel)]="listname"/>
                <select id="domain-select" name="domain-select" [(ngModel)]="domain">
                    <option *ngFor="let domain of domains">{{domain}}</option>
                </select>
                <!-- For testing name checking -->
                <button (click)="checkEmailListNameAvailability()">Tarkasta nimi</button>
            </div>
            <div>
            </div>
            <div>
                <input type="checkbox" name="if-archived" id="if-archived" [(ngModel)]="archive"/> <label
                    for="if-archived">Archive
                messages?</label>
            </div>
            <div>
                <p>Radio buttons example</p>
                <p>Currently selected item: {{ archiveType }}</p>
                <label *ngFor="let item of items">
                    <input
                            name="items-radio"
                            type="radio"
                            [value]="item"
                            [(ngModel)]="archiveType"
                    />
                    {{ item }}
                </label>
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

    // list is archived by default
    archive: boolean = true;
    archiveType: string = "";
    items: string[] = ["public archive", "secret archive"];

    domain: string = "";
    domains: string[] = [];

    emails?: string;

    urlPrefix: string = "/messagelist";

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
            this.domains = result.result;
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
            domain: this.domain,
            archive: this.archive,
            emails: this.parseEmails(),
            archiveType: this.archiveType,
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
        return this.emails.split("\n").filter(Boolean);
    }

    /**
     * Helper to check if this list name exists.
     * VIESTIM: This is a demo function, will only probably need this when we have implemented the creation dialoque?
     */
    async checkEmailListNameAvailability() {
        const nameCandidate: string = this.listname + this.domain;
        const result = await to2(
            this.http
                .get<boolean>(`${this.urlPrefix}/checkname/${nameCandidate}`)
                .toPromise()
        );
        if (result.ok) {
            console.log("Hei maailma, tarkistus on tehty. Tulos on:");
            if (result.result) {
                console.log("nimi on vapaa käyttöön.");
            } else {
                console.log(
                    "nimi on muussa käytössä (tai tarkistusta ei voitu tehdä)."
                );
            }
        } else {
            console.error(result.result.error.error);
        }
    }

    /**
     * Check list name requirements locally.
     * TODO: Hook this into a text field to check at updates.
     *
     * If you make changes here, make sure to check that the server checks the same things. Otherwise there will
     * inconsistant name checking and a confused user.
     *
     * TODO: Expand to return information if returns false, to inform the user why name requirements aren't met.
     * @returns {boolean} Returns true if name requirements are met. Otherwise returns false.
     */
    checkNameRequirements(): boolean {
        // Name length is within length boundaries.
        if (this.listname.length < 5 || 36 < this.listname.length) {
            return false;
        }

        // Name starts with a character that is a letter a - z.
        // Notice that ^ serves two different purposes in the following regular expression.
        // The first one checks at the beginning of the string, the second is a negation.
        const regExpStartCharacter: RegExp = /^[^a-z]/;
        if (regExpStartCharacter.test(this.listname)) {
            return false;
        }

        // Name contains only acceptable characters, which are:
        //     letters                  a - z
        //     numbers                  0 - 9
        //     dot                      '.'
        //     underscore               '_'
        //     hyphen (or "minus sign") '-'
        // The following regular expression searches for characters that are not one of the above. If those are not
        // found the name is of correct form. Notice that hyphen is in two different roles and one hyphen has
        // to be escaped. The dot does not have to be escaped here.
        const regExpNonAllowedCharacters: RegExp = /[^a-z0-9.\-_]/;
        return !regExpNonAllowedCharacters.test(this.listname);
    }
}

@NgModule({
    declarations: [NewMessageListComponent],
    exports: [NewMessageListComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMsgListModule {}
