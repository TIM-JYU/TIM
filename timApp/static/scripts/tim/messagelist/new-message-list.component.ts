import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to2} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {Users} from "../user/userService";

interface ListOptions {
    // VIESTIM Keep this updated with ListOptions class (at the Python side of things)
    listname: string;
    domain: string;
    archive: ArchiveType;
    emails: string[];
    ownerEmail: string;
    notifyOwnerOnListChange: boolean;
    listDescription: string;
    listInfo: string;
    defaultReplyType: ReplyToListChanges;
    htmlAllowed: boolean;
}

enum ArchiveType {
    // See ArchiveType class on Python side of things for explanations.
    NONE,
    SECRET,
    GROUPONLY,
    UNLISTED,
    PUBLIC,
}

enum ReplyToListChanges {
    NOCHANGES,
    ADDLIST,
}

// For proper setting of archive options on UI.
interface ArchivePolicyDescriptions {
    archiveType: ArchiveType;
    policyName: string;
}

interface ReplyToDescriptions {
    replyType: ReplyToListChanges;
    explanation: string;
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
                <textarea name="list-info"
                          [(ngModel)]="listInfo">A more detailed information thingy for this list.</textarea>
            </div>
            <div>
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
                        <label for="archive-{{option.archiveType}}">{{option.policyName}}</label>
                    </li>
                </ul>
            </div>
            <div>
                <input type="checkbox" name="notify-owner-on-list-change" id="notify-owner-on-list-change"
                       [(ngModel)]="notifyOwnerOnListChange"/>
                <label for="notify-owner-on-list-change">Notify me on list changes (e.g. user subscribes)</label>
            </div>
            <div>
                <b>List answer default:</b>
                <ul style="list-style-type: none">
                    <li *ngFor="let replyType of replyToDescriptions">
                        <input
                                name="list-reply-type"
                                type="radio"
                                id="reply-type-{{replyType.replyType}}"
                                [value]="replyType.replyType"
                                [(ngModel)]="replyToOption"
                        />
                        <label for="reply-type-{{replyType.replyType}}">{{replyType.explanation}}</label>
                    </li>
                </ul>
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

    // archiveOptions: string[] = ["none", "private", "public"];
    archiveOptions: ArchivePolicyDescriptions[] = [
        {archiveType: ArchiveType.NONE, policyName: "No archiving."},
        {
            archiveType: ArchiveType.SECRET,
            policyName: "Secret archive, only for owner.",
        },
        {
            archiveType: ArchiveType.GROUPONLY,
            policyName:
                "Members only archive. Only members of this list can access.",
        },
        {
            archiveType: ArchiveType.UNLISTED,
            policyName: "Unlisted archive. Everyone with link can access.",
        },
        {
            archiveType: ArchiveType.PUBLIC,
            policyName:
                "Public archive. Everyone with link can access and the archive is advertised.",
        },
    ];
    // List has a private members only archive by default.
    archive: ArchiveType = ArchiveType.GROUPONLY;

    domain: string = "";
    domains: string[] = [];

    emails?: string;

    urlPrefix: string = "/messagelist";

    ownerEmail: string = "";

    notifyOwnerOnListChange: boolean = false;

    listInfo: string = "";
    listDescription: string = "";

    // For timed server name check id's, see window.setTimeout.
    timeoutID?: number;

    replyToDescriptions: ReplyToDescriptions[] = [
        {
            replyType: ReplyToListChanges.NOCHANGES,
            explanation: "No changes",
        },
        {
            replyType: ReplyToListChanges.ADDLIST,
            explanation: "Add list as reply-to address.",
        },
    ];
    replyToOption: ReplyToListChanges = ReplyToListChanges.NOCHANGES;

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
            // VIESTIM These fields have to match with interface ListOptions, otherwise a type error happens.
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
            defaultReplyType: this.replyToOption,
            htmlAllowed: false,
        });
        if (!result.ok) {
            console.error(result.result.error.error);
        } else {
            // VIESTIM Helps see that data was sent succesfully after clicking the button.
            console.log("List options sent successfully.");
        }
    }

    // VIESTIM this helper function helps keeping types in check.
    private createList(options: ListOptions) {
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
