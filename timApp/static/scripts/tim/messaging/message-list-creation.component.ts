import {BrowserModule} from "@angular/platform-browser";
import {Component, NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {HttpClient} from "@angular/common/http";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {to2} from "../util/utils";
import {Users} from "../user/userService";
import {archivePolicyNames, ArchiveType} from "./listOptionTypes";
import {showInputDialog} from "../ui/showInputDialog";
import {InputDialogKind} from "../ui/input-dialog.kind";
import {IDocument, redirectToItem} from "../item/IItem";

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
                <button class="timButton" type="button" (click)="ok()"></button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class MessageListComponent extends AngularDialogComponent<
    string,
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

    // This was added 13.4.
    async ok() {
        const doc = await showInputDialog({
            isInput: InputDialogKind.InputAndValidator,
            defaultValue: "",
            text: "Enter name of the message list",
            title: "Message list creation",
            validator: async (s) => {
                const r = await to2(
                    this.http.get<IDocument>(`/groups/create/${s}`).toPromise()
                );
                if (r.ok) {
                    return {ok: true, result: r.result};
                } else {
                    return {ok: false, result: r.result.error.error};
                }
            },
        });
        redirectToItem(doc);
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
}

@NgModule({
    declarations: [MessageListComponent],
    imports: [BrowserModule, DialogModule, FormsModule],
})
export class MessageListModule {}
