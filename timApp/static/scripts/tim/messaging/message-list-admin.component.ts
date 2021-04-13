import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to2} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {archivePolicyNames, ArchiveType} from "tim/messaging/listOptionTypes";
import {Users} from "../user/userService";

interface CreateListOptions {
    // VIESTIM Keep this updated with ListOptions class (at the Python side of things)
    listname: string;
    domain: string;
    archive: ArchiveType;
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
            <h1>Message list management</h1>
            <div>
                <label for="list-name">List name: </label>
                <input type="text" name="list-name" id="list-name"
                       [(ngModel)]="listname"/><span>@</span>
                <select id="domain-select" name="domain-select" [(ngModel)]="domain">
                    <option [disabled]="domains.length < 2" *ngFor="let domain of domains">{{domain}}</option>
                </select>
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
                        <label for="archive-{{option}}">{{option.policyName}}</label>
                    </li>
                </ul>
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
export class MessageListAdminComponent implements OnInit {
    listname: string = "";

    // List has a private members only archive by default.
    archive: ArchiveType = ArchiveType.GROUPONLY;

    domain: string = "";
    domains: string[] = [];

    emails?: string;

    urlPrefix: string = "/messagelist";

    ownerEmail: string = "";

    archiveOptions = archivePolicyNames;

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
            this.domains = result.result;
            if (!this.domains.length) {
                this.domain = this.domains[0];
            }
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
    declarations: [MessageListAdminComponent],
    exports: [MessageListAdminComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMsgListModule {}
