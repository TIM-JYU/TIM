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
        <h1>Create new message list</h1>
        <div>
            <label for="list-name">List name: </label><input type="text" id="list-name" [(ngModel)]="listname"/>
            <select id="domain-select" [(ngModel)]="domain">
                <option *ngFor="let domain of domains">{{domain}}</option>
            </select>
        </div>
        <div>
        </div>
        <div>
            <input type="checkbox" id="if-archived" [(ngModel)]="archive"/> <label for="if-archived">Archive
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
            <textarea id="add-multiple-emails" [(ngModel)]="emails"></textarea>
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
            this.getDomains();
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
}

@NgModule({
    declarations: [NewMessageListComponent],
    exports: [NewMessageListComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMsgListModule {}
