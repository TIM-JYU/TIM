// eslint-disable-next-line @typescript-eslint/no-unused-vars
import {HttpClient} from "@angular/common/http";
import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";
import {to2} from "tim/util/utils";
import {FormsModule} from "@angular/forms";
import {Users} from "../user/userService";

@Component({
    selector: "tim-new-message-list",
    template: `
    <h1>Create new message list</h1>
    <div>
        <label for="list-name">List name: </label><input type="text" id="list-name" [(ngModel)]="listname"/>
       <select id="domain-select">
            <option value="lists.tim.jyu.fi">@lists.jyu.fi</option>
            <option value="timlists.jyu.fi">@timlists.jyu.fi</option>
            <option value="lists.jyu.fi">@lists.jyu.fi</option>
        </select>
    </div>
    <div>
 
    </div>
    <div>
        <input type="checkbox" id="if-archived" /> <label for="if-archived">Archive messages?</label>
    </div>
    <div>
        <input type="radio" id="public-archive" name="archive-type" value="isPublic" />
        <label for="public-archive">Public archive</label>
    </div>
    <div>
        <input type="radio" id="secret-archive" name="archive-type" value="isSecret" />
        <label for="secret-archive"> "Super secret archive"</label>
    </div>
    <div>
        <label for="add-multiple-emails">Add multiple emails</label> <br />
        <textarea id="add-multiple-emails" [(ngModel)]="emails"></textarea>
    </div>

    <div>
        <select id="search-groups" multiple>
            <option value="1">Lundberg Tomi</option>
            <option value="15">ViesTIM </option>
            <option value="17">ViesTIM-opetus</option>
            <option value="18">ViesTIM-ohjaajat</option>
        </select>
    </div>
        <button (click)="newList()">Create List</button>
        
    `,
})
export class NewMessageListComponent implements OnInit {
    listname?: string;
    emails?: string;

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
        }
    }

    constructor(private http: HttpClient) {}

    async newList() {
        // TODO: Validate input values before sending, e.g. this list
        // has a unique name.
        const result = await to2(
            this.http
                .post<JSON>("/messagelist/createlist", {
                    listname: this.listname,
                    archiveType: "isSecret",
                    emails: this.parseEmails(),
                })
                .toPromise()
        ); // to2()
        if (!result.ok) {
            console.error(result.result.error.error);
        }
    } // newList()

    /**
     * Compile email addresses separated by line breaks into a list
     * @private
     */
    private parseEmails(): string[] {
        if (this.emails == null) {
            return [];
        }

        return this.emails.split("\n").filter(Boolean);
    }
} // class NewMessageListComponent

@NgModule({
    declarations: [NewMessageListComponent],
    exports: [NewMessageListComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMessageListModule {}
