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
       <select id="domain-select" [(ngModel)]="domain">
            <option *ngFor="let domain of domains">{{domain}}</option>
        </select>
    </div>
    <div>
 
    </div>
    <div>
        <input type="checkbox" id="if-archived" [(ngModel)]="archive"/> <label for="if-archived">Archive messages?</label>
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
        <textarea id="add-multiple-emails"></textarea>
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
    // default domain
    domain: string = "@lists.tim.jyu.fi";
    // list is archived by default
    archive: boolean = true;

    domains = ["@lists.tim.jyu.fi", "@timlists.jyu.fi", "@lists.jyu.fi"];

    ngOnInit(): void {
        if (Users.isLoggedIn()) {
        }
        console.log(this.domain);
    }

    constructor(private http: HttpClient) {}

    async newList() {
        // TODO: Validate input values before sending, e.g. this list
        // has a unique name.
        const result = await to2(
            this.http
                .post<JSON>("/messagelist/createlist", {
                    listname: this.listname,
                    domain: this.domain,
                    archive: this.archive,
                    archiveType: "isSecret",
                })
                .toPromise()
        ); // to2()
        if (!result.ok) {
            console.error(result.result.error.error);
        }
    } // newList()
} // class NewMessageListComponent

@NgModule({
    declarations: [NewMessageListComponent],
    exports: [NewMessageListComponent],
    imports: [CommonModule, FormsModule],
})
export class NewMessageListModule {}
