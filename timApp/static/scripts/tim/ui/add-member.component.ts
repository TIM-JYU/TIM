import {Component, Input} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {to2} from "tim/util/utils";

interface IAddmemberResponse {
    added: string[];
    already_belongs: string[];
    not_exist: string[];
}

@Component({
    selector: "tim-add-member",
    template: `
        <tim-plugin-frame [markupError]="undefined">
            <tim-plugin-header header>
                Add group members
            </tim-plugin-header>
            <p stem>
                Add usernames line-by-line or comma separated. Adding already added users is ok; they will not be
                added twice.
            </p>
            <ng-container body>
                <textarea class="form-control" [(ngModel)]="users" placeholder="Add usernames here">
                </textarea>
                <button class="timButton" (click)="add()">Add</button>
            </ng-container>
            <ng-container footer>
                <div *ngIf="result">
                    <tim-alert severity="success">Group updated.</tim-alert>
                    <ng-container *ngIf="result.added.length > 0">
                        Added:
                        <ul>
                            <li *ngFor="let n of result.added">{{n}}</li>
                        </ul>
                    </ng-container>
                    <ng-container *ngIf="result.already_belongs.length > 0">
                        Already belongs:
                        <ul>
                            <li *ngFor="let n of result.already_belongs">{{n}}</li>
                        </ul>
                    </ng-container>
                    <ng-container *ngIf="result.not_exist.length > 0">
                        Non-existent users:
                        <ul>
                            <li *ngFor="let n of result.not_exist">{{n}}</li>
                        </ul>
                    </ng-container>
                </div>
                <p *ngIf="error">{{error}}</p>
            </ng-container>
        </tim-plugin-frame>
    `,
    styleUrls: [],
})
export class AddMemberComponent {
    users = "";
    result?: IAddmemberResponse;
    error?: string;
    @Input() group!: string;

    constructor(private http: HttpClient) {
    }

    async add() {
        this.result = undefined;
        this.error = undefined;
        const r = await to2(this.http.post<IAddmemberResponse>(`/groups/addmember/${this.group}`, {
            names: this.users.split("\n").flatMap((n) => n.split(",")).map((n) => n.trim()),
        }).toPromise());
        if (r.ok) {
            this.result = r.result;
        } else {
            this.error = r.result.error.error;
        }
    }
}
