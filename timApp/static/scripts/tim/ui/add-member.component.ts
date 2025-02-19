import {Component, Input} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {splitItems, toPromise} from "tim/util/utils";

export interface IAddmemberResponse {
    added: string[];
    already_belongs: string[];
    not_exist: string[];
}

@Component({
    selector: "tim-add-member",
    template: `
        <tim-plugin-frame [markupError]="undefined">
            <tim-plugin-header header>
                Lisää ryhmän jäseniä
            </tim-plugin-header>
            <p stem>
                Lisää käyttäjätunnukset tai tunnuksia vastaavat sähköpostiosoitteet allekkain tai pilkuilla eroteltuina.
                Ei haittaa, vaikka listassa olisi jo lisättyjä jäseniä; heitä ei lisätä toista kertaa.
            </p>
            <ng-container body>
                <textarea class="form-control" [(ngModel)]="users" placeholder="Kirjoita nimet tähän">
                </textarea>
                <button class="timButton" (click)="add()">Lisää</button>
            </ng-container>
            <ng-container footer>
                <div *ngIf="result">
                    <tim-alert severity="success">Päivitetty.</tim-alert>
                    <ng-container *ngIf="result.added.length > 0">
                        Lisätyt:
                        <ul>
                            <li *ngFor="let n of result.added">{{n}}</li>
                        </ul>
                    </ng-container>
                    <ng-container *ngIf="result.already_belongs.length > 0">
                        Ryhmään jo kuuluvat:
                        <ul>
                            <li *ngFor="let n of result.already_belongs">{{n}}</li>
                        </ul>
                    </ng-container>
                    <ng-container *ngIf="result.not_exist.length > 0">
                        Käyttäjiä ei löydy:
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

    constructor(private http: HttpClient) {}

    async add() {
        this.result = undefined;
        this.error = undefined;
        const r = await toPromise(
            this.http.post<IAddmemberResponse>(
                `/groups/addmember/${this.group}`,
                {
                    names: splitItems(this.users),
                }
            )
        );
        if (r.ok) {
            this.result = r.result;
        } else {
            this.error = r.result.error.error;
        }
    }
}
