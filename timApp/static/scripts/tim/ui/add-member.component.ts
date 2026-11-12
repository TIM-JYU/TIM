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
            @if (result) {
              <div>
                <tim-alert severity="success">Päivitetty.</tim-alert>
                @if (result.added.length > 0) {
                  Lisätyt:
                  <ul>
                    @for (n of result.added; track n) {
                      <li>{{n}}</li>
                    }
                  </ul>
                }
                @if (result.already_belongs.length > 0) {
                  Ryhmään jo kuuluvat:
                  <ul>
                    @for (n of result.already_belongs; track n) {
                      <li>{{n}}</li>
                    }
                  </ul>
                }
                @if (result.not_exist.length > 0) {
                  Käyttäjiä ei löydy:
                  <ul>
                    @for (n of result.not_exist; track n) {
                      <li>{{n}}</li>
                    }
                  </ul>
                }
              </div>
            }
            @if (error) {
              <p>{{error}}</p>
            }
          </ng-container>
        </tim-plugin-frame>
        `,
    styleUrls: [],
    standalone: false,
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
