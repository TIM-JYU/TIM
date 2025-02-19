import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {Users} from "tim/user/userService";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import type {IAnswer} from "tim/answer/IAnswer";

@Component({
    selector: "tim-todo",
    template: `
        <p>Hello {{userName}}! {{xp}} XP</p>
        <div *ngFor="let answer of answers"></div>
    `,
    styleUrls: [],
})
export class TodoListComponent implements OnInit {
    userID?: number;
    userName?: string;
    answers?: IAnswer[];
    xp?: number;
    constructor(private http: HttpClient) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userID = Users.getCurrent().id;
            this.userName = Users.getCurrent().name;
        }
        this.fetchAnswers();
    }

    async fetchAnswers() {
        const response = toPromise(
            this.http.get<IAnswer[]>(
                "/exportAnswers/users/hurmerinta-sakari/todo_test"
            )
        );

        // Get result from response of the endpoint.
        const result = await response;

        if (result.ok) {
            this.answers = result.result;
            this.xp = 0;
            for (const alkio of this.answers) {
                if (alkio.points != undefined) {
                    this.xp += alkio.points * 100;
                }
            }
            console.log(this.answers);
        }

        return result;
    }
}

@NgModule({
    declarations: [TodoListComponent],
    imports: [CommonModule, FormsModule],
    exports: [TodoListComponent],
})
export class TodoModule {}
