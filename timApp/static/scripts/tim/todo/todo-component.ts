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
        <p>Hello world! id: {{userID}}</p>
        <div *ngFor="let answer of answers">{{answer.points}}</div>
    `,
    styleUrls: [],
})
export class TodoListComponent implements OnInit {
    userID?: number;
    answers?: IAnswer[];
    constructor(private http: HttpClient) {}

    ngOnInit() {
        if (Users.isLoggedIn()) {
            this.userID = Users.getCurrent().id;
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
