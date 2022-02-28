import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnInit,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import * as t from "io-ts";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {Users} from "../../user/userService";

const TodoItem = t.partial({
    done: t.boolean,
    text: t.string,
});

// All settings that are defined in the plugin markup YAML
const TodoListMarkup = t.intersection([
    t.partial({
        defaultTodos: nullable(t.array(TodoItem)),
    }),
    GenericPluginMarkup,
]);

// All data that plugin receives from the server (Markup + any extra state data)
const TodoListFields = t.intersection([
    getTopLevelFields(TodoListMarkup),
    t.type({}),
]);

interface ITodo extends t.TypeOf<typeof TodoItem> {}

@Component({
    selector: "tim-todo-list",
    template: `
    <p *ngIf="!userName">Please log in to see your TODOs!</p>
    <ng-container *ngIf="userName">
        <p>Hello, {{userName}}!</p>
        <p>Here are your personal TODO:s</p>
        <ul>
            <li *ngFor="let todo of todos" class="todo-item">
                <input type="checkbox" [(ngModel)]="todo.done"/>
                <span class="todo-text">{{todo.text}}</span> 
                <span class="glyphicon glyphicon-remove-circle remove" (click)="deleteTodo(todo)"></span>
            </li>
        </ul>
        <div class="add-todo">
            <input type="text" [(ngModel)]="newTodo"/>
            <button class="timButton" (click)="addTodo()" [disabled]="!newTodo">Add TODO</button>
        </div>
    </ng-container>
    `,
    styleUrls: ["./todo-list.component.scss"],
})
export class TodoListComponent
    extends AngularPluginBase<
        t.TypeOf<typeof TodoListMarkup>,
        t.TypeOf<typeof TodoListFields>,
        typeof TodoListFields
    >
    implements OnInit
{
    newTodo?: string;
    userName?: string;
    usersInSession?: number;
    todos: ITodo[] = [];

    ngOnInit() {
        super.ngOnInit();
        if (Users.isLoggedIn()) {
            this.userName = Users.getCurrent().name;
            this.usersInSession = Users.getSessionUsers().length;
        }
        if (this.markup.defaultTodos) {
            this.todos = this.markup.defaultTodos;
        }
    }

    deleteTodo(todo: ITodo) {
        this.todos = this.todos.filter((td) => td !== todo);
    }

    addTodo() {
        if (!this.newTodo) {
            return;
        }
        this.todos.push({
            done: false,
            text: this.newTodo,
        });
        this.newTodo = "";
    }

    // Boilerplate: Return the encoder for all markup fields
    getAttributeType() {
        return TodoListFields;
    }

    // Boilerplate: Return default values if any
    getDefaultMarkup() {
        return {};
    }
}

@NgModule({
    declarations: [TodoListComponent],
    exports: [TodoListComponent],
    imports: [BrowserModule, HttpClientModule, FormsModule],
})
export class TodoModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

// Downgrate TodoModule to AngularJS module
// Note: We need ngDoBootstrap above because we manually create the module
const angularJsModule = createDowngradedModule((extraProviders) =>
    platformBrowserDynamic(extraProviders).bootstrapModule(TodoModule)
);

// Downgrade TodoListComponent to AngularJS component
// Note: tim-todo-list => timTodoList conversion
doDowngrade(angularJsModule, "timTodoList", TodoListComponent);

// Export the module
export const moduleDefs = [angularJsModule];
