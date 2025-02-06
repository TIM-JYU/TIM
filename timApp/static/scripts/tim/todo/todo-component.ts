import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-todo",
    template: "<p>Hello world!</p>",
    styleUrls: [],
})
export class TodoListComponent implements OnInit {
    ngOnInit() {}
}

@NgModule({
    declarations: [TodoListComponent],
    imports: [CommonModule, FormsModule],
    exports: [TodoListComponent],
})
export class TodoModule {}
