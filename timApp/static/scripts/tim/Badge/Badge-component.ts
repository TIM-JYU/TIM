import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-badge",
    template: "<p>Hello world!</p>",
    styleUrls: [],
})
export class BadgeComponent implements OnInit {
    ngOnInit() {}
}

@NgModule({
    declarations: [BadgeComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeComponent],
})
export class BadgeModule {}
