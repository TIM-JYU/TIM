import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

interface IBadge {
    text: string;
}

@Component({
    selector: "tim-badge-test",
    template: `
        <p>Hello world</p>
        <div *ngFor='let badge of badges'>{{badge.text}}</div>
    `,
    styleUrls: [],
})
export class BadgeTestComponent implements OnInit {
    badges: IBadge[] = [{text: "badge1"}, {text: "badge2"}, {text: "badge3"}];
    ngOnInit() {}
}

@NgModule({
    declarations: [BadgeTestComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeTestComponent],
})
export class BadgeTestModule {}
