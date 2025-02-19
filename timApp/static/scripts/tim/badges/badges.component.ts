import {Component, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-badges",
    template: "<p>Hello kitty :d</p>",
})
export class BadgesComponent implements OnInit {
    ngOnInit() {}
}

@NgModule({
    declarations: [BadgesComponent],
    exports: [BadgesComponent],
    imports: [CommonModule],
})
export class BadgesModule {}
