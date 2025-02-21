import type {OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-badge",
    template: `
    <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
    <div [class]="['badge', this.color]">
      <div class="circle"> <span class="material-symbols-outlined">{{icon}}</span></div>
      <div class="ribbon">{{title}}</div>
    </div>
    `,
    styleUrls: ["badge.component.css"],
})
export class BadgeComponent implements OnInit {
    id?: number;
    title?: string;
    color?: string;
    shape?: string;
    image?: number;
    description?: string;
    icon?: string;

    ngOnInit() {
        this.image = 1;

        if (this.image == 1) {
            this.icon = "diversity_3";
        }
        if (this.image == 2) {
            this.icon = "code";
        }
    }
}

@NgModule({
    declarations: [BadgeComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeComponent],
})
export class BadgeModule {}
