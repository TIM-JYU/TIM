import type {OnInit, OnChanges} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-badge",
    template: `
    <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
    <div class="badge-container" [ngClass]="['badge', color, shape]">
    <div class="circle">
        <span class="material-symbols-outlined">{{ icon }}</span>
    </div>
    <div class="ribbon">{{ title }}</div>
    <!-- Tooltip for description -->
    <div class="tooltip" *ngIf="description">{{ description }}</div>
        <div class="tooltip" *ngIf="message">{{ message }}</div>
</div>

  `,
    styleUrls: ["badge.component.css"],
})
export class BadgeComponent implements OnInit, OnChanges {
    @Input() id?: number;
    @Input() title?: string;
    @Input() color?: string;
    @Input() shape?: string;
    @Input() image?: number;
    @Input() description?: string;
    @Input() message?: string;
    icon?: string;

    ngOnInit() {}

    ngOnChanges() {
        this.icon = "help";

        if (this.image == 1) {
            this.icon = "diversity_3";
        }
        if (this.image == 2) {
            this.icon = "code";
        }
        if (this.image == 3) {
            this.icon = "trophy";
        }
        if (this.image == 4) {
            this.icon = "editor_choice";
        }
        if (this.image == 5) {
            this.icon = "bug_report";
        }
        if (this.image == 6) {
            this.icon = "liquor";
        }
    }
}

@NgModule({
    declarations: [BadgeComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeComponent],
})
export class BadgeModule {}
