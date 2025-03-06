import type {OnInit, OnChanges} from "@angular/core";
import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
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

    private readonly iconMap: Record<number, string> = {
        1: "trophy",
        2: "editor_choice",
        3: "diversity_3",
        4: "code",
        5: "bug_report",
    };

    ngOnInit(): void {
        this.setIcon();
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.image) {
            this.setIcon();
        }
    }

    setIcon(): void {
        this.icon = this.image
            ? this.iconMap[this.image] || "question_mark"
            : "question_mark";
    }
}

@NgModule({
    declarations: [BadgeComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeComponent],
})
export class BadgeModule {}
