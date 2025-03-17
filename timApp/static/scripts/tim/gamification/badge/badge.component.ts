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
        <div class="tooltip" *ngIf="description">
            
            <p><b>Description:</b></p>
            <p>{{ description }}</p>
            
            <div *ngIf="message">
                <p><b>Message:</b></p>
                <p>{{ message }}</p>
            </div>
            
        </div>
</div>

  `,
    styleUrls: ["badge.component.scss"],
})
export class BadgeComponent implements OnInit, OnChanges {
    @Input() id?: number;
    @Input() title?: string;
    @Input() color?: string;
    @Input() shape?: string;
    @Input() image?: string;
    @Input() description?: string;
    @Input() message?: string;
    icon?: string;

    private readonly iconMap: Record<string, string> = {
        Trophy: "trophy",
        Winner: "editor_choice",
        Teamwork: "diversity_3",
        Code: "code",
        Debug: "bug_report",
        On_fire: "local_fire_department",
        Rocket: "rocket_launch",
        Smile: "sentiment_satisfied",
        Terminal: "terminal",
        Deployed: "deployed_code",
        Loop: "loop",
        Full_points: "money",
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
        if (this.image && this.iconMap[this.image]) {
            this.icon = this.iconMap[this.image];
        } else {
            this.icon = "question_mark";
        }
    }
}

@NgModule({
    declarations: [BadgeComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeComponent],
})
export class BadgeModule {}
