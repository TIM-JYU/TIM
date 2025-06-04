import type {OnInit, OnChanges, SimpleChanges} from "@angular/core";
import {Component, Input, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "tim-badge",
    template: `
    <div class="badge-container" [ngClass]="['badge', color, shape]">
        <div class="icon">
          <img [src]="'/static/scripts/vendor/material-design-icons/' + icon + '.svg'" alt="{{ icon }}" />
        </div>

        <div class="ribbon">{{ title }}</div>
        
        <span *ngIf="description" class="tooltip">
            <b>Description:</b><br>{{ description }} 
            <ng-container *ngIf="message">
                <br><b>Message:</b><br>{{ message }}
            </ng-container>
        </span>        
    </div>
  `,
    styleUrls: ["badge.component.scss"],
})
export class BadgeComponent implements OnInit, OnChanges {
    @Input() id?: number;
    @Input() title?: string;
    @Input() color?: string;
    @Input() shape?: string;
    @Input() image?: number;
    @Input() description?: string;
    @Input() message?: string;
    @Input() disableDialogWindow?: boolean;

    icon?: string;

    private readonly iconMap: Record<number, string> = {
        1: "trophy",
        2: "winner",
        3: "teamwork",
        4: "code",
        5: "bug",
        6: "on_fire",
        7: "rocket",
        8: "smile",
        9: "terminal",
        10: "deployed_code",
        11: "loop",
        12: "100_points",
    };

    ngOnInit(): void {
        this.setIcon();
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.image) {
            this.setIcon();
        }
    }

    /**
     * Sets the icon based on the image property. If no icon matches,
     * the deafault "question_mark" is set.
     *
     * @returns void
     */
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
