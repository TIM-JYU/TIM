import type {OnInit, OnChanges} from "@angular/core";
import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {cons} from "fp-ts/ReadonlyNonEmptyArray";
import {BadgeService} from "tim/gamification/badge/badge.service";

@Component({
    selector: "tim-badge",
    template: `
    <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
    <div class="badge-container" [ngClass]="['badge', color, shape]" (click)="openDialog()">
        <div class="circle">
            <span class="material-symbols-outlined">{{ icon }}</span>
        </div>
        <div class="ribbon">{{ title }}</div>
        
        <span class="tooltip">
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
    @Input() preventDialog: boolean = false;

    icon?: string;

    private readonly iconMap: Record<number, string> = {
        1: "trophy",
        2: "editor_choice",
        3: "diversity_3",
        4: "code",
        5: "bug_report",
        6: "local_fire_department",
        7: "rocket_launch",
        8: "sentiment_satisfied",
        9: "terminal",
        10: "deployed_code",
        11: "loop",
        12: "money",
    };

    // hakee dialogServicen BadgeServicesta
    constructor(private dialogService: BadgeService) {}

    // Avaa valitun badgen dialogin yksi ikkuna kerrallaan, josta näkee Descriptionin ja Messagen
    async openDialog(): Promise<void> {
        if (this.dialogService.isDialogOpen() || this.preventDialog) {
            return;
        }

        this.dialogService.setDialogOpen(true);

        await showMessageDialog(`
          <b>${this.title}</b><br><br>
          <b>Description:</b> ${this.description}<br>
          <b>Message:</b> ${this.message}
        `);

        this.dialogService.setDialogOpen(false);
    }

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
