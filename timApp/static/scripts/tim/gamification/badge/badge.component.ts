import type {OnInit, OnChanges} from "@angular/core";
import {Component, Input, NgModule, SimpleChanges} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";
import {BadgeService} from "tim/gamification/badge/badge.service";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {MessageDialogComponent} from "tim/ui/message-dialog.component";

@Component({
    selector: "tim-badge",
    template: `
    <link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" rel="stylesheet" />
    <div class="badge-container" [ngClass]="['badge', color, shape]">
        <div class="icon">
            <span class="material-symbols-outlined">{{ icon }}</span>
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

    async openDialog(): Promise<void> {
        if (this.preventDialog) {
            this.dialogService.closeActiveDialog();
            return;
        }
        // Close any open dialog
        this.dialogService.closeActiveDialog();

        // Open a new dialog
        this.dialogService.activeDialogRef = await angularDialog.open(
            MessageDialogComponent,
            {
                message: `
            <b>${this.title}</b><br><br>
            <b>Description:</b> ${this.description}<br>
            <b>Message:</b> ${this.message}
            <b>Color:</b> ${this.color}
            <b>Icon:</b> ${this.icon}
            <b>Shape:</b> ${this.shape}
            
        `,
                modal: false,
            }
        );

        // Wait for the dialog to close
        await this.dialogService.activeDialogRef.result;
        this.dialogService.activeDialogRef = null; // Reset the reference after closing
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
