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
        <p></p>
        <div class="lisaaBadge">
            <span> 
                <input type="text" [(ngModel)]="newBadge"/> 
                <button class="addBadgeButton" (click)="addBadge()">Lisää badge</button>
            </span>
        </div>
        
    `,
    styleUrls: [],
})
export class BadgeTestComponent implements OnInit {
    badges: IBadge[] = [{text: "badge1"}, {text: "badge2"}, {text: "badge3"}];
    newBadge: string = "";
    ngOnInit() {}

    addBadge() {
        if (this.newBadge) {
            this.badges.push({
                text: this.newBadge,
            });
        }
        this.newBadge = "";
    }
}

@NgModule({
    declarations: [BadgeTestComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgeTestComponent],
})
export class BadgeTestModule {}
