import {Component, Input, NgModule, OnInit} from "@angular/core";
import {CommonModule} from "@angular/common";

@Component({
    selector: "tim-badges",
    templateUrl: "badges.component.html",
})
export class BadgesComponent implements OnInit {
    badges: {text: string; color: string}[] = [];

    // Array of colors
    badgeColors: string[] = [
        "#FF5733", // Red
        "#33FF57", // Green
        "#3357FF", // Blue
        "#FF33A1", // Pink
        "#FFFF33", // Yellow
        "#33FFF5", // Cyan
        "#F533FF", // Purple
    ];

    ngOnInit() {}

    addBadge() {
        const newBadgeText = `Badge ${this.badges.length + 1}`;
        const randomColor = this.getRandomColor();
        this.badges.push({text: newBadgeText, color: randomColor});
        console.log("asd");
    }

    getRandomColor(): string {
        const randomIndex = Math.floor(Math.random() * this.badgeColors.length);
        return this.badgeColors[randomIndex];
    }
}

@NgModule({
    declarations: [BadgesComponent],
    exports: [BadgesComponent],
    imports: [CommonModule],
})
export class BadgesModule {}
