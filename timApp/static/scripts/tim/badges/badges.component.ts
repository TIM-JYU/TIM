import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {FormsModule} from "@angular/forms";

@Component({
    selector: "app-badges",
    templateUrl: "./badges.component.html",
    styleUrls: ["./badges.component.scss"],
})
@NgModule({
    declarations: [BadgesComponent],
    imports: [CommonModule, FormsModule],
    exports: [BadgesComponent],
})
export class BadgesComponent {}

export class BadgesComponentModule {}
