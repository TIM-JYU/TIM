import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";
import {getURLParameter} from "tim/util/utils";
import {inIframe} from "tim/plugin/util";
import {$httpParamSerializer} from "tim/util/ngimport";
import {showLoginDialog} from "tim/user/showLoginDialog";
import {Users} from "tim/user/userService";

@Component({
    selector: "user-profile",
    template: `
        <div [innerHTML]="'Say hello'">
            Hello
            <button (click)="clickThis()"></button>
        </div>
    `,
    styleUrls: ["./user-profile.component.scss"],
})
export class UserProfileComponent implements OnInit {
    ngOnInit() {
        console.log("Kisu123");
    }

    clickThis() {
        console.log("Hello123");
    }
}
/*
@NgModule({
    declarations: [UserProfileComponent],
})
export class UserProfileModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
*/
