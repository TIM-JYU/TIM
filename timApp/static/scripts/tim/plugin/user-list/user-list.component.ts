import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {StandaloneTextfieldComponent} from "../../../../../modules/fields/js/standalone-textfield.component";

@Component({
    selector: "tim-user-list",
    styleUrls: ["./user-list.component.scss"],
    imports: [
        StandaloneTextfieldComponent,
        CsUtilityModule,
        FormsModule,
        CommonModule,
    ],
    standalone: true,
    template: `
        <div class="tim-user-list-container">
            <div class="heading">
                <h2>Participants listing</h2>
            </div>
            <div class="container">
                Participants
            </div>
        </div>
    `,
})
export class UserListComponent implements OnInit {
    @Input() documentId: number = 0;
    @Input() modifyEnabled: boolean = false;
    editable: boolean = false;

    constructor(private http: HttpClient) {}

    ngOnInit() {}

    async onSubmit() {}
}

@NgModule({
    imports: [UserListComponent],
})
export class UserListModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
