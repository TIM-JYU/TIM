import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";

@Component({
    selector: "tim-participant-list",
    styleUrls: ["./participant-list.component.scss"],
    imports: [CsUtilityModule, FormsModule, CommonModule],
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
export class ParticipantListComponent implements OnInit {
    @Input() documentId: number = 0;
    @Input() modifyEnabled: boolean = false;
    editable: boolean = false;

    constructor(private http: HttpClient) {}

    ngOnInit() {}

    async onSubmit() {}
}

@NgModule({
    imports: [ParticipantListComponent],
})
export class UserListModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
