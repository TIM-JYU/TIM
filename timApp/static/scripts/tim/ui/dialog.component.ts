import {Component} from "@angular/core";

@Component({
    selector: "tim-dialog",
    template: `
        <div style="display: none" class="modal-header">
            <h4 class="modal-title" id="modal-title">
                <ng-content select="dialog-header"></ng-content>
            </h4>
        </div>
        <div class="modal-body" id="modal-body">
            <ng-content select="dialog-body"></ng-content>
        </div>
        <div class="modal-footer">
            <ng-content select="dialog-footer"></ng-content>
        </div>
    `,
    styleUrls: ["./dialog.component.scss"],
})
export class DialogComponent {
}
