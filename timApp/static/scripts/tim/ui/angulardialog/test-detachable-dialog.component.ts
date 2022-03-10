import {Component, OnInit} from "@angular/core";

@Component({
    selector: "tim-test-detachable-dialog",
    template: `
    <tim-dialog-frame [detachable]="true">
        <ng-container header>
            Test
        </ng-container>
        <ng-container body>
            This is a body!
        </ng-container>
    </tim-dialog-frame>
  `,
    styles: [],
})
export class TestDetachableDialogComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
