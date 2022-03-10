import {Component, OnInit} from "@angular/core";
import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

@Component({
    selector: "tim-test-detachable-dialog",
    template: `
        <p>Dialog!</p>
    `,
    styles: [],
})
export class TestDetachableDialogComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {
        void createDialog();
    }
}

async function createDialog() {
    await angularDialog.open(TestDetachableDialogDialog, undefined);
}

@Component({
    selector: "tim-test-detachable-dialog-with-header",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                Test
            </ng-container>
            <ng-container body>

                Dialog!

                <tim-dialog-frame [detachable]="true">
                    <ng-container header>
                        Test
                    </ng-container>
                    <ng-container body>
                        This is a body!
                    </ng-container>
                </tim-dialog-frame>

            </ng-container>
        </tim-dialog-frame>
    `,
})
export class TestDetachableDialogDialog extends AngularDialogComponent<
    undefined,
    undefined
> {
    protected dialogName: string = "testDialog";
}
