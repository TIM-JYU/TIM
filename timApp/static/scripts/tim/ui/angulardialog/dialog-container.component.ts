import {Component, NgZone} from "@angular/core";
import {TimDefer} from "tim/util/timdefer";
import {DialogService} from "tim/ui/angulardialog/dialog.service";
import {
    DialogConstructor,
    IDialogInstanceEvent,
} from "tim/ui/angulardialog/dialog-host.directive";
import {
    AngularDialogComponent,
    IDialogOptions,
} from "tim/ui/angulardialog/angular-dialog-component.directive";

let nextDialogId = 0;

interface IDialogEntry {
    comp: DialogConstructor;
    data: unknown;
    id: number;
    dialogOptions?: IDialogOptions;
}

@Component({
    selector: "tim-dialog-container",
    template: `
        <div *ngFor="let c of dialogs"
             [timDialogHost]="c.comp"
             [hostData]="c.data"
             [instanceId]="c.id"
             [dialogOptions]="c.dialogOptions"
             (instanceCreated)="created($event)"
             (instanceClosed)="closed($event)"></div>
    `,
})
export class DialogContainerComponent {
    dialogs: IDialogEntry[] = [];
    private loading = new Map<
        number,
        TimDefer<AngularDialogComponent<unknown, unknown>>
    >();

    constructor(private ds: DialogService, private zone: NgZone) {
        ds.registerContainer(this);
    }

    created(d: IDialogInstanceEvent) {
        const instPromise = this.loading.get(d.id);
        if (!instPromise) {
            throw Error("dialog promise not found");
        }
        instPromise.resolve(d.instance);
    }

    closed(d: IDialogInstanceEvent) {
        const f = this.dialogs.findIndex((entry) => entry.id === d.id);
        if (f < 0) {
            throw Error("closed dialog not found in list");
        }
        this.dialogs.splice(f, 1);
    }

    add<P, R>(
        dialog: new (...args: never[]) => AngularDialogComponent<P, R>,
        params: P,
        dialogOptions?: IDialogOptions
    ): Promise<AngularDialogComponent<P, R>> {
        return this.zone.run(() => {
            const p = {
                comp: dialog as DialogConstructor,
                data: params,
                id: nextDialogId,
                dialogOptions,
            };
            nextDialogId++;
            const defer = new TimDefer<AngularDialogComponent<P, R>>();
            this.dialogs.push(p);
            this.loading.set(
                p.id,
                defer as TimDefer<AngularDialogComponent<unknown, unknown>>
            );
            return defer.promise;
        });
    }
}
