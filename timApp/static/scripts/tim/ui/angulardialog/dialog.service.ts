import {Injectable} from "@angular/core";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {
    AngularDialogComponent,
    IDialogOptions,
} from "tim/ui/angulardialog/angular-dialog-component.directive";

export let angularDialog = (null as unknown) as DialogService;

@Injectable({
    providedIn: "platform",
})
export class DialogService {
    private container?: DialogContainerComponent;

    constructor() {
        angularDialog = this;
    }

    open<P, R, Dialog extends AngularDialogComponent<P, R>>(
        dialog: new (...args: never[]) => AngularDialogComponent<P, R>,
        params: P,
        dialogOptions?: IDialogOptions
    ): Promise<Dialog> {
        if (!this.container) {
            throw Error("DialogContainerComponent has not been registered.");
        }
        // @ts-expect-error
        return this.container.add(dialog, params, dialogOptions);
    }

    registerContainer(c: DialogContainerComponent) {
        this.container = c;
    }
}
