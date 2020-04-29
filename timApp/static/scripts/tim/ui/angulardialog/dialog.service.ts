import {Injectable} from "@angular/core";
import {DialogContainerComponent} from "tim/ui/angulardialog/dialog-container.component";
import {AngularDialogComponent, IDialogOptions} from "tim/ui/angulardialog/angular-dialog-component.directive";

export let angularDialog = null as unknown as DialogService;

@Injectable(
    {
        providedIn: "platform",
    }
)
export class DialogService {
    private container?: DialogContainerComponent;

    constructor() {
        angularDialog = this;
    }

    open<P, R>(dialog: new (...args: unknown[]) => AngularDialogComponent<P, R>, params: P, dialogOptions?: IDialogOptions): Promise<AngularDialogComponent<P, R>> {
        if (!this.container) {
            throw Error("DialogContainerComponent has not been registered.");
        }
        return this.container.add(dialog, params, dialogOptions);
    }

    registerContainer(c: DialogContainerComponent) {
        this.container = c;
    }
}
