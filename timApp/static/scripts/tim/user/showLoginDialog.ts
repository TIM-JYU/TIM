import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {to} from "tim/util/utils";
import {ILoginParams} from "tim/user/login-dialog.component";

let instance: AngularDialogComponent<ILoginParams, void> | undefined;

/**
 * Open login dialog if no other instances are opened.
 */
export async function showLoginDialog(params: ILoginParams) {
    if (instance) {
        return;
    }
    const {LoginDialogComponent} = await import("./login-dialog.component");
    const dialog = angularDialog.open(LoginDialogComponent, params, {
        resetSize: true,
    });
    instance = await dialog;
    await to(instance.result);
    instance = undefined;
    return;
}
