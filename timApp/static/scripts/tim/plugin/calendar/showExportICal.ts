import {unknown} from "io-ts";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showExportICal() {
    const {ExportIcalComponent} = await import("./export-ical.component");
    return (
        await angularDialog.open(ExportIcalComponent, unknown, {
            resetSize: true,
        })
    ).result;
}
