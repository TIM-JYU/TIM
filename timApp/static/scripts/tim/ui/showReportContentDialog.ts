import type {IReportContentParams} from "tim/ui/report-content-dialog.component";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showReportContentDialog(params?: IReportContentParams) {
    const {ReportContentDialogComponent} = await import(
        "./report-content-dialog.component"
    );
    return await (
        await angularDialog.open(ReportContentDialogComponent, params)
    ).result;
}
