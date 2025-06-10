import {IReportContentParams} from "./report-content-dialog.component";
import {angularDialog} from "./angulardialog/dialog.service";

export async function showReportContentDialog(params?: IReportContentParams) {
    const {ReportContentDialogComponent} = await import(
        "./report-content-dialog.component"
    );
    return await angularDialog.open(ReportContentDialogComponent, params);
}
