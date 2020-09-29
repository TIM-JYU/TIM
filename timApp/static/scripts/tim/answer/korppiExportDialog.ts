import {DialogController} from "tim/ui/dialogController";
import {registerDialogComponent, showDialog} from "tim/ui/dialog";
import {IExportOptions} from "tim/answer/userlistController";

export class KorppiExportCtrl extends DialogController<void, IExportOptions> {
    static component = "timKorppiExport";
    static $inject = ["$element", "$scope"] as const;
    private options: IExportOptions = {
        totalPointField: "",
        velpPointField: "",
        taskPointField: "",
        copy: false,
    };

    protected getTitle() {
        return "Export to Korppi";
    }

    ok() {
        this.close(this.options);
    }

    copy() {
        this.options.copy = true;
        this.close(this.options);
    }
}

registerDialogComponent(KorppiExportCtrl, {
    templateUrl: "/static/templates/korppiExport.html",
});

export function showKorppiExportDialog() {
    return showDialog(KorppiExportCtrl, {}).result;
}
