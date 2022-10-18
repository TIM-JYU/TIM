import {to2} from "tim/util/utils";
import {showInputDialog} from "tim/ui/showInputDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";

export async function showConfirm(
    title: string,
    message: string
): Promise<boolean> {
    const ans = await to2(
        showInputDialog(
            {
                isInput: InputDialogKind.NoValidator,
                okValue: true,
                text: message,
                title: title,
                asyncContent: false,
            },
            {resetPos: true}
        )
    );
    return ans.ok && ans.result;
}
