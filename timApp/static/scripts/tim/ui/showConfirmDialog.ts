import {to2} from "../util/utils";
import {showInputDialog} from "./showInputDialog";
import {InputDialogKind} from "./input-dialog.kind";

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
