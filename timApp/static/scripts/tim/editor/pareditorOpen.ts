import {toPromiseAndRefresh} from "tim/util/utils";
import {getHttpClient} from "tim/util/http";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {IPluginInfoResponse} from "tim/editor/parCompiler";
import type {
    IEditorParams,
    PareditorDialogComponent,
} from "tim/editor/pareditor.component";

/**
 * Opens the paragraph editor.
 *
 * @returns The dialog of the editor. Its "result" promise resolves when the editor is closed.
 */
export async function openEditor(
    p: IEditorParams
): Promise<PareditorDialogComponent> {
    const parEd = await import("tim/editor/pareditor.component");
    // The editor has its own dialog frame, so it only implements the part of AngularDialogComponent
    // that the dialog container uses.
    return (await angularDialog.open(parEd.PareditorDialogComponent as never, {
        params: p,
    })) as unknown as PareditorDialogComponent;
}

export async function openEditorSimple(
    docId: number,
    text: string,
    caption: string,
    localSaveTag: string
) {
    return (
        await openEditor({
            defaultSize: "lg",
            initialText: text,
            extraData: {docId, tags: {markread: false}},
            options: {
                caption: caption,
                choices: undefined,
                localSaveTag: localSaveTag,
                showDelete: false,
                showUpload: true,
                showImageUpload: true,
                showDocumentImport: true,
                showPlugins: false,
                showSettings: false,
                tags: [],
                touchDevice: false,
            },
            previewCb: async (txt, proofread) => {
                const resp = await toPromiseAndRefresh(
                    getHttpClient().post<IPluginInfoResponse>(
                        `/preview/${docId}`,
                        {
                            text: txt,
                            proofread,
                            isComment: true,
                        }
                    )
                );
                if (!resp.ok) {
                    throw new Error("preview route failed");
                }
                return resp.result;
            },
            saveCb: async (txt, data) => {
                return await {};
            },
            deleteCb: async () => {
                return await {};
            },
            unreadCb: async () => {},
        })
    ).result;
}
