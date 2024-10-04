import {showDialog} from "tim/ui/dialog";
import {to} from "tim/util/utils";
import {$http, $injector} from "tim/util/ngimport";
import type {IPluginInfoResponse} from "tim/editor/parCompiler";
import type {IEditorParams} from "tim/editor/pareditor";

export async function openEditor(p: IEditorParams) {
    const parEd = await import("tim/editor/pareditor");
    const ctrl = parEd.PareditorController;
    $injector.loadNewModules([parEd.parEditorModule.name]);
    return showDialog(
        ctrl,
        {params: () => p},
        {
            saveKey: p.options.localSaveTag,
            absolute: true,
            size: p.defaultSize,
            forceMaximized: true,
        }
    );
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
                const resp = await to(
                    $http.post<IPluginInfoResponse>(`/preview/${docId}`, {
                        text: txt,
                        proofread,
                        isComment: true,
                    })
                );
                if (!resp.ok) {
                    throw new Error("preview route failed");
                }
                return resp.result.data;
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
