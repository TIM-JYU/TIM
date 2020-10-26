import {IParResponse} from "tim/document/editing/edittypes";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";
import {IAskedQuestion, IQuestionParagraph} from "tim/lecture/lecturetypes";
import {showQuestionEditDialog} from "tim/document/question/showQuestionEditDialog";
import {showMessageDialog} from "tim/ui/showMessageDialog";

export async function fetchQuestion(
    docId: number,
    parId: string,
    edit: boolean = true
): Promise<IQuestionParagraph> {
    const response = await to(
        $http<IQuestionParagraph>({
            url: "/getQuestionByParId",
            method: "GET",
            params: {par_id: parId, doc_id: docId, edit},
        })
    );
    if (!response.ok) {
        throw Error("getQuestionByParId failed");
    }
    return response.result.data;
}

export async function fetchAskedQuestion(
    askedId: number
): Promise<IAskedQuestion> {
    const response = await to(
        $http<IAskedQuestion>({
            url: "/getAskedQuestionById",
            method: "GET",
            params: {asked_id: askedId},
        })
    );
    if (!response.ok) {
        throw Error("getQuestionByParId failed");
    }
    return response.result.data;
}

export async function deleteQuestionWithConfirm(
    docId: number,
    parId: string
): Promise<IParResponse | null> {
    const confirmDi = window.confirm(
        "Are you sure you want to delete this question?"
    );
    if (confirmDi) {
        const response = await to(
            $http.post<IParResponse>(`/deleteParagraph/${docId}`, {par: parId})
        );
        if (!response.ok) {
            throw Error("getQuestionByParId failed");
        }
        return response.result.data;
    }
    return null;
}

export async function fetchAndEditQuestion(
    docId: number,
    parId: string,
    edit: boolean = true
) {
    const q = await fetchQuestion(docId, parId, edit);
    if (q.isPreamble) {
        await showMessageDialog("Cannot edit a question in preamble.");
        return undefined;
    }
    return await showQuestionEditDialog(q);
}
