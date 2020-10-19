import {IAskedQuestion, IUniqueParId} from "tim/lecture/lecturetypes";
import {to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

/**
 * FILL WITH SUITABLE TEXT
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export interface IShowAsk {
    showAsk: boolean;
}

export interface IAskNew extends IUniqueParId {}

export interface IReAsk {
    askedId: number;
}

export type AskParams = IAskNew | IReAsk;

export function isReasking(p: AskParams): p is IReAsk {
    return (p as IReAsk).askedId != null;
}

export async function askQuestion(p: AskParams) {
    const args = isReasking(p)
        ? {
              asked_id: p.askedId,
          }
        : {
              doc_id: p.docId,
              par_id: p.parId,
          };
    const response = await to(
        $http.post<IAskedQuestion>(
            "/askQuestion",
            {},
            {
                params: {buster: new Date().getTime(), ...args},
            }
        )
    );
    if (!response.ok) {
        throw Error("askQuestion failed");
    }
    return response.result.data;
}
