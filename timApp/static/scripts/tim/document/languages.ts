/**
 * Global functions related to getting languages and translators and data related to them from the server.
 * @author Noora Jokela
 * @author Sami Viitanen
 * @author Juho Tarkkanen
 * @author Vili Moisala
 * @date 29.4.2022
 * @licence MIT
 * @copyright 2022 TIMTra project authors
 */

import {ILanguage, ITranslator} from "../item/IItem";
import {to} from "../util/utils";
import {$http} from "../util/ngimport";

/**
 * Goes through the given language list and inserts each of its members into it.
 * @param languages The list of languages to be added
 * @param languageArray The list the languages need to be added to
 */
export function listLanguages(
    languages: ILanguage[],
    languageArray: ILanguage[]
) {
    for (const lang of languages) {
        languageArray.push(lang);
    }
}

/**
 * Fetches the lists of the languages supported by the chosen translator and lists them to front-end's language lists.
 * @param sourceL The list of supported source languages
 * @param docL The list of document languages available
 * @param targetL The list of supported target languages
 * @param translator The chosen translator
 * @returns The last error message that came up.
 */
export async function updateLanguages(
    sourceL: ILanguage[],
    docL: ILanguage[],
    targetL: ILanguage[],
    translator: string
) {
    let error = "";
    let sources = await to(
        $http.post<ILanguage[]>("/translations/sourceLanguages", {
            translator: translator,
        })
    );
    if (sources.ok) {
        listLanguages(sources.result.data, sourceL);
    } else {
        error = sources.result.data.error;
    }

    sources = await to(
        $http.get<ILanguage[]>("/translations/documentLanguages")
    );
    if (sources.ok) {
        listLanguages(sources.result.data, docL);
    } else {
        error = sources.result.data.error;
    }

    sources = await to(
        $http.post<ILanguage[]>("/translations/targetLanguages", {
            translator: translator,
        })
    );
    if (sources.ok) {
        listLanguages(sources.result.data, targetL);
    } else {
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Fetches the list of the available translators and adds them to front-end's list of them.
 * @param translators The list the translators will be added to
 * @param includeManual Whether or not the option for manual translation should be included in the list
 * @returns The error message if the request was unsuccessful.
 */
export async function listTranslators(
    translators: ITranslator[],
    includeManual: boolean
) {
    let error = "";
    const sources = await to($http.get<string[]>("/translations/translators"));
    if (sources.ok) {
        for (const translator of sources.result.data) {
            if (translator == "Manual" && !includeManual) {
                continue;
            }
            translators.push({name: translator, available: false});
        }
    } else {
        if (includeManual) {
            translators.push({name: "Manual", available: false});
        }
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Fetches the translators the user can use
 * @param translators the list the translators will be added to
 * @param includeManual Whether or not the option for manual translation should be included in the list
 * @returns The error message if the request was unsuccessful.
 */
export async function availableTranslators(
    translators: string[],
    includeManual: boolean
) {
    let error = "";
    const sources = await to(
        $http.get<string[]>("/translations/myTranslators")
    );
    if (includeManual) {
        translators.push("Manual");
    }
    if (sources.ok) {
        for (const translator of sources.result.data) {
            translators.push(translator);
        }
    } else {
        error = sources.result.data.error;
    }
    return error;
}

/**
 * Checks if the translator is available for the user.
 * @param tr the translator getting checked
 * @param translators the list of translators to check through
 */
export function isOptionAvailable(tr: ITranslator, translators: string[]) {
    for (const translator of translators) {
        if (tr.name == translator) {
            tr.available = true;
        }
    }
}

/**
 * Handles updating data regarding translations.
 * @param sourceLanguages the list of source languages
 * @param documentLanguages the list of document languages
 * @param targetLanguages the list of target languages
 * @param docTranslator the document's translator
 * @param translators the full list of translators
 * @param availableTrs the list of translators the user can use
 * @param errorMessage the error message user is shown if something goes wrong
 * @param includeManual whether or not Manual should be listed among translators
 */
export async function updateTranslationData(
    sourceLanguages: ILanguage[],
    documentLanguages: ILanguage[],
    targetLanguages: ILanguage[],
    docTranslator: string,
    translators: ITranslator[],
    availableTrs: string[],
    errorMessage: string,
    includeManual: boolean
) {
    let finalError = errorMessage;
    const error = ["", "", ""];
    error[0] = await listTranslators(translators, includeManual);
    error[1] = await updateLanguages(
        sourceLanguages,
        documentLanguages,
        targetLanguages,
        docTranslator
    );
    error[2] = await availableTranslators(availableTrs, includeManual);
    for (const errors of error) {
        if (errors != "") {
            finalError = errors;
        }
    }
    for (const tr of translators) {
        isOptionAvailable(tr, availableTrs);
    }
    return finalError;
}
