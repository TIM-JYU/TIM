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

import {ILanguage, ITranslator} from "tim/item/IItem";
import {Result, to} from "tim/util/utils";
import {$http} from "tim/util/ngimport";

type SupportedLanguagesAndTranslators = {
    source: ILanguage[];
    document: ILanguage[];
    target: ILanguage[];
    translators: ITranslator[];
    availableTransls: string[];
    error: string;
};

type SupportedLanguages = {
    source: ILanguage[];
    target: ILanguage[];
};

/**
 * Fetches ILanguage data from the given url with either post or get and updates the given list.
 * @param url the url to fetch from
 * @param list the ILanguage list that needs to be updated
 * @param data the data needed for the post request
 * @param post whether or not a post request needs to be made
 * @returns The error message if the fetching failed or an empty string
 */
async function fetchData(
    url: string,
    list: ILanguage[],
    data: Record<string, string>,
    post: boolean
) {
    let sources;
    if (post) {
        sources = await to($http.post<ILanguage[]>(url, data));
    } else {
        sources = await to($http.get<ILanguage[]>(url));
    }
    if (sources.ok) {
        list.push(...sources.result.data);
        return "";
    } else {
        return sources.result.data.error;
    }
}

/**
 * Fetches the lists of the languages supported by the chosen translator.
 * @param lists The lists of languages
 * @param translator The chosen translator
 * @returns The fetched lists or last error message that came up
 */
async function updateLanguages(
    lists: SupportedLanguagesAndTranslators,
    translator: string
): Promise<Result<SupportedLanguagesAndTranslators, string>> {
    let error = "";

    error = await fetchData(
        "/translations/sourceLanguages",
        lists.source,
        {
            translator: translator,
        },
        true
    );
    error = await fetchData(
        "/translations/allLanguages",
        lists.document,
        {},
        false
    );
    error = await fetchData(
        "/translations/targetLanguages",
        lists.target,
        {
            translator: translator,
        },
        true
    );

    if (error == "") {
        return {
            ok: true,
            result: lists,
        };
    }
    return {
        ok: false,
        result: error,
    };
}

/**
 * Fetches the list of the available translators.
 * @param includeManual Whether or not the option for manual translation should be included in the list
 * @returns The list of translators or error message if the request was unsuccessful
 */
export async function listTranslators(
    includeManual: boolean
): Promise<Result<ITranslator[], string>> {
    const translators = [];
    const sources = await to($http.get<string[]>("/translations/translators"));
    if (sources.ok) {
        for (const translator of sources.result.data) {
            if (translator == "Manual" && !includeManual) {
                continue;
            }
            translators.push({name: translator, available: false});
        }
        return {
            ok: true,
            result: translators,
        };
    } else {
        if (includeManual) {
            translators.push({name: "Manual", available: false});
        }
        return {
            ok: false,
            result: sources.result.data.error,
        };
    }
}

/**
 * Fetches the translators the user can use.
 * @param includeManual Whether or not the option for manual translation should be included in the list
 * @returns The list of available translators or the error message if the request was unsuccessful
 */
export async function availableTranslators(
    includeManual: boolean
): Promise<Result<string[], string>> {
    const translators: string[] = [];
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
        return {
            ok: true,
            result: translators,
        };
    } else {
        return {
            ok: false,
            result: sources.result.data.error,
        };
    }
}

/**
 * Fetches the data regarding translations and returns it for handling.
 * @param docTranslator the document's translator
 * @param errorMessage the error message user is shown if something goes wrong
 * @param includeManual whether or not Manual should be listed among translators
 * @returns The lists of translations, translators, their availability and the last error
 */
export async function updateTranslationData(
    docTranslator: string,
    errorMessage: string,
    includeManual: boolean
) {
    let results: SupportedLanguagesAndTranslators = {
        source: [],
        document: [],
        target: [],
        translators: [],
        availableTransls: [],
        error: "",
    };

    results.error = errorMessage;
    const error = ["", "", ""];

    const response1 = await listTranslators(includeManual);
    if (response1.ok) {
        results.translators = response1.result;
    } else {
        error[0] = response1.result;
    }

    const response2 = await updateLanguages(results, docTranslator);
    if (response2.ok) {
        results = response2.result;
    } else {
        error[1] = response2.result;
    }

    const response3 = await availableTranslators(includeManual);
    if (response3.ok) {
        results.availableTransls = response3.result;
    } else {
        error[2] = response3.result;
    }
    for (const errors of error) {
        if (errors != "") {
            results.error = errors;
        }
    }
    for (const tr of results.translators) {
        tr.available = results.availableTransls.includes(tr.name);
    }

    return results;
}

/**
 * Fetches the lists of languages supported by the given translator.
 * @param translator The translator the languages are fetched for
 * @return The lists of supported languages or an error message if something went wrong
 */
export async function updateTranslatorLanguages(
    translator: string
): Promise<Result<SupportedLanguages, string>> {
    const results: SupportedLanguages = {
        source: [],
        target: [],
    };

    let error = await fetchData(
        "/translations/targetLanguages",
        results.target,
        {
            translator: translator,
        },
        true
    );
    if (error != "") {
        return {
            ok: false,
            result: error,
        };
    }
    error = await fetchData(
        "/translations/sourceLanguages",
        results.source,
        {
            translator: translator,
        },
        true
    );
    if (error != "") {
        return {
            ok: false,
            result: error,
        };
    }
    return {
        ok: true,
        result: results,
    };
}
