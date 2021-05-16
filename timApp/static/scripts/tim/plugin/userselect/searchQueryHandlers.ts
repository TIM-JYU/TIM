import {HttpClient, HttpParams} from "@angular/common/http";
import {Result, to2} from "../../util/utils";
import {IUser} from "../../user/IUser";

export interface UserResult {
    user: IUser;
    fields: Record<string, string | number | undefined>;
}

export interface SearchResult {
    matches: UserResult[];
    allMatchCount: number;
    fieldNames: string[];
}

export interface IQueryHandler {
    initialize(): Promise<void>;

    searchUser(
        queryStrings: string[],
        maxMatches: number,
        t9Mode: boolean
    ): Promise<Result<SearchResult, {errorMessage: string}>>;
}

export class ServerQueryHandler implements IQueryHandler {
    constructor(
        private http: HttpClient,
        private par: {doc_id: number; par_id: string}
    ) {}

    async initialize() {
        // Nothing to initialize
    }

    async searchUser(queryStrings: string[]) {
        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        const result = await to2(
            this.http
                .post<SearchResult>(
                    "/userSelect/search",
                    {
                        par: this.par,
                        search_strings: queryStrings,
                    },
                    {params}
                )
                .toPromise()
        );
        // seems like proper typing is needed here
        const res: Result<SearchResult, {errorMessage: string}> = result.ok
            ? result
            : {ok: false, result: {errorMessage: result.result.error.error}};
        return res;
    }
}

export class PrefetchedQueryHandler implements IQueryHandler {
    allUsers: {
        userResult: UserResult;
        searchStrings: string[][];
    }[] = [];
    allFields: string[] = [];
    t9Mode: boolean = false;

    constructor(
        private http: HttpClient,
        private par: {doc_id: number; par_id: string}
    ) {}

    async initialize() {
        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        const result = await this.http
            .get<{users: UserResult[]; fieldNames: string[]}>(
                "/userSelect/fetchUsers",
                {
                    params: {
                        doc_id: this.par.doc_id.toString(),
                        par_id: this.par.par_id,
                        ...params,
                    },
                }
            )
            .toPromise();

        const removeEmpty = (s?: string | null): s is string =>
            s !== undefined && s != null;

        this.allUsers = result.users.map((userResult) => ({
            userResult,
            searchStrings: splitIntoWordSets(
                [
                    userResult.user.name,
                    userResult.user.real_name,
                    userResult.user.email,
                    ...Object.values(userResult.fields).map((field) =>
                        field?.toString()
                    ),
                ].filter(removeEmpty)
            ),
        }));
        this.allFields = result.fieldNames;
    }

    searchUser(queryStrings: string[], maxMatches: number, t9Mode: boolean) {
        this.t9Mode = t9Mode;
        // Workaround for "no await in async method"
        return new Promise<Result<SearchResult, {errorMessage: string}>>(
            (accept) => accept(this.searchUserImpl(queryStrings, maxMatches))
        );
    }

    private searchUserImpl(
        queryStrings: string[],
        maxMatches: number
    ): Result<SearchResult, {errorMessage: string}> {
        const queryStringsSet = queryStrings
            .map((s) => s.toLowerCase().trim().split(/\s/))
            .sort((a, b) => b.length - a.length);

        let matches = this.allUsers
            .filter((u) =>
                u.searchStrings.some((valueToCheck) =>
                    queryStringsSet.some((queryWords) =>
                        matchKeywords(queryWords, valueToCheck, this.t9Mode)
                    )
                )
            )
            .map((u) => u.userResult);

        const matchCount = matches.length;
        if (matchCount > maxMatches) {
            matches = matches.slice(0, maxMatches);
        }

        return {
            ok: true,
            result: {
                allMatchCount: matchCount,
                fieldNames: this.allFields,
                matches,
            },
        };
    }
}

function matchKeywords(
    queryWords: string[],
    keywords: string[],
    t9Mode: boolean
) {
    const keywordsSet = new Set(keywords);
    const normalMatch = matchNormalKeywords(queryWords, keywordsSet);
    if (normalMatch) return true;
    if (!t9Mode) return false;
    return matchT9Keywords(queryWords, keywordsSet);
}

function matchNormalKeywords(queryWords: string[], keywordsSet: Set<string>) {
    for (const queryWord of queryWords) {
        // const found = setHas(keywordsSet, (v) => v.includes(queryWord));
        const found = setHas(keywordsSet, (v) => v.startsWith(queryWord));
        if (found === undefined) {
            return false;
        }
        keywordsSet.delete(found);
    }
    return true;
}

function matchT9Keywords(queryWords: string[], keywordsSet: Set<string>) {
    if (queryWords.length === 0) return false;
    const qw = queryWords[0];
    if (!qw.match(/^[0-9]+$/)) return false;
    // If match to original qw with 0's, use that.
    if (setHas(keywordsSet, (v) => isT9match(v, qw))) {
        return true;
    }
    if (qw.startsWith("0") || qw.endsWith("0")) {
        // id 0 in any end, do not use 0 as space
        return false;
    }
    // split from 0's and trim
    let qw1 = qw.replace(/0+/, " ");
    qw1 = qw1.trim();
    queryWords = qw1.split(" ");
    for (const queryWord of queryWords) {
        // const found = setHas(keywordsSet, (v) => v.includes(queryWord));
        const found = setHas(keywordsSet, (v) => isT9match(v, queryWord));
        if (found === undefined) {
            return false;
        }
        keywordsSet.delete(found);
    }
    return true;
}

const T9CHARS: string[] = [
    "0", // 0
    "1", // 1
    "2abcäå", // 2
    "3def", // 3
    "4ghi", // 4
    "5jkl", // 5
    "6mnoö", // 6
    "7pqrs", // 7
    "8tuvü", // 8
    "9wxyz", // 9
];

function isT9match(word: string, nums: string): boolean {
    if (word.length < nums.length) return false;
    for (let i = 0; i < nums.length; i++) {
        const ti: number = +nums[i];
        if (!T9CHARS[ti].includes(word[i])) return false;
    }
    return true;
}

function setHas<T>(
    set: Set<T>,
    fn: (t: T) => boolean,
    defaultValue: T | undefined = undefined
) {
    for (const val of set) {
        if (fn(val)) return val;
    }
    return defaultValue;
}

function splitIntoWordSets(fields: string[]) {
    return fields.map((f) => f.toLowerCase().trim().split(/\s/));
}
