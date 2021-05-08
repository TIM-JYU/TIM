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
        maxMatches: number
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
        searchStrings: (string | undefined | null)[];
    }[] = [];
    allFields: string[] = [];

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

        this.allUsers = result.users.map((userResult) => ({
            userResult,
            searchStrings: [
                userResult.user.name,
                userResult.user.real_name,
                userResult.user.email,
                ...Object.values(userResult.fields).map((field) =>
                    field?.toString().toLowerCase()
                ),
            ],
        }));
        this.allFields = result.fieldNames;
    }

    searchUser(queryStrings: string[], maxMatches: number) {
        // Workaround for "no await in async method"
        return new Promise<Result<SearchResult, {errorMessage: string}>>(
            (accept) => accept(this.searchUserImpl(queryStrings, maxMatches))
        );
    }

    private searchUserImpl(
        queryStrings: string[],
        maxMatches: number
    ): Result<SearchResult, {errorMessage: string}> {
        queryStrings = queryStrings.map((s) => s.trim().toLowerCase());

        let matches = this.allUsers
            .filter((u) =>
                u.searchStrings.some((valueToCheck) =>
                    queryStrings.some((s) => valueToCheck?.includes(s))
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
