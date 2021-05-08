import {HttpClient, HttpParams} from "@angular/common/http";
import {Result, to2} from "../../util/utils";
import {IUser} from "../../user/IUser";
import {Paragraph} from "../../document/structure/paragraph";

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
        queryStrings: string[]
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
    allUsers: UserResult[] = [];

    constructor(
        private http: HttpClient,
        private par: {doc_id: number; par_id: string}
    ) {}

    async initialize() {
        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        this.allUsers = await this.http
            .get<UserResult[]>("/userSelect/fetchUsers", {
                params: {
                    doc_id: this.par.doc_id.toString(),
                    par_id: this.par.par_id,
                    ...params,
                },
            })
            .toPromise();
    }

    searchUser(queryStrings: string[]) {
        // Workaround for "no await in async method"
        return new Promise<Result<SearchResult, {errorMessage: string}>>(() =>
            this.searchUserImpl(queryStrings)
        );
    }

    private searchUserImpl(
        queryStrings: string[]
    ): Result<SearchResult, {errorMessage: string}> {
        return {ok: false, result: {errorMessage: "wew"}};
    }
}
