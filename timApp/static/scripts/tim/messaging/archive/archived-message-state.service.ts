import {Injectable} from "@angular/core";
import * as t from "io-ts";
import {isRight} from "fp-ts/Either";
import {HttpClient} from "@angular/common/http";
import {documentglobals} from "tim/util/globals";
import {DateFromString, nullable, withDefault} from "../../plugin/attributes";
import {to2} from "../../util/utils";

const Address = t.type({
    name: withDefault(nullable(t.string), null),
    email: t.string,
});

const ArchivedMessageData = t.type({
    sender: Address,
    recipients: t.array(Address),
    date: DateFromString,
});

export interface DocLink {
    title: string;
    path: string;
}

export interface SiblingMessages {
    prev?: DocLink;
    next?: DocLink;
}

@Injectable()
export class ArchivedMessageStateService {
    messageData?: t.TypeOf<typeof ArchivedMessageData>;
    private siblingMessages?: SiblingMessages;
    private relatedMessageInfoPromise?: Promise<SiblingMessages>;

    constructor(private http: HttpClient) {}

    initState(state: string) {
        if (this.messageData) {
            return;
        }
        const rawData = JSON.parse(state);
        const res = ArchivedMessageData.decode(rawData);
        if (isRight(res)) {
            this.messageData = res.right;
        }
    }

    getRelatedMessages(): Promise<SiblingMessages> {
        if (this.siblingMessages) {
            return new Promise<SiblingMessages>((r) =>
                r(this.siblingMessages!)
            );
        }
        if (!this.relatedMessageInfoPromise) {
            this.relatedMessageInfoPromise = this.resolveRelatedMessageInfo();
        }
        return this.relatedMessageInfoPromise;
    }

    private async resolveRelatedMessageInfo(): Promise<SiblingMessages> {
        const doc = documentglobals().curr_item.id;
        const result = await to2(
            this.http
                .get<{next?: DocLink; prev?: DocLink}>(
                    `/messagelist/archive/siblings/${doc}`
                )
                .toPromise()
        );
        if (result.ok) {
            return result.result;
        }
        return {next: undefined, prev: undefined};
    }
}
