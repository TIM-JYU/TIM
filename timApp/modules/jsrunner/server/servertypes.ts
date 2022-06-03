import * as t from "io-ts";
import {nullable} from "tim/plugin/attributes";
import {JsrunnerMarkup} from "../shared/jsrunnertypes";

export const UserFieldData = t.type({
    fields: t.record(t.string, t.unknown),
    user: t.type({
        id: t.Int,
        name: t.string,
        real_name: nullable(t.string),
    }),
    groupinfo: t.type({
        membership_end: nullable(t.number),
        membership_add: nullable(t.number),
    }),
});

export const User = t.interface({
    id: t.Int,
    name: t.string,
});

export const VelpData = t.intersection([
    t.type({
        points: nullable(t.Int),
        annotator: t.type({
            id: t.Int,
            name: t.string,
            real_name: nullable(t.string),
        }),
        answer: nullable(
            t.type({
                id: t.Int,
                users: t.array(User),
                task_id: t.string,
            })
        ),
    }),
    t.partial({
        groupinfo: t.type({
            membership_end: t.number,
        }),
    }),
]);

export const PeerReviewData = t.type({
    id: t.Int,
    block_id: t.Int,
    reviewer_id: t.Int,
    reviewable_id: t.Int,
    reviewed: t.boolean,
    answer_id: nullable(t.Int),
    task_name: nullable(t.string),
});

// export type UserFields = t.type({id: t.Int, fields: t.string});

export type UserFieldDataT = t.TypeOf<typeof UserFieldData>;

export const AliasData = t.record(t.string, t.string);

export type AliasDataT = t.TypeOf<typeof AliasData>;

export type VelpDataT = t.TypeOf<typeof VelpData>;

export type PeerReviewDataT = t.TypeOf<typeof PeerReviewData>;

export const JsrunnerAnswer = t.type({
    markup: JsrunnerMarkup,
    input: t.type({
        data: t.array(UserFieldData),
        aliases: AliasData,
        testvelps: t.array(VelpData),
        peerreviews: t.array(PeerReviewData),
    }),
    taskID: t.string,
});
