import * as t from "io-ts";

const Rights = t.type({
    browse_own_answers: t.boolean,
    can_comment: t.boolean,
    copy: t.boolean,
    editable: t.boolean,
    manage: t.boolean,
    owner: t.boolean,
    see_answers: t.boolean,
    teacher: t.boolean,
    restricted_mode: t.boolean,
});

export const RightNames = t.keyof(Rights.props);

export interface IRights extends t.TypeOf<typeof Rights> {}
