import {ReadonlyMoment} from "tim/util/readonlymoment";

export type IChangelogEntry = {
    group: string;
    op: "Added" | "Deleted" | "Modified" | "Inserted";
    op_params: null; // TODO
    par_id: string;
    time: ReadonlyMoment;
    ver: [number, number];
};
