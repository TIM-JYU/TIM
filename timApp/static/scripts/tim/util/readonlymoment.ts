import {Moment, MomentInput, unitOfTime} from "moment";

/**
 * Represents an immutable Moment object. Not perfect, but prevents some bugs.
 */
export type ReadonlyMoment = Omit<Moment, "add" | "subtract" | "set"> & {
    isSame(m: ReadonlyMoment | MomentInput): boolean;
    diff(
        b: ReadonlyMoment | MomentInput,
        unitOfTime?: unitOfTime.Diff,
        precise?: boolean
    ): number;
};
