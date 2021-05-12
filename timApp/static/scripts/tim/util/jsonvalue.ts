/* eslint-disable @typescript-eslint/consistent-indexed-object-style */
import {ReadonlyMoment} from "tim/util/readonlymoment";

export type JsonValue =
    | number
    | string
    | boolean
    | null
    | Date
    | ReadonlyMoment
    | {[key: string]: JsonValue}
    | JsonValue[]
    | undefined;
