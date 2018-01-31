export interface IParResponse {
    texts: string;
    version: [number, number];
}

export interface IExtraData {
    "ref-id"?: string;
    par: string;
    area_start?: string;
    area_end?: string;
}

export type Duplicate = [string, string] | [string, string, "hasAnswers"];
