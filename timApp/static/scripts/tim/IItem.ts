import {IRights} from "./IRights";
export interface IItem {
    id: number;
    name: string;
    location: string;
    title: string;
    isFolder: boolean;
    fulltext: string;
    rights: IRights;
    versions: {}[];
    path: string;
}
