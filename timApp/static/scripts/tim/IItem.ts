import {Moment} from "moment";
import {IRights} from "./IRights";
import {$http} from "./ngimport";

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

export interface ITag {
    block_id: number;
    expires: Moment;
    tag: string;
}

export interface ITaggedItem extends IItem {
    tags: ITag[];
}

export async function getItem(itemId: number) {
    return (await $http.get<IItem>(`/items/${itemId}`)).data;
}
