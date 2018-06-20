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
    type: TagType;
    name: string;
}

export enum TagType {
    Regular = 1,
    CourseCode = 2,
    Subject = 3,
}

export interface ITaggedItem extends IItem {
    tags: ITag[];
}

export async function getItem(itemId: number) {
    return (await $http.get<IItem>(`/items/${itemId}`)).data;
}
