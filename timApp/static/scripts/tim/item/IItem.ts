import moment, {Moment} from "moment";
import {IRights} from "../user/IRights";
import {$http} from "../util/ngimport";

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
    expires?: Moment;
    type: TagType;
    name: string;
}

export enum TagType {
    Regular = 1,
    CourseCode = 2,
    Subject = 3,
}

export interface ICourseSettings {
    course_subjects: ISubjectList;
}

// A list of course subjects. May contain lists within lists.
export type ISubjectList = string | { [subject: string]: ISubjectList[] };

export interface ITaggedItem extends IItem {
    tags: ITag[];
}

export async function getItem(itemId: number) {
    return (await $http.get<IItem>(`/items/${itemId}`)).data;
}

/**
 * Returns course code if it exists for the item.
 * @param {ITag[]} tags A list of tags.
 * @param {boolean} checkExpiration If true, expired courses will be return as undefined.
 * @returns {string} The course code as a string or undefined if none was found.
 */
export function getCourseCode(tags: ITag[], checkExpiration: boolean = false) {
    for (const tag of tags) {
        if (tag.type === TagType.CourseCode) {
            if (checkExpiration && tagIsExpired(tag)) {
                    return undefined;
            }
            return tag.name;
        }
    }
    return undefined;
}

/**
 * Checks if the tag has expired.
 * @param {ITag} tag
 * @returns {boolean} False if the tag has no expiration or hasn't yet expired.
 */
export function tagIsExpired(tag: ITag) {
    if (tag.expires) {
        if (tag.expires.diff(moment.now()) < 0) {
            return true;
        }
    } else {
        return false;
    }
}
