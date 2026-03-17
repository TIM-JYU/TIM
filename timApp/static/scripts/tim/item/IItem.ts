import type {Moment} from "moment";
import moment from "moment";
import {to} from "tim/util/utils";
import type {IChangelogEntry} from "tim/document/editing/IChangelogEntry";
import type {IRights} from "tim/user/IRights";
import type {IGroup} from "tim/user/IUser";
import {$http} from "tim/util/ngimport";

export interface IDocument extends IItem {
    isFolder: false;
    src_docid?: number;
    lang_id?: string;
}

export interface IFullDocument extends IDocument {
    fulltext: string;
    errors?: string;
    versions: Array<IChangelogEntry>;
}

export type DocumentOrFolder = IDocument | IFolder;

export interface IItem {
    id: number;
    location: string;
    modified: string; // TODO change type to Moment
    name: string;
    owners: IGroup[];
    path: string;
    public: boolean;
    relevance?: IRelevance;
    rights: IRights;
    title: string;
    unpublished: boolean;
    isFolder: boolean;
}

export interface IRelevance {
    block_id: number;
    relevance: number;
}

export interface ITag {
    block_id: number;
    expires?: Moment;
    type: TagType;
    name: string;
}

export interface IFolder extends IItem {
    isFolder: true;
}

export enum TagType {
    Regular = 1,
    CourseCode = 2,
    Subject = 3,
}

export interface ICourseSettings {
    course_subjects?: ISubjectList;
}

// A list of course subjects.
export type ISubjectList = string[];

export interface ITaggedItem extends IItem {
    tags: ITag[];
}

export function isTaggedItem(item: IItem): item is ITaggedItem {
    return (item as ITaggedItem).tags !== undefined;
}

export async function getItem(itemId: number) {
    const i = await to($http.get<IItem>(`/items/${itemId}`));
    if (!i.ok) {
        return;
    }
    return i.result.data;
}

/**
 * Check if item is root folder.
 * @param item Item to check.
 * @returns {boolean} True if root folder.
 */
export function isRootFolder(item: IItem) {
    return item.id === -1;
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

/**
 * Set tag css style classes depending on the tag type. Currently normal tags are light blue-green
 * special tags green, selected tag red with borders and expired tags fainter colored..
 * @param {ITag} tag The tag.
 * @param {boolean} selected Whether the tag is selected tag.
 * @returns {string} String containing style classes.
 */
export function tagStyleClass(tag: ITag, selected: boolean) {
    let opacity = "";
    let highlight = "";
    let color = "btn-success";
    if (tagIsExpired(tag)) {
        opacity = "less-opacity";
    }
    if (tag.type === TagType.Regular) {
        color = "btn-primary";
    }
    if (selected) {
        color = "btn-danger";
        highlight = "selected-tag";
    }
    return color + " " + opacity + " " + highlight;
}

export function getViewUrl(data: IItem) {
    return "/view/" + data.path;
}

export function redirectToItem(data: IItem) {
    location.href = getViewUrl(data);
}

export interface ITranslation extends IDocument {
    lang_id: string;
    lang_name: string | null;
}

export interface IEditableTranslation extends ITranslation {
    old_langid: string;
    old_title: string;
}

export function getItemTypeName(i: DocumentOrFolder) {
    return i.isFolder ? "folder" : "document";
}

export interface ILanguage {
    name: string;
    code: string;
}

export interface ITranslator {
    name: string;
    available: boolean;
}

export interface ITranslatorUsage {
    character_count: number;
    character_limit: number;
}
