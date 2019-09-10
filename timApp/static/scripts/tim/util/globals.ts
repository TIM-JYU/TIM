import {IBookmarkGroup} from "../bookmark/bookmarks";
import {IDocSettings} from "../document/IDocSettings";
import {EditMode} from "../document/popupMenu";
import {DocumentOrFolder, IDocument, IFolder, IFullDocument, IItem} from "../item/IItem";
import {ILecture} from "../lecture/lecturetypes";
import {HeaderIndexItem, IGroupWithSisuPath} from "../sidebar/sidebarMenuCtrl";
import {IFullUser, IGroup, IUser, IUserListEntry} from "../user/IUser";
import {INotification, ISettings} from "../user/settingsCtrl";

export interface IGenericGlobals {
    IS_TESTING: boolean;
    current_user: IFullUser;
    other_users: IUser[];
    bookmarks: IBookmarkGroup[];
    ANGULARMODULES: unknown[];
    JSMODULES: string[];
    item?: DocumentOrFolder;
    userPrefs: ISettings;
}

export interface IItemGlobals extends IGenericGlobals {
    breadcrumbs: unknown[];
    item: DocumentOrFolder;
}

export interface IFolderGlobals extends IItemGlobals {
    items: IItem[];
    item: IFolder;
}

export interface IDocumentGlobals extends IItemGlobals {
    users: IUserListEntry[];
    startIndex: number;
    docVersion: [number, number];
    item: IDocument;
    noBrowser: boolean;
    allowMove: boolean; // TODO this doesn't come from server and should be removed from globals
    group: IGroup;
    docSettings: IDocSettings;
    editMode: EditMode | null;
    hideLinks: boolean;
    hideTopButtons: boolean;
    in_lecture: boolean;
    index: HeaderIndexItem[];
    lectureMode: boolean;
    liveUpdates: number;
    memoMinutes?: string;
    noQuestionAutoNumbering: boolean;
    notifications: unknown[];
    readExpiry: string;
    reqs: {};
    showIndex: boolean;
    teacherMode: boolean;
    translations: IDocument[];
    velpMode: boolean;
    wordList: string[];
    linked_groups: IGroupWithSisuPath[] | null;
}

export interface ILectureInfoGlobals extends IDocumentGlobals {
    lecture: ILecture;
    inLecture: boolean;
}

export interface ISlideGlobals extends IDocumentGlobals {
    background_url: string;
    background_color: string;
}

export interface IManageGlobals extends IGenericGlobals {
    accessTypes: Array<{}>;
    objName: "folder" | "document";
    item: IFullDocument | IFolder;
}

export interface ISettingsGlobals extends IGenericGlobals {
    settings: ISettings;
    css_files: Array<{}>;
    notifications: INotification[];
    notificationLimit: number;
}

export type SomeGlobals =
    | IGenericGlobals
    | IFolderGlobals
    | IDocumentGlobals
    | ISlideGlobals
    | IManageGlobals
    | ISettingsGlobals
    | ILectureInfoGlobals;

export function someglobals(): SomeGlobals {
    return someGlobals();
}

export function itemglobals(): IItemGlobals {
    return someGlobals();
}

export function genericglobals(): IGenericGlobals {
    return someGlobals();
}

export function documentglobals(): IDocumentGlobals {
    return someGlobals();
}

export function slideglobals(): ISlideGlobals {
    return someGlobals();
}

export function folderglobals(): IFolderGlobals {
    return someGlobals();
}

export function manageglobals(): IManageGlobals {
    return someGlobals();
}

export function settingsglobals(): ISettingsGlobals {
    return someGlobals();
}

export function lectureinfoglobals(): ILectureInfoGlobals {
    return someGlobals();
}

function someGlobals<T extends IGenericGlobals>(): T {
    return window as unknown as T;
}
