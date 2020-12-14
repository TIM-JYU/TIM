import {IBookmarkGroup} from "tim/bookmark/bookmark.service";
import {HeaderIndexItem} from "tim/sidebarmenu/services/header-indexer.service";
import {IDocScoreInfo} from "tim/sidebarmenu/services/scoreboard.service";
import {IDocSettings} from "../document/IDocSettings";
import {EditMode} from "../document/popup-menu-dialog.component";
import {IViewRange, IViewRangeUnnamed} from "../document/viewRangeInfo";
import {
    DocumentOrFolder,
    IDocument,
    IFolder,
    IFullDocument,
    IItem,
    ITranslation,
} from "../item/IItem";
import {ILecture} from "../lecture/lecturetypes";
import {
    IFullUser,
    IGroup,
    IGroupWithSisuPath,
    IUser,
    IUserListEntry,
} from "../user/IUser";
import {ICssFile, INotification, ISettings} from "../user/settings.component";

interface ILayout {
    col_1_lg: number;
    col_2_lg: number;
    col_3_lg: number;
    col_m_lg: number;
    col_1_md: number;
    col_2_md: number;
    col_3_md: number;
    col_m_md: number;
    col_1_sm: number;
    col_2_sm: number;
    col_3_sm: number;
    col_m_sm: number;
    col_1_xs: number;
    col_2_xs: number;
    col_3_xs: number;
    col_m_xs: number;
}

interface IHostConfig {
    allowed: string[];
    defaultwarning: string;
    warnings: Record<string, string>;
}

interface IConfig {
    gitLastestCommitTimestamp: string;
    helpEmail: string;
    gitBranch: string;
    hakaEnabled: boolean;
    emailRegistrationEnabled: boolean;
    hosts?: IHostConfig;
}

export type Locale = "fi" | "en-US";

export interface IGenericGlobals {
    IS_TESTING: boolean;
    current_user: IFullUser;
    locale: Locale;
    other_users: IUser[];
    bookmarks: IBookmarkGroup[] | null;
    ANGULARMODULES: unknown[];
    JSMODULES: string[];
    curr_item?: DocumentOrFolder;
    userPrefs: ISettings;
    homeOrganization: string;
    config: IConfig;
    layout: ILayout;
    lectureInfo: {in_lecture: boolean; is_lecturer: boolean};
}

export interface IItemGlobals extends IGenericGlobals {
    breadcrumbs: IFolder[];
    curr_item: DocumentOrFolder;
}

export interface IFolderGlobals extends IItemGlobals {
    items: IItem[];
    curr_item: IFolder;
}

export interface IDocumentGlobals extends IItemGlobals {
    parsOnly: boolean;
    users: IUserListEntry[];
    startIndex: number;
    docVersion: [number, number];
    curr_item: IDocument;
    noBrowser: boolean;
    allowMove: boolean; // TODO this doesn't come from server and should be removed from globals
    group: IGroup;
    docSettings: IDocSettings;
    editMode: EditMode | null;
    hideLinks: boolean;
    hideTopButtons: boolean;
    index: HeaderIndexItem[];
    lectureMode: boolean;
    liveUpdates: number;
    memoMinutes?: string;
    noQuestionAutoNumbering: boolean;
    notifications: unknown[];
    readExpiry: string;
    reqs: Record<string, unknown>; // TODO proper type
    showIndex: boolean;
    teacherMode: boolean;
    translations: ITranslation[];
    velpMode: boolean;
    wordList: string[];
    linked_groups: IGroupWithSisuPath[] | null;
    current_view_range?: IViewRangeUnnamed | null;
    nav_ranges?: IViewRange[];
    exam_mode: boolean;
    hide_sidemenu: boolean;
    score_infos: IDocScoreInfo[] | null;
    current_list_user?: IUser;
    show_unpublished_bg: boolean;
}

export interface ILectureInfoGlobals extends IDocumentGlobals {
    lecture: ILecture<string>;
    inLecture: boolean;
}

export interface ISlideGlobals extends IDocumentGlobals {
    background_url: string;
    background_color: string;
}

export interface IManageGlobals extends IGenericGlobals {
    orgs: IGroup[];
    accessTypes: Array<unknown>; // TODO proper type
    curr_item: IFullDocument | IFolder;
}

export interface ISettingsGlobals extends IGenericGlobals {
    settings: ISettings;
    css_files: Array<ICssFile>;
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

export function isDocumentGlobals(g: SomeGlobals): g is IDocumentGlobals {
    return "docSettings" in g;
}

function someGlobals<T extends IGenericGlobals>(): T {
    return (window as unknown) as T;
}
