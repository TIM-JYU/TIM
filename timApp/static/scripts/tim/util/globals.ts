import type {IBookmarkGroup} from "tim/bookmark/bookmark.service";
import type {HeaderIndexItem} from "tim/sidebarmenu/services/header-indexer.service";
import type {IDocScoreInfo} from "tim/sidebarmenu/services/scoreboard.service";
import type {
    IDocSettings,
    ISlideDocSettings,
    MeetingDateEntry,
} from "tim/document/IDocSettings";
import type {EditMode} from "tim/document/popup-menu-dialog.component";
import type {IViewRange, IViewRangeUnnamed} from "tim/document/viewRangeInfo";
import type {
    DocumentOrFolder,
    IDocument,
    IFolder,
    IFullDocument,
    IItem,
    ITranslation,
} from "tim/item/IItem";
import type {ILecture} from "tim/lecture/lecturetypes";
import type {
    ICurrentUser,
    IGroup,
    IGroupWithSisuPath,
    IUser,
    IUserContact,
    IUserListEntry,
} from "tim/user/IUser";

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
    minPasswordLength: number;
    simpleLoginUseStudyInfoMessage: boolean;
    simpleLoginCustomLoginMessage: string | null;
    gitLatestCommitTimestamp: string;
    helpEmail: string;
    gitBranch: string;
    hakaEnabled: boolean;
    emailRegistrationEnabled: boolean;
    passwordResetEnabled: boolean;
    simpleEmailLogin: boolean;
    hosts?: IHostConfig;
    messageListsEnabled: boolean;
}

export type Locale = "fi" | "sv" | "en-US";

export interface IFooterDocs {
    accessibilityStatement?: string;
    privacyNotice?: string;
    termsOfService?: string;
}

export interface IGenericGlobals {
    IS_TESTING: boolean;
    current_user: ICurrentUser;
    locale: Locale;
    other_users: IUser[];
    restoreContextUser: string | null;
    bookmarks: IBookmarkGroup[] | null;
    ANGULARMODULES: unknown[];
    JSMODULES: string[];
    curr_item?: DocumentOrFolder;
    userPrefs: ISettings;
    homeOrganization: string;
    config: IConfig;
    layout: ILayout;
    footerDocs: IFooterDocs;
    lectureInfo: {in_lecture: boolean; is_lecturer: boolean};
}

export interface IErrorGlobals extends IGenericGlobals {
    errorCode: number;
    errorStatus: string;
}

export interface IItemGlobals extends IGenericGlobals {
    breadcrumbs: IFolder[];
    curr_item: DocumentOrFolder;
}

export interface IFolderGlobals extends IItemGlobals {
    items: IItem[];
    curr_item: IFolder;
}

export interface IMeetingMemoSettings {
    dates?: Array<MeetingDateEntry>;
    knro?: number;
    stampformat?: string;
}

export interface IDocumentGlobals extends IItemGlobals {
    parsOnly: boolean;
    users: IUserListEntry[];
    startIndex: number;
    docVersion: [number, number];
    curr_item: IDocument;
    noBrowser: boolean;
    allowMove: boolean; // TODO this doesn't come from server and should be removed from globals
    groups?: [string];
    docSettings: IDocSettings;
    memoMinutesSettings?: IMeetingMemoSettings;
    editMode: EditMode | null;
    hideLinks: boolean;
    hideTopButtons: boolean;
    index: HeaderIndexItem[];
    lectureMode: boolean;
    liveUpdates: number;
    memoMinutes?: string;
    showReviewTab: boolean; // needs functionality
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
    requires_login?: boolean;
    showValidAnswersOnly: boolean;
    hideNamesRequested: boolean;
    useLoginCodes: boolean;
    loginMessage: string | null;
}

export enum NotificationType {
    DocModified = 1,
    ParAdded = 2,
    ParModified = 3,
    ParDeleted = 4,
    CommentAdded = 5,
    CommentModified = 6,
    CommentDeleted = 7,
    AnswerAdded = 8,
    AnnotationAdded = 9,
    AnnotationModified = 10,
    AnnotationDeleted = 11,
}

export interface INotification {
    type: NotificationType;
    item: DocumentOrFolder;
}

export interface ICssFile {
    name: string;
    desc: string;
}

export interface ISettings {
    custom_css: string;
    disable_menu_hover: boolean;
    remember_last_sidebar_menu_tab: boolean;
    remember_last_sidebar_menu_state: boolean;
    email_exclude: string;
    language: string | null;
    use_document_word_list: boolean;
    word_list: string;
    auto_mark_all_read: boolean;
    max_uncollapsed_toc_items: number | null;
    style_doc_ids: number[];
    quick_select_style_doc_ids: number[];
    parmenu_position: number;
    always_show_header_menu: boolean;
}

export interface ILectureInfoGlobals extends IDocumentGlobals {
    lecture: ILecture<string>;
    inLecture: boolean;
}

export interface ISlideGlobals extends IDocumentGlobals {
    background_url: string;
    background_color: string;
    themes: string[];
    slide_size: [number, number];
    docSettings: ISlideDocSettings;
}

export interface IManageGlobals extends IGenericGlobals {
    orgs: IGroup[];
    accessTypes: Array<unknown>; // TODO proper type
    curr_item: IFullDocument | IFolder;
}

export interface IOAuthGlobals extends IGenericGlobals {
    oauthClientName: string;
    oauthScopes: string[];
}

export interface ISettingsGlobals extends IGenericGlobals {
    settings: ISettings;
    notifications: INotification[];
    notificationLimit: number;
    contacts: IUserContact[];
}

// See VerificationType on verification.py for details.
export enum VerificationType {
    LIST_JOIN = "list",
    CONTACT_OWNERSHIP = "contact",
    SET_PRIMARY_CONTACT = "set_primary_contact",
}

export interface IVerificationGlobals extends IGenericGlobals {
    verifyType: VerificationType;
    verifyInfo?: {type: string};
    verifyError?: string;
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

export function oauthglobals(): IOAuthGlobals {
    return someGlobals();
}

export function settingsglobals(): ISettingsGlobals {
    return someGlobals();
}

export function lectureinfoglobals(): ILectureInfoGlobals {
    return someGlobals();
}

export function verificationglobals(): IVerificationGlobals {
    return someGlobals();
}

export function isDocumentGlobals(g: SomeGlobals): g is IDocumentGlobals {
    return "docSettings" in g;
}

export function isErrorGlobals(g: SomeGlobals): g is IErrorGlobals {
    return "errorCode" in g;
}

function someGlobals<T extends IGenericGlobals>(): T {
    return window as unknown as T;
}
