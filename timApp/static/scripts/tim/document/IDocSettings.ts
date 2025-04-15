import type {IAnswerBrowserSettings} from "tim/plugin/attributes";
import type {ITemplateButton} from "../../../../modules/cs/js/csPlugin";

export type MeetingDateEntry = [string, string, string];

export interface ITimeLeftSettings {
    low_time_threshold: number;
    low_time_glow_period: number;
    low_time_glow_duration: number;
    sync_interval_deviation: number;
    sync_interval: number;
    low_time_sync_interval: number;
    low_time_sync_deviation: number;
}

export interface ILoginHideSettings {
    hakaLogin?: boolean;
    emailLogin?: boolean;
    signup?: boolean;
    passwordRecovery?: boolean;
}

export interface ILoginSettings {
    hide?: ILoginHideSettings;
}

export interface IDocSettings {
    show_velps?: boolean;
    force_velps?: boolean;
    autocounters?: Record<string, string>;
    custom_stamp_model?: string;
    course_main?: string;
    form_mode?: boolean;
    hideBrowser?: boolean;
    login?: ILoginSettings;
    timeLeft?: ITimeLeftSettings;
    saveTeacher?: boolean;
    show_scoreboard?: boolean;
    exam_mode?: string;
    sidemenu_initial_state?: string;
    displayViewInitialState?: boolean;
    cache?: boolean;
    peer_review?: boolean;
    peer_review_start?: string;
    peer_review_stop?: string;
    peer_review_allow_invalid?: boolean;
    max_uncollapsed_toc_items?: number;
    description?: string;
    translator?: string;
    sync_answerbrowsers?: boolean;
    answerBrowser?: IAnswerBrowserSettings;
    buttons?: string;
    mdButtons?: ITemplateButton[];
    lazyAnswers?: boolean;
    parAuthorOnlyEdit?: boolean;
    pollDocumentViewInfo?: number;
    pollDocumentViewInfoJitter?: number;
    redirectAnonymousNoRight?: string;
    disable_answer?: string;
    nonAnswerAnnotationsDisabled?: string;
}

export interface ISlideDocSettings extends IDocSettings {
    slide_revealjs_options?: Record<string, string>;
}
