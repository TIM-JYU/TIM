export type MeetingDateEntry = [string, string, string];

interface IMacros {
    dates?: Array<MeetingDateEntry>;
    knro?: number;
    stampformat?: string;
}

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
    macros?: IMacros;
    custom_stamp_model?: string;
    course_main?: string;
    form_mode?: boolean;
    hideBrowser?: boolean;
    login?: ILoginSettings;
    timeLeft?: ITimeLeftSettings;
    save_teacher?: boolean;
    show_scoreboard?: boolean;
    exam_mode?: string;
    cache?: boolean;
}
