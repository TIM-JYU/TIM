export type MeetingDateEntry = [string, string, string];

interface IMacros {
    dates?: Array<MeetingDateEntry>;
    knro?: number;
    stampformat?: string;
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
    login?: ILoginSettings;
}
