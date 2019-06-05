interface IMacros {
    dates?: Array<[string, string, string]>;
    knro?: number;
    stampformat?: string;
}

export interface IDocSettings {
    macros?: IMacros;
    custom_stamp_model?: string;
    course_main?: string;
}
