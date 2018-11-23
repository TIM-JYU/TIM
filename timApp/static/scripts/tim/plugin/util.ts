export interface IPluginAttributes<Markup extends IGenericPluginMarkup, State> {
    markup: Markup; // not in csplugin?
    doLazy: boolean;
    anonymous: boolean;
    info: {};
    preview: boolean;
    show_result: boolean; // not in csplugin?
    state: State | null; // not in csplugin?
    targetFormat: string;
    taskID: string;
    taskIDExt: string;
    userPrint: boolean;
    // csplugin has these:
    // user_id: string
    // review: boolean
}

// Attributes that are valid for all plugins.
export interface IGenericPluginMarkup {
    answerLimit?: number;
    button?: string;
    buttonText?: string;
    lazy?: boolean;
    header?: string;
    footer?: string;
    resetText?: string;
    stem?: string;
}
