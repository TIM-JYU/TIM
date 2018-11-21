export interface IPluginAttributes<Markup extends IGenericPluginMarkup, State> {
    markup: Markup;
    doLazy: boolean;
    anonymous: boolean;
    info: {};
    preview: boolean;
    show_result: boolean;
    state: State | null;
    targetFormat: string;
    taskID: string;
    taskIDExt: string;
    userPrint: boolean;
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
