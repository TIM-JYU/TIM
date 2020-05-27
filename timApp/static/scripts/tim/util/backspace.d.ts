declare module "backspace-disabler" {
    interface IBackspaceDisabler {
        enable(): void;
        disable(): void;
    }

    declare const backspaceDisabler: IBackspaceDisabler;
    export = backspaceDisabler;
}
