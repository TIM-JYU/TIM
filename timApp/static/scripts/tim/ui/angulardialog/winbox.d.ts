declare module "winbox/src/js/winbox" {
    export as namespace WinBox;

    interface WinBox {
        dom: HTMLElement;
        background: string;
        body: HTMLElement;
        border: string | number;

        class: string | string[];

        html: string;
        id: string | number;
        index: number;
        readonly max: boolean;
        readonly min: boolean;
        readonly full: boolean;
        readonly hidden: boolean;
        readonly focused: boolean;
        modal: boolean;
        root: Node;
        title: string;
        url: string;

        x: number;
        y: number;
        width: number;
        height: number;
        top: number;
        left: number;
        right: number;
        bottom: number;
        minwidth: number;
        minheight: number;
        maxwidth: number;
        maxheight: number;

        onfocus: (this: WinBox) => void;
        onblur: (this: WinBox) => void;
        onresize: (this: WinBox, width: number, height: number) => void;
        onmove: (this: WinBox, x: number, y: number) => void;
        onclose: (this: WinBox, force: boolean) => boolean;
        mount(src?: Element): WinBox;
        unmount(dest?: Element): WinBox;
        setTitle(title: string): WinBox;
        setBackground(background: string): WinBox;
        setUrl(url: string): WinBox;
        focus(): WinBox;
        hide(): WinBox;
        show(): WinBox;
        minimize(state?: boolean): WinBox;
        maximize(state?: boolean): WinBox;
        fullscreen(state?: boolean): WinBox;
        close(force?: boolean): boolean | void;
        move(
            x?: string | number,
            y?: string | number,
            skipUpdate?: boolean
        ): WinBox;
        resize(
            w?: string | number,
            h?: string | number,
            skipUpdate?: boolean
        ): WinBox;
        addClass(classname: string): WinBox;
        removeClass(classname: string): WinBox;
    }
    declare namespace WinBox {
        interface WinBoxConstructor {
            (title: string, params?: Params): WinBox;
            (params: Params): WinBox;
            new (title: string, params?: Params): WinBox;
            new (params: Params): WinBox;
        }

        interface Params {
            autosize?: boolean;
            background?: string | undefined;
            body?: HTMLElement | undefined;
            border?: string | number | undefined;
            bottom?: string | number | undefined;
            class?: string | string[] | undefined;
            height?: string | number | undefined;
            html?: string | undefined;
            id?: string | number | undefined;
            index?: number | undefined;
            left?: string | number | undefined;
            max?: boolean | undefined;
            minheight?: string | number | undefined;
            minwidth?: string | number | undefined;
            modal?: boolean | undefined;
            mount?: Node | undefined;
            right?: string | number | undefined;
            root?: Node | undefined;
            splitscreen?: boolean | undefined;
            title?: string | undefined;
            top?: string | number | undefined;
            url?: string | undefined;
            width?: string | number | undefined;
            x?: "right" | "center" | string | number | undefined;
            y?: "bottom" | "center" | string | number | undefined;
            onclose?: ((this: WinBox, force?: boolean) => boolean) | undefined;
            onfocus?: ((this: WinBox) => void) | undefined;
            onblur?: ((this: WinBox) => void) | undefined;
            onresize?:
                | ((this: WinBox, width: number, height: number) => void)
                | undefined;
            onmove?: ((this: WinBox, x: number, y: number) => void) | undefined;
            onmaximize?: ((this: WinBox) => void) | undefined;
            onminimize?: ((this: WinBox) => void) | undefined;
            onrestore?: ((this: WinBox) => void) | undefined;
        }
    }

    declare const WinBox: WinBox.WinBoxConstructor & {
        new:
            | ((title: string, params?: WinBox.Params) => WinBox)
            | ((params: WinBox.Params) => WinBox);
    };

    export = WinBox;
}
