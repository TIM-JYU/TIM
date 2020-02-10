function getClosestParentWithSelectorOrOptional(elem: Element | null, selector?: string, optional?: string) {
    // Find closest upward parent that matches either selector or optional selector.
    // if both are empty, return document

    if (!selector && !optional) {return document; }

    // Get the closest matching element
    for (; elem; elem = elem.parentElement) {
        if (selector && elem.matches(selector)) {return elem; }
        if (optional && elem.matches(optional)) {return elem; }
    }
    return null;
}

// noinspection JSUnusedGlobalSymbols
export function findParentElementFromScript(scriptId: string, parentTopSelector: string, parentDivSelector: string) {
    // scriptId: id for script where to start
    // parentTopSelector : string with top selector to find element where to start looking inputs
    // parentDivSelector: string used as a selector with parentTopSelector to find what element comes first
    //               when scanning up from current script context
    try {
        let parentElem;
        let elem: HTMLElement | null = document.scripts[document.scripts.length - 1]; // document.currentScript does not work???
        if (!elem) {return document.querySelector(parentDivSelector) ?? document; }
        if (scriptId && scriptId != elem.id) {elem = document.getElementById(scriptId); }
        if (!elem) {return document.querySelector(parentDivSelector) ?? document; }
        return getClosestParentWithSelectorOrOptional(elem, parentTopSelector, parentDivSelector) ?? document;
    } catch (ex) {
        return document;
    }
}


interface IOptions {
    // List of inputs whose values are sent in first contact
    sendInputs?: string;
    // Object whose value is sent on first contact
    initObject?: unknown;
}

type CustomInput = HTMLInputElement & {servname: string};
type CustomChange = Event & {port: MessagePort, target: CustomInput};

interface InputObj {input: HTMLInputElement, lastValue: unknown, servname: string}

interface ICommand {
    cmd: string;
    name: string;
    value: unknown;
}

/**
 * A communication object for iframe that is already on the document.
 */
export class ServerSyncValues {
    private readonly parentElem: Element;
    private readonly dname: string;
    private readonly inputs: Record<string, InputObj | undefined>;
    private readonly prefix: string;
    private readonly channel: MessageChannel;
    private readonly jsxFrame: HTMLIFrameElement;
    private readonly debugElem?: HTMLInputElement | null;

    /**
     *
     * @param parentElement element that is used to find inputs
     * @param iframeSelector selector to find iframe
     * @param prefix prefix for inputs // TODO: make search more general?
     * @param debugName name (id) for possible debug input to show debug results
     * @param dname string to prefix debug texts like S1
     * @param opts values to be sent on first contact
     */
    constructor(parentElement: Element, iframeSelector: string, prefix: string, debugName: string, dname: string, opts?: IOptions) {
        this.parentElem = parentElement;
        this.dname = dname;
        this.prefix = prefix;
        this.inputs = {};
        const options = opts ?? {};

        this.jsxFrame = this.parentElem.querySelector(iframeSelector) as HTMLIFrameElement;
        if (debugName) {this.debugElem = this.parentElem.querySelector(debugName) as HTMLInputElement; }
        this.channel = new MessageChannel();

        this.debug("Start port initialize " + parentElement.id + " " + iframeSelector, "i");

        const values: Record<string, unknown> = {};
        if (options.sendInputs) {
            const names = options.sendInputs.split(",");
            for (const n of names) {
                const name = n.trim();
                const inp = this.getInput(name);
                values[name] = inp ? this.getValue(inp) : "";
            }
        }
        const init = {
            values: values,
            initObject: options.initObject,
        };

        this.channel.port1.onmessage = (e) => {
            this.debug(JSON.stringify(e.data), "r");
            const retCmd = [];
            for (const cmd of e.data as ICommand[]) {
                const name = cmd.name;
                const inp = this.getInput(name);
                if (!inp) {continue; }
                switch (cmd.cmd) {
                    case "get":
                        const value = this.getValue(inp);
                        retCmd.push({cmd: "set", name: name, value: value});
                        break;
                    case "set":
                        const val = JSON.stringify(cmd.value);
                        if (inp.input.value === val) {return; }
                        inp.input.value = val;
                        const ev = new Event("change") as CustomChange;
                        ev.port = this.channel.port1;
                        inp.input.dispatchEvent(ev);
                        break;
                    case "setVisibility":
                        inp.input.style.display = cmd.value as string;
                        break;
                }
            }
            if (retCmd.length) {this.send(retCmd); }
        };

        this.jsxFrame.addEventListener("load", () => {
            this.debug(iframeSelector + " loaded", "i");
            this.jsxFrame.contentWindow!.postMessage(init, "*", [this.channel.port2]);
        });
    }

    debug(s: string, dir: string) {
        if (!this.debugElem) {return; }
        console.log(this.dname + " " + dir + ":" + s);
        if (dir === "r") {this.debugElem.value = s; }
    }

    send(obj: ICommand[]) {
        this.channel.port1.postMessage(obj);
        this.debug(JSON.stringify(obj), "s");
    }

    getValue(inp: InputObj) {
        const value = inp.input.value;
        if (!value) {return ""; }
        try {
            return JSON.parse(inp.input.value);
        } catch (e) {
            return value;
        }
    }

    onChange(e: CustomChange) {
        try {
            const inp = this.getInput(e.target.servname);
            if (!inp) {
                return;
            }
            const val = this.getValue(inp);
            this.debug(inp.servname + " = " + val, "c");

            if (val === inp.lastValue) {return; }
            if (e.port == this.channel.port1) {return; } /* ei samaan kanavaan takaisin */
            this.send([{cmd: "set", name: inp.servname, value: val}]);
            inp.lastValue = val;
        } catch (err) {
            return;
        }
    }

    getInput(name: string) {
        let inp = this.inputs[name];
        if (inp) {return inp; }
        const input = this.parentElem.querySelector<CustomInput>("#" + this.prefix + name);
        if (!input) {return null; }
        inp = {input: input, lastValue: "", servname: name};
        this.inputs[name] = inp;
        input.servname = name;
        input.addEventListener("input", (e) => this.onChange(e as CustomChange));
        input.addEventListener("change", (e) => this.onChange(e as CustomChange));
        return inp;
    }

    sendValues() {
        const retCmd = [];
        for (const [name, inp] of Object.entries(this.inputs)) {
            const value = JSON.parse(inp!.input.value);
            retCmd.push({cmd: "set", name: name, value: value});
        }
        if (retCmd.length) {this.send(retCmd); }
    }
}
