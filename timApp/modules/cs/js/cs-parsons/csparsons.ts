import $ from "jquery";
import {shuffleStrings} from "tim/plugin/util";
import "./jquery-ui-sortable.min.js";
import "./jquery.ui.touch-punch.min.js";

interface CsParsonsOptions {
    shuffle: boolean;
    sortable: Element;
    notordermatters: boolean;
    words: boolean;
    maxcheck: number | undefined;
    separator?: string;
    minWidth: string;
    styleWords: string;

    onChange?(widget: CsParsonsWidget): void;
}

export class CsParsonsWidget {
    options: CsParsonsOptions;
    text: string = "";
    lines: string[] = [];

    constructor(options: CsParsonsOptions) {
        const defaults = {
            separator: "\n",
            styleWords: "",
        };

        this.options = {
            ...defaults,
            ...options,
        };
    }

    init(text: string, userText: string) {
        this.text = text;
        if (this.options.shuffle) {
            this.lines = text.split("\n");
            this.lines = shuffleStrings(this.lines);
        } else {
            this.lines = userText.split("\n");
        }
    }

    show() {
        let classes = "sortable-code sortable-output";
        const parsonsEditDiv = this.options.sortable;
        let type = "div";
        if (this.options.words) {
            classes += " sortable-words ";
            this.options.separator = " ";
            type = "span";
        }
        const words = this.lines;
        parsonsEditDiv.innerHTML = "";
        for (let i = 0; i < words.length; i++) {
            let div;
            let w = words[i];
            if (this.options.words && w === "") {
                div = document.createElement("div");
                w = "\\n";
            } else {
                div = document.createElement(type);
                if (this.options.words && this.options.minWidth && w.length < 3) {
                    div.setAttribute("style", "width: " + this.options.minWidth);
                }
            }
            div.setAttribute("class", "sortitem");

            const t = document.createTextNode(w);
            div.appendChild(t);
            // var div2 = document.createElement(typ);
            // div2.appendChild(div);
            parsonsEditDiv.appendChild(div);
            if (this.options.maxcheck && this.options.maxcheck == i + 1) {
                div = document.createElement(type);
                div.className = "parsonsstatic";
                parsonsEditDiv.appendChild(div);
            }
        }
        parsonsEditDiv.setAttribute("class", classes);
        parsonsEditDiv.setAttribute("style", "float: left; width: 100%" + ";" + this.options.styleWords);
        const a = $(parsonsEditDiv);
        if (this.options.maxcheck) {
            a.sortable({
                items: ":not(.parsonsstatic)",
                start: function() {
                    $(".parsonsstatic", this).each(function() {
                        const thisJq = $(this);
                        thisJq.data("pos", thisJq.index());
                    });
                },
                change: function(this: Element) {
                    const sortable = $(this);
                    const statics = $(".parsonsstatic", this).detach();
                    const helper = $("<" + type + "></" + type + ">").prependTo(this);
                    statics.each(function() {
                        const thisJq = $(this);
                        const target = thisJq.data("pos");

                        thisJq.insertAfter($(type, sortable).eq(target as number));
                    });
                    helper.remove();
                },
            });
        } else {
            a.sortable();
        }
        // a.sortable( "option", "axis", "x" );
        a.on("sortstop", (event, ui) => {
            if (this.options.onChange) {
                this.options.onChange(this);
            }
            this.clear();
        });
    }

    join(separator: string) {
        let result = "";
        let sep = "";
        const div = this.options.sortable;
        let nosep = false;
        if (!div) {
            return "";
        }
        for (let i = 0; i < div.childElementCount; i++) {
            const node = div.childNodes[i];
            let line = node.textContent;
            if (line === "") {
                continue;
            } // separatorline
            if (line === "\\n") {
                line = "\n";
                sep = "";
                nosep = true;
            }
            result += sep + line;
            sep = separator;
            if (nosep) {
                sep = "";
            }
            nosep = false;
        }

        return result;
    }

    check(userText: string) {
        const result = "";
        const div = this.options.sortable;
        const lines = this.text.split("\n");
        const ulines = userText.split("\n");
        if (!div) {
            return "";
        }
        let maxn = div.childElementCount;
        if (this.options.maxcheck && this.options.maxcheck < div.childElementCount) {
            maxn = this.options.maxcheck;
        }
        for (let i = 0; i < maxn; i++) {
            const node = div.children[i];
            let nodeok = true;
            if (this.options.notordermatters) {
                nodeok = false;
                for (let j = 0; j < maxn; j++) {
                    if (lines[j] === ulines[i]) {
                        lines[j] = "XXXXXXXXXXXXXX";
                        nodeok = true;
                        break;
                    }
                }
            } else {
                if (lines[i] !== ulines[i]) {
                    nodeok = false;
                }
            }
            if (!nodeok) {
                node.setAttribute("style", "background-color: RED;");
            } else if (this.options.maxcheck) {
                node.setAttribute("style", "background-color: LIGHTGREEN;");
            }
        }

        return result;
    }

    clear() {
        const div = this.options.sortable;
        if (!div) {
            return "";
        }
        for (let i = 0; i < div.childElementCount; i++) {
            const node = div.children[i];
            node.removeAttribute("style");
        }
    }
}
