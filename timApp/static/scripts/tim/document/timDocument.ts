import $ from "jquery";
import moment from "moment";
import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {DerefOption} from "tim/document/structure/derefOption";
import {maybeDeref} from "tim/document/structure/maybeDeref";
import {enumDocParts, PreambleIteration} from "tim/document/structure/parsing";
import {ReadonlyWeakMap} from "tim/util/types/readonlyWeakMap";
import {documentglobals} from "../util/globals";

type ParMapValue = [
    prev: Paragraph | undefined,
    curr: Paragraph,
    next: Paragraph | undefined
];

export class TimDocument {
    getSections() {
        return this.sections;
    }

    private sections = new WeakMap<Element, ParContext[]>();
    private sectionList: ParContext[][] = [];
    private parMap = new WeakMap<Element, ParMapValue>();

    constructor(private root: HTMLElement) {}

    /**
     * Rebuilds the sections and refreshes the section read marks.
     */
    public rebuildSections() {
        $(this.root).find(".readsection").remove();
        this.buildSections();
        this.refreshSectionReadMarks();
    }

    public getParMap(): ReadonlyWeakMap<Element, ParMapValue> {
        return this.parMap;
    }

    /**
     * Hides any visible .readline marks
     */
    public hideReadMarks() {
        $(this.root).find(".readline").attr("class", "readline read");
    }

    public print() {
        let s = "";
        for (const p of enumDocParts(PreambleIteration.Include, this.root)) {
            s += p.format("") + "\n";
        }
        console.log(s);
    }

    /**
     * Refreshes the section read marks.
     */
    public refreshSectionReadMarks() {
        $(this.root).find(".readsection").remove();
        for (const sectionPars of this.sectionList) {
            const readlines: JQuery<Element> = $(
                sectionPars
                    .map((p) => p.getReadline())
                    .filter((r): r is Element => r != null)
            );
            const modifiedCount = readlines
                .filter(".read-modified")
                .not(".read").length;
            const unreadCount = readlines.not(".read-modified").not(".read")
                .length;
            if (modifiedCount + unreadCount > 0) {
                const div = document.createElement("div");
                div.className = "readsection";
                div.title = `Mark preceding section as read (${sectionPars.length} paragraphs - ${unreadCount} unread, ${modifiedCount} modified)`;
                const i1 = document.createElement("i");
                i1.className = "glyphicon glyphicon-align-left";
                const i2 = document.createElement("i");
                i2.className = "glyphicon glyphicon-ok";
                div.appendChild(i1);
                div.appendChild(i2);
                sectionPars[sectionPars.length - 1].par.htmlElement.appendChild(
                    div
                );
            }
        }
    }

    /**
     * Builds a dictionary of sections that maps the last paragraph id of each section to the section paragraphs.
     */
    public buildSections() {
        this.sections = new WeakMap();
        this.sectionList = [];
        this.parMap = new WeakMap();
        const allpars = enumDocParts(PreambleIteration.Include, this.root);
        let currentSection: ParContext[] = [];
        let prev: Element | undefined;

        const addSection = () => {
            if (prev && currentSection.length > 0) {
                this.sections.set(prev, currentSection);
                this.sectionList.push(currentSection);
            }
        };

        let prev1;
        let prev2;
        for (const p of allpars) {
            const iter = p.enumPars(DerefOption.Deref);
            for (const x of iter) {
                if (prev1) {
                    this.parMap.set(prev1.htmlElement, [prev2, prev1, x]);
                }

                prev2 = prev1;
                prev1 = x;
                if (x.parent) {
                    const parent = maybeDeref(x.parent);
                    if (parent instanceof Area) {
                        if (!parent.collapse && x.equals(parent.startPar.par)) {
                            continue;
                        }
                        if (x.equals(parent.endPar.par)) {
                            continue;
                        }
                    }
                }
                if (x.htmlElement.querySelector("h1, h2, h3")) {
                    addSection();
                    currentSection = [];
                }
                if (x.isSetting()) {
                    continue;
                }
                currentSection.push(new ParContext(x));
                prev = x.htmlElement;
            }
            if (prev1) {
                this.parMap.set(prev1.htmlElement, [prev2, prev1, undefined]);
            }
        }
        addSection();
    }

    public readExpiry() {
        return moment.duration(documentglobals().readExpiry);
    }
}
