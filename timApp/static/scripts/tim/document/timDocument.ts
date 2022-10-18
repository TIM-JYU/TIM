import $ from "jquery";
import moment from "moment";
import type {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ParContext} from "tim/document/structure/parContext";
import {DerefOption} from "tim/document/structure/derefOption";
import {maybeDeref} from "tim/document/structure/maybeDeref";
import {enumDocParts, PreambleIteration} from "tim/document/structure/parsing";
import type {ReadonlyWeakMap} from "tim/util/types/readonlyWeakMap";
import type {NotRemovableHTMLElement} from "tim/document/structure/notRemovableElement";
import {documentglobals} from "tim/util/globals";

type ParMapValue = [
    prev: Paragraph | undefined,
    curr: Paragraph,
    next: Paragraph | undefined
];

export class TimDocument {
    private sections = new WeakMap<Element, ParContext[]>();
    private sectionsByLastAreas = new WeakMap<Area, ParContext[]>();
    private sectionList: ParContext[][] = [];
    private parMap = new WeakMap<Element, ParMapValue>();
    private uncollapsibleAreaMap = new WeakMap<Element, Area>();

    constructor(private root: HTMLElement) {}

    getSections() {
        return this.sections;
    }

    getRoot() {
        return this.root;
    }

    getSectionFor(par: Paragraph): ParContext[] | undefined {
        const pars = this.sections.get(par.htmlElement);
        if (pars) {
            return pars;
        }
        if (par.parent) {
            const parent = maybeDeref(par.parent);
            if (parent instanceof Area) {
                return this.sectionsByLastAreas.get(parent);
            }
        }
        return undefined;
    }

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

    public getUncollapsibleAreaMap(): ReadonlyWeakMap<Element, Area> {
        return this.uncollapsibleAreaMap;
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
            const unreadCount = readlines
                .not(".read-modified")
                .not(".read").length;

            if (modifiedCount + unreadCount > 0) {
                const info = {
                    all: sectionPars.length,
                    unread: unreadCount,
                    modified: modifiedCount,
                };
                const lastPar = sectionPars[sectionPars.length - 1].par;
                this.addSectionReadMark(lastPar.htmlElement, info);
                if (lastPar.parent) {
                    const parent = maybeDeref(lastPar.parent);
                    // Collapsible area => section mark was applied to area and paragraph
                    // Add an additional mark to area start (visible only when area is collapsed)
                    if (parent instanceof Area && parent.collapse) {
                        this.addSectionReadMark(
                            parent.collapse.titlepar.par.htmlElement,
                            info
                        );
                    }
                }
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
        this.sectionsByLastAreas = new WeakMap();
        this.uncollapsibleAreaMap = new WeakMap();
        const allpars = enumDocParts(PreambleIteration.Include, this.root);
        let currentSection: ParContext[] = [];
        let prev: Element | undefined;
        let prevArea: Area | undefined;

        const addSection = () => {
            if (prev && currentSection.length > 0) {
                this.sections.set(prev, currentSection);
                this.sectionList.push(currentSection);
                if (prevArea) {
                    this.sectionsByLastAreas.set(prevArea, currentSection);
                }
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
                let isInArea = false;
                if (x.parent) {
                    const parent = maybeDeref(x.parent);
                    if (parent instanceof Area) {
                        prevArea = parent;
                        isInArea = true;
                        if (!parent.collapse) {
                            this.uncollapsibleAreaMap.set(
                                parent.getAreaContainer(),
                                parent
                            );
                        }
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
                if (!isInArea) {
                    prevArea = undefined;
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

    private addSectionReadMark(
        target: NotRemovableHTMLElement,
        parCount: {all: number; unread: number; modified: number}
    ) {
        const div = document.createElement("div");
        div.className = "readsection";
        div.title = `Mark preceding section as read (${parCount.all} paragraphs - ${parCount.unread} unread, ${parCount.modified} modified)`;
        const i1 = document.createElement("i");
        i1.className = "glyphicon glyphicon-align-left";
        const i2 = document.createElement("i");
        i2.className = "glyphicon glyphicon-ok";
        div.appendChild(i1);
        div.appendChild(i2);
        target.appendChild(div);
    }

    public readExpiry() {
        return moment.duration(documentglobals().readExpiry);
    }
}
