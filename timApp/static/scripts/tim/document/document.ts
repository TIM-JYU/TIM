import $ from "jquery";
import moment from "moment";
import {Paragraph} from "tim/document/structure/paragraph";
import {Area} from "tim/document/structure/area";
import {ReferenceParagraph} from "tim/document/structure/referenceParagraph";
import {BrokenArea} from "tim/document/structure/brokenArea";
import {ParContext} from "tim/document/structure/parContext";
import {
    enumDocParts,
    PreambleIteration,
} from "tim/document/structure/iteration";
import {documentglobals} from "../util/globals";

function* getInnerPars(p: Paragraph | Area | BrokenArea) {
    if (p instanceof Paragraph) {
        yield p;
    } else if (p instanceof BrokenArea) {
        yield* p.inner;
    } else {
        if (p.collapse) {
            yield p.startPar.par;
        }
        yield* p.inner;
    }
}

export class Document {
    getSections() {
        return this.sections;
    }

    getId(): number {
        return this.id;
    }

    private sections = new WeakMap<Element, ParContext[]>();
    private sectionList: ParContext[][] = [];

    constructor(private id: number) {}

    /**
     * Rebuilds the sections and refreshes the section read marks.
     */
    public rebuildSections() {
        $(".readsection").remove();
        this.buildSections();
        this.refreshSectionReadMarks();
    }

    /**
     * Hides any visible .readline marks
     */
    public hideReadMarks() {
        $(".readline").attr("class", "readline read");
    }

    /**
     * Refreshes the section read marks.
     */
    public refreshSectionReadMarks() {
        $(".readsection").remove();
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
    private buildSections() {
        this.sections = new WeakMap();
        this.sectionList = [];
        const allpars = enumDocParts(PreambleIteration.Include);
        let currentSection: ParContext[] = [];
        let prev: Element | undefined;

        const addSection = () => {
            if (prev && currentSection.length > 0) {
                this.sections.set(prev, currentSection);
                this.sectionList.push(currentSection);
            }
        };

        for (const p of allpars) {
            const iter =
                p instanceof ReferenceParagraph
                    ? getInnerPars(p.target)
                    : getInnerPars(p);
            for (const x of iter) {
                if (x.htmlElement.querySelector("h1, h2, h3")) {
                    addSection();
                    currentSection = [];
                }
                if (x.isSetting()) {
                    continue;
                }
                currentSection.push(new ParContext(x, p));
                prev = x.htmlElement;
            }
        }
        addSection();
    }

    public readExpiry() {
        return moment.duration(documentglobals().readExpiry);
    }
}
