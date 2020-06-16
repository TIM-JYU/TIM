import $ from "jquery";
import moment from "moment";
import {documentglobals} from "../util/globals";
import {getParAttributes, getParId, getRefAttrs, Paragraph} from "./parhelpers";

type SectionMap = Map<string, JQuery[]>;

export class Document {
    getSections(): SectionMap {
        return this.sections;
    }

    getId(): number {
        return this.id;
    }

    private sections = new Map<string, JQuery[]>();
    private id: number;

    constructor(id: number) {
        this.id = id;
    }

    /**
     * Rebuilds the sections and refreshes the section read marks.
     */
    public rebuildSections() {
        $(".readsection").remove();
        this.sections = new Map();
        this.buildSections([], $("#pars"));
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
        for (const sectionPars of this.sections.values()) {
            const readlines = $(sectionPars.map((p) => p[0])).children(".readline");
            const modifiedCount = readlines.filter(".read-modified").not(".read").length;
            const unreadCount = readlines.not(".read-modified").not(".read").length;
            if (modifiedCount + unreadCount > 0) {
                sectionPars[sectionPars.length - 1].append($("<div>", {
                    class: "readsection",
                    title: "Mark preceding section as read (" +
                        sectionPars.length + " paragraphs - " + unreadCount +
                        " unread, " + modifiedCount + " modified)",
                }).html('<i class="glyphicon glyphicon-align-left"></i><i class="glyphicon glyphicon-ok"></i>'));
            }
        }
    }

    /**
     * Builds a dictionary of sections that maps the last paragraph id of each section to the section paragraphs.
     *
     * @param currentSectionPars The collection of paragraphs in the current section being processed.
     * @param container The container element where the paragraphs are located.
     * @returns The collection of paragraphs in the current section being processed.
     */
    private buildSections(currentSectionPars: Paragraph[], container: JQuery): JQuery[] {
        let child = container.children(".par:first");
        while (child.length > 0) {
            if (child.hasClass("area")) {
                currentSectionPars = this.buildSections(currentSectionPars, child.find(".areaContent"));
            } else if (child.hasClass("par")) {
                const attrs = getParAttributes(child);
                const refAttrs = getRefAttrs(child)["ref-attrs"] as Record<string, string>;
                const content = child.children(".parContent");
                if (content.is(":visible")) {
                    if (content.children("h1, h2, h3").length > 0) {
                        if (currentSectionPars.length > 0) {
                            const parId = getParId(currentSectionPars[currentSectionPars.length - 1])!;
                            this.sections.set(parId, currentSectionPars);
                        }
                        currentSectionPars = [child];
                    } else if (!attrs.hasOwnProperty("settings") && !attrs.hasOwnProperty("area") && !attrs.hasOwnProperty("area_end") && !refAttrs.hasOwnProperty("area") && !refAttrs.hasOwnProperty("area_end")) {
                        currentSectionPars.push(child);
                    }
                }
            } else if (child.hasClass("addBottomContainer")) {
                if (currentSectionPars.length > 0) {
                    this.sections.set(getParId(currentSectionPars[currentSectionPars.length - 1])!, currentSectionPars);
                }
            }
            child = child.next();
        }
        return currentSectionPars;
    }

    public readExpiry() {
        return moment.duration(documentglobals().readExpiry);
    }
}

