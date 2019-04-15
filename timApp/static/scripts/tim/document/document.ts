import moment from "moment";
import {$window} from "../util/ngimport";
import {getParAttributes, getParId, getRefAttrs, Paragraph} from "./parhelpers";

type SectionMap = Map<string, JQuery[]>;

export class Document {
    get sections(): SectionMap {
        return this._sections;
    }

    get id(): number {
        return this._id;
    }

    private _sections = new Map<string, JQuery[]>();
    private _id: number;

    constructor(id: number) {
        this._id = id;
    }

    /**
     * Rebuilds the sections and refreshes the section read marks.
     */
    public rebuildSections() {
        $(".readsection").remove();
        this._sections = new Map();
        this.buildSections([], $("#pars"));
        this.refreshSectionReadMarks();
    }

    /**
     * Refreshes the section read marks.
     */
    public refreshSectionReadMarks() {
        $(".readsection").remove();
        for (const sectionPars of this._sections.values()) {
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
                const refAttrs = getRefAttrs(child)["ref-attrs"];
                const content = child.children(".parContent");
                if (content.is(":visible")) {
                    if (content.children("h1, h2, h3").length > 0) {
                        if (currentSectionPars.length > 0) {
                            const parId = getParId(currentSectionPars[currentSectionPars.length - 1])!;
                            this._sections.set(parId, currentSectionPars);
                        }
                        currentSectionPars = [child];
                    } else if (!attrs.hasOwnProperty("settings") && !attrs.hasOwnProperty("area") && !attrs.hasOwnProperty("area_end") && !refAttrs.hasOwnProperty("area") && !refAttrs.hasOwnProperty("area_end")) {
                        currentSectionPars.push(child);
                    }
                }
            } else if (child.hasClass("addBottomContainer")) {
                if (currentSectionPars.length > 0) {
                    this._sections.set(getParId(currentSectionPars[currentSectionPars.length - 1])!, currentSectionPars);
                }
            }
            child = child.next();
        }
        return currentSectionPars;
    }

    public readExpiry() {
        return moment.duration($window.readExpiry);
    }
}

let activeDocument: Document | null = null;

export function setActiveDocument(d: Document) {
    activeDocument = d;
}

export function getActiveDocument(): Document {
    if (activeDocument == null) {
        throw new Error("Active document was null; getActiveDocument was probably called before setActiveDocument was called");
    }
    return activeDocument;
}
