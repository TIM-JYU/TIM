import angular, {IScope} from "angular";
import $ from "jquery";
import {IAnswer} from "../answer/IAnswer";
import {addElementToParagraphMargin} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";
import {IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {documentglobals} from "../util/globals";
import {$compile, $http} from "../util/ngimport";
import {angularWait, assertIsText, checkIfElement, getElementParent, stringOrNull, to} from "../util/utils";
import {VelpSelectionController} from "./velpSelection";
import {IAnnotation, IAnnotationCoordless, IAnnotationInterval, isFullCoord, IVelp, IVelpUI} from "./velptypes";

/**
 * The controller handles the logic related to adding and removing annotations. It also handles the way how
 * the selected area is defined in the view. Requires `velpSelection` directive.
 *
 * @module reviewController
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

const illegalClasses = ["annotation-element", "highlighted", "editorArea", "previewcontent"];

/**
 * A class for handling annotations.
 */
export class ReviewController {
    private annotationsAdded: boolean;
    private selectedArea?: Range;
    public selectedElement?: Element;
    public item: IItem;
    private annotations: IAnnotation[];
    private annotationids: {[i: number]: number};
    public zIndex: number;
    private scope: IScope;
    private velpBadge?: HTMLElementTagNameMap["input"];
    private velpBadgePar?: string;
    private velpSelection!: VelpSelectionController; // initialized through onInit
    public velpMode: boolean;
    public velps?: IVelpUI[];

    constructor(public vctrl: ViewCtrl) {
        this.scope = vctrl.scope;
        this.annotationsAdded = false;
        this.velpMode = documentglobals().velpMode;
        this.item = documentglobals().curr_item;
        this.annotations = [];
        this.annotationids = {0: 0};
        this.zIndex = 1;
    }

    initVelpSelection(velpSelection: VelpSelectionController) {
        this.velpSelection = velpSelection;
    }

    /**
     * Loads the document annotations into the view.
     */
    async loadDocumentAnnotations() {
        const response = await to($http.get<IAnnotation[]>(`/${this.item.id}/get_annotations`));
        if (!response.ok) {
            return;
        }
        this.annotations = response.result.data;
        const annotationsToRemove = [];

        for (const a of this.annotations) {

            const placeInfo = a.coord;
            const parent = document.getElementById(placeInfo.start.par_id);

            if (parent == null) {
                // TODO: Decide what to do, when parent element has been deleted, for now remove annotation from list
                annotationsToRemove.push(a);
                continue;
            }

            if (a.answer_id != null) {
                if (!parent.classList.contains("has-annotation")) {
                    parent.classList.add("has-annotation");
                }
                continue;
            }

            if (parent.getAttribute("t") === placeInfo.start.t && isFullCoord(placeInfo.start) && isFullCoord(placeInfo.end)) {

                try {
                    let elements = parent.querySelector(".parContent");
                    if (elements == null) {
                        this.addAnnotationToElement(
                            parent,
                            a,
                            false,
                            "Could not show annotation in correct place");
                        return;
                    }

                    const startElpath = placeInfo.start.el_path;

                    for (const p of startElpath) {
                        const elementChildren = this.getElementChildren(elements);
                        if (elementChildren[p] != null) {
                            elements = elementChildren[p];
                        }
                    }

                    const startel = elements.childNodes[placeInfo.start.node];
                    const endel = elements.childNodes[placeInfo.end.node];

                    const range = document.createRange();
                    range.setStart(startel, placeInfo.start.offset);
                    range.setEnd(endel, placeInfo.end.offset);
                    this.addAnnotationToCoord(range, a, false);
                    this.addAnnotationToElement(parent, a, false, "Added also margin annotation");
                } catch (err) {
                    this.addAnnotationToElement(parent, a, false, "Could not show annotation in correct place");
                }
            } else {
                this.addAnnotationToElement(parent, a, false, "Paragraph has been modified");
            }
        }

        for (const r of annotationsToRemove) {
            const index = this.annotations.indexOf(r);
            this.annotations.splice(index, 1);
        }

        this.annotationsAdded = true;
    }

    /**
     * Gets the children (not childNodes) of the element.
     * @param element - Element whose children are requested
     * @returns {Array} Element children
     */
    getElementChildren(element: Node): Element[] {
        /* if (typeof element.children !== "undefined")
         return element.children;
         */
        const children = [];
        for (const childNode of element.childNodes) {
            if (checkIfElement(childNode)) {
                children.push(childNode);
            }
        }
        return children;
    }

    /**
     * Gets element parent element until given attribute is present.
     * @param element - Element whose parent is queried for
     * @param attribute - Attribute as a string
     * @returns {Element} First element that has the given attribute
     */
    getElementParentUntilAttribute(element: Node, attribute: string): Element | null {
        let e = getElementParent(element);
        while (e && !e.hasAttribute(attribute)) {
            e = getElementParent(e);
        }
        return e;
    }

    getFirstChildUntilNull(element: Node): Node {
        if (element.firstChild == null) {
            return element;
        }
        return this.getFirstChildUntilNull(element.firstChild);
    }

    /**
     * Get last inner last child of the element.
     * @param element
     * @returns Element
     */
    getLastChildUntilNull(element: Node): Node {
        if (element.lastChild == null) {
            return element;
        }
        return this.getFirstChildUntilNull(element.lastChild);
    }

    /**
     * Checks if the given node is an annotation or not.
     * @param node - Element to check
     * @returns {boolean} - Whether the node is an annotation or not
     */
    checkIfAnnotation(node: Node): node is Element {
        if (node.nodeName === "ANNOTATION") {
            return true;
        }
        return false;
    }

    /**
     * Loads the annotations to the given answer.
     * @param answerId - Answer ID
     * @param par - Paragraph element
     * @param showInPlace - show velp inside answer text
     */
    loadAnnotationsToAnswer(answerId: number, par: Element, showInPlace: boolean): void {
        const annotations = this.getAnnotationsByAnswerId(answerId);

        const oldAnnotations = par.querySelectorAll(".notes [aid]");
        angular.element(oldAnnotations).remove();

        for (const a of annotations) {
            const placeInfo = a.coord;

            const preElem = par.getElementsByTagName("PRE")[0];
            if (!preElem) {
                continue;
            }
            const element = preElem.firstChild;

            if (!showInPlace || !isFullCoord(placeInfo.start) || !isFullCoord(placeInfo.end)) {
                this.addAnnotationToElement(par, a, false, "Added as margin annotation");
            } else if (element) {
                const range = document.createRange();
                try {
                    range.setStart(element, placeInfo.start.offset);
                    range.setEnd(element, placeInfo.end.offset);
                } catch (e) {
                    // catch: Failed to execute 'setStart' on 'Range': The offset XX is larger than the node's length (YY).
                }
                this.addAnnotationToCoord(range, a, false);
                this.addAnnotationToElement(par, a, false, "Added also margin annotation");
            } else {
                showMessageDialog("Couldn't add annotations");
            }
        }
    }

    /**
     * Gets all the annotations with a given answer ID.
     * @param id - Answer ID
     * @returns {Array} Annotations of the answer
     */
    getAnnotationsByAnswerId(id: number): IAnnotation[] {
        const annotations: IAnnotation[] = [];
        this.annotations.forEach((a) => {
            if (a.answer_id != null && a.answer_id === id) {
                annotations.push(a);
            }
        });

        annotations.sort((a, b) => (b.coord.start.offset || 0) - (a.coord.start.offset || 0));

        return annotations;
    }

    /**
     * Gets all the annotations with a given answer ID.
     * @param id - annotation ID
     * @returns annotation with this id
     */
    getAnnotationById(id: number) {
        for (const a of this.annotations) {
            if (a.id === id) {
                return a;
            }
        }
        return undefined;
    }

    /**
     * Adds an annotation to the given element in a given coordinate.
     * @param range - Annotation coordinate
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     */
    addAnnotationToCoord(range: Range, annotation: IAnnotationCoordless, show: boolean): void {
        const span = this.createPopOverElement(annotation, show);
        try {
            range.surroundContents(span);
        } catch (err) {
            this.addAnnotationToElement(span, annotation, true, "Annotation crosses taglines");
            this.selectedArea = undefined;

            /*
             var new_range = document.createRange();

             var el = range.startContainer;
             var start = range.startOffset;
             var end = range.startContainer.parentNode.innerHTML.length - 1;

             new_range.setStart(el, start);
             new_range.setEnd(el, end);
             this.addAnnotationToCoord(new_range, annotation, show);
             */
        }
        $compile(span)(this.scope);
    }

    /**
     * Adds an annotation to the given element. The annotation will be placed in the margin.
     * @param el - Given element
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     * @param reason - The reason why the annotation is put here (not implemented yet)
     */
    addAnnotationToElement(el: Element, annotation: IAnnotationCoordless, show: boolean, reason: string): void {
        annotation.reason = reason;
        const span = this.createPopOverElement(annotation, show);
        const text = document.createTextNode("\u00A0" + annotation.content + "\u00A0");
        span.appendChild(text);
        addElementToParagraphMargin(el, span);

        $compile(span)(this.scope);
    }

    /**
     * Creates the velp badge button (the button with letter 'V' on it).
     */
    createVelpBadge(par: string): HTMLElementTagNameMap["input"] {
        this.velpBadgePar = par;
        if (this.velpBadge) {
            // $compile(this.velpBadge)(this);
            return this.velpBadge;
        }
        const btn = document.createElement("input");
        btn.type = "button";
        btn.classList.add("velp-badge");
        btn.classList.add("timButton");
        btn.value = "V";
        btn.id = "velpBadge";
        btn.onclick = (e) => {
            this.clearVelpBadge(e);
        };
        // btn.setAttribute("ng-click", "clearVelpBadge($event)");
        // $compile(btn)(this);
        this.velpBadge = btn;
        return btn;
    }

    /**
     * Moves the velp badge to the correct element.
     * @param oldElement - Element where the badge was
     * @param newElement - Element where the badge needs to be attached
     */
    updateVelpBadge(oldElement: Element | null, newElement: Element | null): void {
        if (newElement == null) {
            return;
        } else if (oldElement == null) {
            addElementToParagraphMargin(newElement, this.createVelpBadge(newElement.id));
        } else if (oldElement.id !== newElement.id) {
            this.clearVelpBadge(null);
            addElementToParagraphMargin(newElement, this.createVelpBadge(newElement.id));
        }
    }

    /**
     * Removes the velp badge and clears the element selection.
     * @param e - Current click event
     */
    clearVelpBadge(e: Event | null): void {
        const btn = this.velpBadge;
        if (btn) {
            $(btn).remove();
        }

        if (e != null) {
            this.selectedElement = undefined;
            this.selectedArea = undefined;
            this.velpSelection.updateVelpList();
            e.stopPropagation();
        }
    }

    /**
     * Returns the real ID of an annotation.
     * @param id - Annotation ID generated by the client
     * @returns {int} Annotation ID generated by the server
     */
    getRealAnnotationId(id: number): number {
        if (id < 0) {
            return this.annotationids[id];
        }
        return id;
    }

    /**
     * Deletes the given annotation.
     * @param id - Annotation ID
     */
    async deleteAnnotation(id: number): Promise<void> {
        const annotationParents = document.querySelectorAll(`[aid="${id}"]`);
        const annotationHighlights = annotationParents[0].getElementsByClassName("highlighted");

        if (annotationParents.length > 1) {
            let savedHTML = "";
            for (const a of annotationHighlights) {
                let addHTML = a.innerHTML.replace('<span class="ng-scope">', "");
                addHTML = addHTML.replace("</span>", "");
                savedHTML += addHTML;
            }
            annotationParents[0].outerHTML = savedHTML;
            $(annotationParents[1]).remove();
        } else {
            $(annotationParents[0]).remove();
        }

        for (let a = 0; a < this.annotations.length; a++) {
            if (id === this.annotations[a].id) {
                this.annotations.splice(a, 1);
            }
        }

        const currentId = this.getRealAnnotationId(id);

        await to($http.post("/invalidate_annotation", {annotation_id: currentId}));
    }

    /**
     * Updates annotation data.
     * @param id - Annotation ID
     * @param inmargin - Whether the annotation is to be placed in the margin or not
     */
    updateAnnotation(id: number, inmargin: boolean): void {
        const annotationParents = document.querySelectorAll(`[aid="${id}"]`);
        const annotationElement = $(`[aid="${id}"]`);
        const par = annotationElement.parents(".par");
        const annotationHighlights = annotationElement[0].getElementsByClassName("highlighted");
        if (!inmargin) {
            for (const a of this.annotations) {
                if (id === a.id) {
                    $(annotationElement[1]).remove();
                    this.addAnnotationToElement(par[0], a, false, "Added also margin annotation");
                    // addAnnotationToElement(this.annotations[a], false, "Added also margin annotation");
                }
            }

        } else {
            if (annotationParents.length > 1) {
                let savedHTML = "";
                for (const a of annotationHighlights) {
                    let addHTML = a.innerHTML.replace('<span class="ng-scope">', "");
                    addHTML = addHTML.replace("</span>", "");
                    savedHTML += addHTML;
                }
                annotationParents[0].outerHTML = savedHTML;

                // TODO: add redraw annotation text
            }
        }

    }

    /**
     * Changes annotation points.
     * @param id - Annotation ID
     * @param points - Annotation points
     */
    changeAnnotationPoints(id: number, points: number): void {
        for (const a of this.annotations) {
            if (a.id === id) {
                a.points = points;
                break;
            }
        }
    }

    changeAnnotationColor(id: number, color: string | null): void {
        for (const a of this.annotations) {
            if (a.id === id) {
                a.color = color;
                break;
            }
        }
    }

    /**
     * Adds a comment to the given annotation.
     * @param id - Annotation ID
     * @param name - Commenter name
     * @param comment - Content of the given comment
     */
    addComment(id: number, name: string, comment: string): void {
        for (const a of this.annotations) {
            if (a.id === id) {
                a.comments.push({
                    commenter_username: name,
                    content: comment,
                    comment_time: "now",
                    comment_relative_time: "just now",
                });
                break;
            }
        }
    }

    /**
     * Changes the visibility of an annotation. Visibility can be one of the following:
     *
     * - 1 = Myself
     * - 2 = Document owner
     * - 3 = Teacher
     * - 4 = Everyone.
     *
     * @param id - Annoation ID
     * @param visibility - Annotation visibility (1, 2, 3 or 4)
     */
    changeVisibility(id: number, visiblity: number): void {
        for (const a of this.annotations) {
            if (a.id === id) {
                a.visible_to = visiblity;
                break;
            }
        }
    }

    /**
     * Selects text range or just the element.
     * @todo When annotations can break tags, check annotations from all elements in the selection.
     */
    selectText($event: Event): void {
        if (!$event.target) {
            return;
        }
        const par = $($event.target as HTMLElement).parents(".par")[0];

        let oldElement = null;
        if (this.selectedElement != null) {
            oldElement = this.selectedElement;
        }

        try {
            let range;
            if (window.getSelection) {
                range = window.getSelection();
            } else {
                range = document.getSelection();
            }
            if (range && range.toString().length > 0) {
                this.selectedArea = range.getRangeAt(0);
                this.selectedElement = this.getElementParentUntilAttribute(this.selectedArea.startContainer, "t") || undefined;
            } else {
                this.selectedArea = undefined;
            }
        } catch (err) {
            // return;
        }

        if (this.selectedArea != null) {
            // Check if selection breaks tags, has annotation as a parent or as a child.
            if (this.isSelectionTagParentsUnequal(this.selectedArea) ||
                this.hasSelectionParentAnnotation(this.selectedArea) ||
                this.hasSelectionChildrenAnnotation(this.selectedArea)) {
                this.selectedArea = undefined;
            }
        } else {
            /*
             var elements = document.getElementsByClassName("lightselect");
             if (elements.length > 0)
             this.selectedElement = elements[0];
             */
            if (par && par.id) {
                this.selectedElement = par;
            }

        }

        const newElement = this.selectedElement;
        this.updateVelpBadge(oldElement, newElement || null);
        if (newElement != null) {
            this.velpSelection.updateVelpList();
        }
    }

    /**
     * Checks if the selection breaks HTML tags. Returns true if the tags were broken.
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the HTML tags were broken or not.
     */
    isSelectionTagParentsUnequal(range: Range): boolean {
        return getElementParent(range.startContainer) !== getElementParent(range.endContainer);
    }

    /**
     * Checks iteratively if the element has an annotation element as its parent.
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the element has an annotation element as its parent or not
     */
    hasSelectionParentAnnotation(range: Range): boolean {
        let startcont = getElementParent(range.startContainer);
        while (startcont && !startcont.hasAttribute("t")) {
            startcont = getElementParent(startcont);
            if (startcont && (this.checkIfAnnotation(startcont) || this.hasAnyIllegalClass(startcont))) {
                return true;
            }
        }

        let endcont = getElementParent(range.endContainer);
        while (endcont && !endcont.hasAttribute("t")) {
            endcont = getElementParent(endcont);
            if (endcont && this.checkIfAnnotation(endcont)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Checks if the element has any class in `illegalClasses` array.
     * @param element - Element to be checked
     * @returns {boolean} Whether illegal classes were found or not.
     */
    hasAnyIllegalClass(element: Element): boolean {
        for (const c of illegalClasses) {
            if (element.classList.contains(c)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks recursively if the selection has any annotation elements as children.
     * @param range - Range object containing the user's selection
     * @returns {boolean} Whether the selection has any annotation elements as children or not
     */
    hasSelectionChildrenAnnotation(range: Range): boolean {
        const div = document.createElement("div");
        const clone = range.cloneContents();
        div.appendChild(clone);
        const children = div.childNodes;
        for (const c of children) {
            if (this.hasElementChildrenAnnotation(c)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the element children has an annotation element.
     * @param element - Element to check
     * @returns {boolean} Whether annotation was found or not
     */
    hasElementChildrenAnnotation(element: Element | Node): boolean {

        if (this.checkIfAnnotation(element)) {
            return true;
        }

        const children = element.childNodes;

        for (const c of children) {
            if (this.hasElementChildrenAnnotation(c)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Gets the velp by its ID. If no velps are found, this method returns null.
     * @param id - Velp to be found
     * @returns {Object|null} Velp or null
     */
    getVelpById(id: number): IVelp | null {
        if (!this.velps) {
            return null;
        }

        for (const v of this.velps) {
            if (v.id === id) {
                return v;
            }
        }

        return null;
    }

    /**
     * Get marking highlight style.
     * @param points - Points given in marking
     * @returns {string} Highlight style
     */
    getAnnotationHighlight(points: number): string {
        let highlightStyle = "positive";
        if (points === 0) {
            highlightStyle = "neutral";
        } else if (points < 0) {
            highlightStyle = "negative";
        }
        return highlightStyle;
    }

    /**
     * Adds an annotation with the selected velp's data to
     * the selected text area or element.
     * @todo When the annotations can cross HTML tags, end coordinate needs to be changed according to the end element.
     * @todo Also get the paragraph element (parelement) according to endContainer.
     * @param velp - Velp selected in the `velpSelection` directive
     */
    async useVelp(velp: IVelp): Promise<void> {

        if (this.selectedElement == null && this.selectedArea == null) {
            return;
        }

        /*
         if (velp.default_comment != null && velp.default_comment.length > 0){
         comment.push({
         content: velp.default_comment,
         commenter_username: "me",
         comment_time: "now",
         comment_relative_time: "just now"
         });
         }
         */

        const newAnnotation: IAnnotationCoordless = {
            id: -(this.annotations.length + 1),
            velp: velp.id,
            points: velp.points,
            doc_id: this.item.id,
            visible_to: velp.visible_to,
            content: velp.content,
            annotator_name: "me",
            edit_access: true,
            email: "",
            timesince: "just now",
            creationtime: "now",
            color: velp.color,
            comments: [],
            default_comment: velp.default_comment,
            newannotation: true,
            user_id: -1,
            reason: null,
            answer_id: null,
        };
        let coord: IAnnotationInterval;
        if (this.selectedArea != null) {

            let parelement = getElementParent(this.selectedArea.startContainer);
            const startElement = getElementParent(this.selectedArea.startContainer);

            const innerDiv = document.createElement("div");
            const cloned = this.selectedArea.cloneContents();
            innerDiv.appendChild(cloned);

            while (parelement && !parelement.hasAttribute("t")) {
                parelement = getElementParent(parelement);
            }
            if (!parelement || !startElement) {
                showMessageDialog("Could not add annotation");
                return;
            }

            const elementPath = this.getElementPositionInTree(startElement, []);
            const answerInfo = this.getAnswerInfo(startElement);

            if (answerInfo == null) {
                newAnnotation.user_id = null;
            } else {
                newAnnotation.user_id = this.vctrl.selectedUser.id;
            }

            const startoffset = this.getRealStartOffset(this.selectedArea.startContainer, this.selectedArea.startOffset);
            let endOffset = this.selectedArea.endOffset;
            if (innerDiv.childElementCount === 0) {
                const lastChild = innerDiv.childNodes[innerDiv.childNodes.length - 1];
                if (assertIsText(lastChild)) {
                    endOffset = startoffset + lastChild.length;
                }
            }

            if (answerInfo != null) {
                newAnnotation.answer_id = answerInfo.id;
            }

            if (this.selectedElement != null) {
                this.addAnnotationToElement(this.selectedElement, newAnnotation, false, "Added also margin annotation");
            }
            this.addAnnotationToCoord(this.selectedArea, newAnnotation, true);
            await angularWait();

            const nodeNums = this.getNodeNumbers(this.selectedArea.startContainer, newAnnotation.id, innerDiv);
            coord = {
                start: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") || undefined,
                    el_path: elementPath,
                    offset: startoffset,
                    depth: elementPath.length,
                    node: nodeNums[0],
                },
                end: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") || undefined,
                    el_path: elementPath,
                    offset: endOffset,
                    depth: elementPath.length,
                    node: nodeNums[1],
                },
            };
            this.annotations.push({...newAnnotation, coord});
            this.annotationids[newAnnotation.id] = newAnnotation.id;

            this.selectedArea = undefined;

        } else if (this.selectedElement != null) {

            coord = {
                start: {
                    par_id: this.selectedElement.id,
                    t: this.selectedElement.getAttribute("t") || undefined,
                },
                end: {
                    par_id: this.selectedElement.id,
                    t: this.selectedElement.getAttribute("t") || undefined,
                },
            };

            const answerInfo = this.getAnswerInfo(this.selectedElement);

            if (answerInfo != null) {
                newAnnotation.answer_id = answerInfo.id;
            }

            this.addAnnotationToElement(this.selectedElement, newAnnotation, true, "No coordinate found");
            this.annotationids[newAnnotation.id] = newAnnotation.id;
        } else {
            throw new Error("selectedArea and selectedElement were both null");
        }

        if (!velp.used) {
            velp.used = 1;
        } else {
            velp.used += 1;
        }

        this.annotationsAdded = true;

        const json = await to($http.post<IAnnotation>("/add_annotation", {...newAnnotation, coord}));
        if (!json.ok) {
            return;
        }
        this.annotationids[newAnnotation.id] = json.result.data.id;
    }

    getAnswerBrowserFromPluginLoader(first: Element) {
        const taskId = first.getAttribute("task-id");
        if (!taskId) {
            console.warn("tim-plugin-loader did not have task-id?");
            return;
        }
        const ctrl = this.vctrl.getAnswerBrowser(taskId);
        if (!ctrl) {
            console.warn(`answerbrowser with task id ${taskId} was not found`);
            return;
        }
        return ctrl;
    }

    /**
     * Gets the answer info of the element. Returns null if no answer found.
     * @param start - Paragraph where the answerbrowser element is searched for.
     * @returns {Element|null} answerbrowser element or null.
     */
    getAnswerInfo(start: Element): IAnswer | undefined {

        if (start.hasAttribute("attrs") && start.hasAttribute("t")) {
            const answ = start.getElementsByTagName("tim-plugin-loader");
            if (answ.length > 0) {
                const first = answ[0];
                const isInline = first.classList.contains("inlineplugin");
                if (isInline && answ.length > 1) {
                    console.warn("Paragraph has multiple plugins but the first of them was not inlineplugin?");
                    return;
                }
                if (isInline) {
                    return;
                }
                const ab = this.getAnswerBrowserFromPluginLoader(first);
                return ab && ab.selectedAnswer;
            }
            return;
        }

        const loader = $(start).parents("tim-plugin-loader")[0];
        if (!loader) {
            return;
        }
        const ctrl = this.getAnswerBrowserFromPluginLoader(loader);
        return ctrl && ctrl.selectedAnswer;
    }

    /**
     * Gets an array of element indexes from the TIM paragraph element to the given element.
     * TIM paragraph element is defined as an element containing a 't' attribute.
     * If the given element is inside the TIM paragraph element, this method returns the following array: [0].
     * If the given element is inside the second child element of the TIM paragraph element, the following
     * array is returned: [0, 1].
     *
     * @param start - Starting element
     * @param array - Array of indexes
     * @returns {Array} Array of element indexes
     */
    getElementPositionInTree(start: Element, array: number[]): number[] {
        const myparent = getElementParent(start);
        if (myparent == null) {
            throw new Error("Element position in tree was not found (getElementParent returned null)");
        }

        if (myparent.hasAttribute("t")) {
            return array.reverse();
        }

        let count = 0;

        const children = this.getElementChildren(myparent);
        for (const c of children) {

            if (c === start) {
                array.push(count);
                return this.getElementPositionInTree(myparent, array);
            }

            if (this.checkIfAnnotation(c)) {
                const innerElements = c.getElementsByClassName("highlighted")[0];
                const innerChildren = this.getElementChildren(innerElements);
                if (innerChildren.length > 2) {
                    count += innerChildren.length - 2;
                }
                continue;
            }

            count++;
        }

        throw new Error("Element position in tree was not found");
    }

    /**
     * Get start offset according to the "original state" of DOM.
     * Ignores `annotation` elements, but not the elements inside the annotation.
     *
     * @param el - Start container
     * @param startoffset - Original start offset
     * @returns {int} Start offset according to the "original state" of the DOM.
     */
    getRealStartOffset(el: Node, startoffset: number): number {

        const startType = el.nodeName;
        let storedOffset = startoffset;

        while (el.previousSibling != null) {
            el = el.previousSibling;
            if (this.checkIfAnnotation(el)) {

                const innerElements = el.getElementsByClassName("highlighted")[0];
                const lastInnerLastChild = this.getLastChildUntilNull(innerElements);
                if (assertIsText(lastInnerLastChild)) {
                    storedOffset += lastInnerLastChild.length;
                }

                // if (typeof innerElements.lastChild.innerHTML !== UNDEFINED)
                //     storedOffset += innerElements.lastChild.innerHTML.length;
                // else storedOffset += innerElements.lastChild.length;

                if (innerElements.childNodes.length > 1) {
                    return storedOffset;
                }
            } else if (el.nodeName !== startType) {
                return storedOffset;
            } else {
                if (assertIsText(el)) {
                    storedOffset += el.length;
                }
            }
        }

        return storedOffset;
    }

    /**
     * Gets the start and end node numbers of created annotation element.
     * Ignores annotations elements, but not elements inside it.
     *
     * @param el - Start container
     * @param aid - Annotation ID
     * @param innerElement - Annotation content
     * @returns {Array} array with the start and end node numbers
     */
    getNodeNumbers(el: Node, aid: number, innerElement: Element): [number, number] {
        let parent: Node | null = el;
        const lastInnerFirstChild = this.getFirstChildUntilNull(innerElement);
        const lastInnerLastChild = this.getLastChildUntilNull(innerElement);

        while (parent && parent.nodeName === "#text") {
            parent = parent.parentNode;
        }

        if (parent == null) {
            throw new Error("No node found; parent was null");
        }

        let num = 0;

        let prevNodeName = parent.childNodes[0].nodeName;

        for (let i = 0; i < parent.childNodes.length; i++) {

            const child = parent.childNodes[i];

            if (this.checkIfAnnotation(child)) {

                const aidAttr = child.getAttribute("aid");
                if (aidAttr && parseInt(aidAttr, 10) === aid) {

                    let startnum = num - 1;
                    num += innerElement.childNodes.length;

                    if (lastInnerFirstChild.nodeName === prevNodeName) {
                        num--;
                    }
                    if (i < parent.childNodes.length - 1 && lastInnerLastChild.nodeName === parent.childNodes[i + 1].nodeName) {
                        num--;
                    }

                    if (startnum < 0) {
                        startnum = 0;
                    }
                    return [startnum, num];

                } else if (checkIfElement(child)) {
                    const innerEl = child.getElementsByClassName("highlighted")[0];
                    num += innerEl.childNodes.length;

                    if (lastInnerFirstChild.nodeName === prevNodeName) {
                        num--;
                    }

                    if (i < parent.childNodes.length - 1 && lastInnerLastChild.nodeName === parent.childNodes[i + 1].nodeName) {
                        num--;
                    }

                    continue;
                } else {
                    throw new Error("Child was not an element");
                }
            }

            num++;
            prevNodeName = parent.childNodes[i].nodeName;
        }

        throw new Error("No node found");
    }

    /**
     * Gets the comments of the given annotation.
     * @param id - Annotation ID
     * @returns Object - Annotation
     */
    getAnnotation(id: number): IAnnotation | undefined {
        for (const a of this.annotations) {
            if (id === a.id) {
                return a;
            }
        }
    }

    /**
     * Creates the actual (pop over) annotation element.
     * @param annotation - Annotation info
     * @param show - Whether to show the annotation on creation or not
     * @returns {Element} - Annotation element
     */
    createPopOverElement(annotation: IAnnotationCoordless, show: boolean): HTMLElementTagNameMap["span"] {
        const element = document.createElement("annotation");

        const velpData = this.getVelpById(annotation.velp);

        let velpContent;

        if (velpData != null) {
            velpContent = velpData.content;
        } else {
            velpContent = annotation.content;
        }

        if (annotation.default_comment == null) {
            annotation.default_comment = "";
        }

        if (annotation.color == null) {
            annotation.color = "";
        }

        element.setAttribute("velp", velpContent);
        element.setAttribute("show-str", show.toString());

        element.setAttribute("annotationdata", JSON.stringify(annotation));

        // The getNodeNumbers method uses aid attribute, so we include that. Otherwise it's not needed.
        element.setAttribute("aid", stringOrNull(annotation.id));

        return element;
    }

    /**
     * Shows the annotation (despite the name).
     * @todo If the annotation should be toggled, change all `showAnnotation()` methods to `toggleAnnotation()`.
     * @param annotation - Annotation to be showed.
     * @param scrollToAnnotation Whether to scroll to annotation if it is not in viewport.
     */
    async toggleAnnotation(annotation: IAnnotation, scrollToAnnotation: boolean) {
        const parent = document.getElementById(annotation.coord.start.par_id);
        if (parent == null) {
            return;
        }

        // We might click a margin annotation, but we still want to open the corresponding inline annotation,
        // if it exists.
        const prefix = isFullCoord(annotation.coord.start) && isFullCoord(annotation.coord.end) ? "t" : "m";
        let actrl = this.vctrl.getAnnotation(prefix + annotation.id);
        if (!annotation.answer_id && !actrl) {
            actrl = this.vctrl.getAnnotation("m" + annotation.id);
        }
        if (actrl) {
            if ((annotation.coord.start == null || actrl.show || !annotation.user_id)) {
                actrl.toggleAnnotationShow();
                if (scrollToAnnotation && actrl.show) {
                    actrl.scrollToIfNotInViewport();
                }
                return;
            }
        }

        if (!annotation.answer_id) {
            return;
        }
        // Find answer browser and its scope
        // set answer id -> change answer to that
        // query selector element -> toggle annotation
        const loader = parent.getElementsByTagName("TIM-PLUGIN-LOADER")[0];
        if (!loader) {
            return;
        }
        const taskId = loader.getAttribute("task-id");
        if (!taskId) {
            return;
        }
        let ab = this.getAnswerBrowserFromPluginLoader(loader) || null;

        if (!ab) {
            const loaderCtrl = this.vctrl.getPluginLoader(taskId);
            if (!loaderCtrl) {
                return;
            }
            loaderCtrl.loadPlugin();
            ab = await loaderCtrl.abLoad.promise;
            if (!ab) {
                return;
            }
        }
        if (this.vctrl.selectedUser.id !== annotation.user_id) {
            for (const u of this.vctrl.users) {
                if (u.user.id === annotation.user_id) {
                    this.vctrl.changeUser(u.user, false);
                    break;
                }
            }
        }

        if (!(ab.review && ab.selectedAnswer && ab.selectedAnswer.id === annotation.answer_id)) {
            // If review is false, setting review to true will eventually update the answer,
            // and we don't want to do it twice. Therefore setAnswerById shall only update the answer if review
            // is already true.
            ab.setAnswerById(annotation.answer_id, ab.review);
            ab.review = true;
        }

        const actrl2 = await this.vctrl.getAnnotationAsync(prefix + annotation.id);
        if (!actrl2) {
            return;
        }
        actrl2.showAnnotation();
        if (scrollToAnnotation) {
            actrl2.scrollToIfNotInViewport();
        }
    }
}
