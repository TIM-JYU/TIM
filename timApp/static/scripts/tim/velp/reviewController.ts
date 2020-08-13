import angular, {IScope} from "angular";
import $ from "jquery";
import {Users} from "tim/user/userService";
import {
    AnnotationAddReason,
    AnnotationComponent,
    AnnotationPlacement,
    IAnnotationBindings,
    updateAnnotationServer,
} from "tim/velp/annotation.component";
import {deserialize} from "typescript-json-serializer";
import {TaskId} from "tim/plugin/taskid";
import {DrawCanvasComponent, DrawObject, getDrawingDimensions} from "tim/plugin/drawCanvas";
import {IAnswer} from "../answer/IAnswer";
import {addElementToParagraphMargin} from "../document/parhelpers";
import {ViewCtrl} from "../document/viewctrl";
import {IItem} from "../item/IItem";
import {showMessageDialog} from "../ui/dialog";
import {documentglobals} from "../util/globals";
import {$compile, $http, $rootScope} from "../util/ngimport";
import {angularWait, assertIsText, checkIfElement, getElementParent, log, to, truncate} from "../util/utils";
import {VelpSelectionController} from "./velpSelection";
import {
    Annotation,
    IAnnotationCoordinate,
    IAnnotationInterval,
    isFullCoord,
    IVelp,
    IVelpUI,
    NewAnnotation,
} from "./velptypes";

/**
 * The controller handles the logic related to adding and removing annotations. It also handles the way how
 * the selected area is defined in the view. Requires `velpSelection` directive.
 *
 * @author Joonas Lattu
 * @author Petteri Palojärvi
 * @author Seppo Tarvainen
 * @licence MIT
 * @copyright 2016 Timber project members
 */

const illegalClasses = ["annotation-info", "highlighted", "editorArea", "previewcontent"];

function tryCreateRange(
    start: Required<IAnnotationCoordinate>,
    end: Required<IAnnotationCoordinate>,
    startElem: Node,
    endElem: Node | undefined,
) {
    const range = document.createRange();
    try {
        range.setStart(startElem, start.offset);
    } catch {
        return undefined;
    }
    try {
        range.setEnd(endElem ?? startElem, end.offset);
    } catch {
        // Ignore exception; it is still possibly useful to put the annotation in text even though it's zero sized.
    }
    return range;
}

/**
 * A class for handling annotations.
 */
export class ReviewController {
    private selectedArea?: Range;
    public selectedElement?: Element;
    private selectionIsDrawing = false;
    private selectedCanvas?: DrawCanvasComponent;
    public item: IItem;
    private annotations: Annotation[];
    public zIndex: number;
    private scope: IScope;
    private velpBadge?: HTMLInputElement;
    private velpBadgePar?: string;
    private velpSelection?: VelpSelectionController; // initialized through onInit
    public velpMode: boolean;
    public velps?: IVelpUI[];

    constructor(public vctrl: ViewCtrl) {
        this.scope = vctrl.scope;
        this.velpMode = documentglobals().velpMode;
        this.item = documentglobals().curr_item;
        this.annotations = [];
        this.zIndex = 3;
    }

    initVelpSelection(velpSelection: VelpSelectionController) {
        this.velpSelection = velpSelection;

        document.addEventListener("selectionchange", () => {
            const range = getSelection()?.getRangeAt(0);

            // Length check is important - we don't want to lose the previous selection in touch devices.
            if (range && range.toString().length > 0) {
                this.selectionIsDrawing = false;
                this.selectText($(range.startContainer).parents(".par")[0] as Element);
                $rootScope.$applyAsync();
            }
        });
    }

    /**
     * Loads the document annotations into the view.
     */
    async loadDocumentAnnotations() {
        const response = await to($http.get<Record<string, unknown>[]>(`/${this.item.id}/get_annotations`));
        if (!response.ok) {
            return;
        }
        this.annotations = response.result.data.map((o) => {
            const ann = deserialize(o, Annotation);
            return ann;
        });
        const annotationsToRemove = [];

        for (const a of this.annotations) {

            const placeInfo = a.coord;
            const parent = document.getElementById(placeInfo.start.par_id);

            if (parent == null) {
                // TODO: Decide what to do, when parent element has been deleted, for now remove annotation from list
                annotationsToRemove.push(a);
                continue;
            }

            if (a.answer != null) {
                if (!parent.classList.contains("has-annotation")) {
                    parent.classList.add("has-annotation");
                }
                continue;
            }

            if (parent.getAttribute("t") === placeInfo.start.t && isFullCoord(placeInfo.start) && isFullCoord(placeInfo.end)) {

                try {
                    let elements = parent.querySelector(".parContent");
                    if (elements == null) {
                        this.addAnnotationToMargin(
                            parent,
                            a,
                            AnnotationAddReason.LoadingExisting,
                            AnnotationPlacement.InMarginOnly);
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

                    const range = tryCreateRange(placeInfo.start, placeInfo.end, startel, endel);
                    let added = false;
                    if (range) {
                        added = this.addAnnotationToCoord(range, a, AnnotationAddReason.LoadingExisting);
                    }
                    this.addAnnotationToMargin(parent, a, AnnotationAddReason.LoadingExisting, added ? AnnotationPlacement.InMargin : AnnotationPlacement.InMarginOnly);
                } catch (err) {
                    this.addAnnotationToMargin(parent, a, AnnotationAddReason.LoadingExisting, AnnotationPlacement.InMarginOnly);
                }
            } else {
                this.addAnnotationToMargin(parent, a, AnnotationAddReason.LoadingExisting, AnnotationPlacement.InMarginOnly);
            }
        }

        for (const r of annotationsToRemove) {
            const index = this.annotations.indexOf(r);
            this.annotations.splice(index, 1);
        }

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
     * Removes answer annotations from the paragraph margin
     * @param par Paragraph to inspect
     */
    clearAnswerAnnotationsFromParMargin(par: Element): void {
        const oldAnnotations = par.querySelectorAll(".notes [aid]");
        for (const ele of oldAnnotations) {
            // aid cannot be null here because the above selector has [aid]
            const aid = ele.getAttribute("aid")!;
            const numAid = parseInt(aid, 10);
            if (!isNaN(numAid)) {
                const ann = this.vctrl.getAnnotation("m" + numAid);
                if (ann?.annotation.answer) {
                    angular.element(ele).remove();
                }
            }
        }
    }

    /**
     * Loads the annotations to the answer if given and removes answer annotations not associated with the answer.
     * @param answerId - Optional answer ID
     * @param par - Paragraph element
     */
    loadAnnotationsToAnswer(answerId: number | undefined, par: Element): void {
        this.clearAnswerAnnotationsFromParMargin(par);
        if (answerId == undefined) {
            return;
        }
        const annotations = this.getAnnotationsByAnswerId(answerId);
        for (const a of annotations) {
            const placeInfo = a.coord;
            let added = false;

            if (a.draw_data) {
                if (!a.coord.start.t) {
                    continue;
                }
                const targ = par.querySelector(".drawnAnnotationContainer");
                if (targ) {

                    // const drawdata = JSON.parse(a.coord.start.t) as ILineSegment[];
                    const drawdata = JSON.parse(a.coord.start.t) as DrawObject[];
                    const rect = getDrawingDimensions(drawdata);
                    const borderElement = this.createPictureBorder(rect.w, rect.h);
                    targ.appendChild(borderElement);
                    // -----
                    // this.addAnnotationToMargin(par, a, AnnotationAddReason.LoadingExisting, AnnotationPlacement.InMargin);
                    const ele = this.compilePopOver(targ, borderElement, a, AnnotationAddReason.LoadingExisting) as HTMLElement;
                    ele.style.position = "absolute";
                    ele.style.left = rect.x + "px";
                    // ele.style.top = rect.y + rect.h + "px";
                    ele.style.top = rect.y + "px";
                    const span = ele.querySelector("span");
                    // a.color = "transparent";
                    const textAnnotation = this.vctrl.getAnnotation(`t${a.id}`);
                    if (textAnnotation) {
                        textAnnotation.values.color = "transparent";
                    }
                    added = true;
                }
            } else {

                const element = par.querySelector(".review pre")?.firstChild;

                if (!isFullCoord(placeInfo.start) || !isFullCoord(placeInfo.end) || !element) {
                    this.addAnnotationToMargin(
                        par,
                        a,
                        AnnotationAddReason.LoadingExisting,
                        // If the coordinates exist but the review element does not, we don't know yet if the annotation can be
                        // successfully placed in text.
                        !isFullCoord(placeInfo.start) ||
                        !isFullCoord(placeInfo.end) ? AnnotationPlacement.InMarginOnly : AnnotationPlacement.InMarginAndUnknownIfItWillBeInText,
                    );
                    continue;
                }
                const range = tryCreateRange(placeInfo.start, placeInfo.end, element, undefined);
                if (range) {
                    added = this.addAnnotationToCoord(range, a, AnnotationAddReason.LoadingExisting);
                } else {
                    log("annotation range invalid, adding to margin only");
                }
            }
            this.addAnnotationToMargin(par, a, AnnotationAddReason.LoadingExisting, added ? AnnotationPlacement.InMargin : AnnotationPlacement.InMarginOnly);
        }
        $rootScope.$applyAsync(); // TODO: run only if we are in Angular zone
    }

    /**
     * Gets all the annotations with a given answer ID.
     * @param id - Answer ID
     * @returns {Array} Annotations of the answer
     */
    getAnnotationsByAnswerId(id: number): Annotation[] {
        const annotations = this.annotations.filter((a) => a.answer != null && a.answer.id === id);
        annotations.sort((a, b) => (b.coord.start.offset ?? 0) - (a.coord.start.offset ?? 0));
        return annotations;
    }

    /**
     * Adds an annotation to the given element in a given coordinate.
     * @param range - Annotation coordinate
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     */
    addAnnotationToCoord(range: Range, annotation: Annotation, show: AnnotationAddReason): boolean {
        const {element, scope} = this.createPopOverElement(annotation, show, AnnotationPlacement.InText);
        try {
            range.surroundContents(element);
        } catch (err) {
            this.selectedArea = undefined;
            return false;
        }
        $compile(element)(scope);
        return true;
    }


    compilePopOver(container: Element, target: Element, annotation: Annotation, show: AnnotationAddReason): Element {
        const {element, scope} = this.createPopOverElement(annotation, show, AnnotationPlacement.InPicture);
        element.appendChild(target);
        container.appendChild(element);
        $compile(element)(scope);
        return element;
    }


    /**
     * Adds an annotation to the given element. The annotation will be placed in the margin.
     * @param el - Given element
     * @param annotation - Annotation info
     * @param show - Whether annotation is shown when created or not
     * @param reason - The reason why the annotation is put here (not implemented yet)
     */
    addAnnotationToMargin(el: Element, annotation: Annotation, show: AnnotationAddReason, reason: AnnotationPlacement): void {
        const {element, scope} = this.createPopOverElement(annotation, show, reason);
        const text = document.createTextNode("\u00A0" + annotation.getContent() + "\u00A0");
        element.appendChild(text);
        addElementToParagraphMargin(el, element);
        $compile(element)(scope);
    }

    /**
     * Creates the velp badge button (the button with letter 'V' on it).
     */
    createVelpBadge(par: string): HTMLInputElement {
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
    updateVelpBadge(oldElement: Element | undefined, newElement: Element | null): void {
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

        if (e != null && this.velpSelection) {
            this.selectedElement = undefined;
            this.selectedArea = undefined;
            this.velpSelection.updateVelpList();
            e.stopPropagation();
        }
    }

    /**
     * Deletes the given annotation.
     * @param id - Annotation ID
     */
    async deleteAnnotation(id: number) {
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
        await to($http.post("/invalidate_annotation", {id: id}));
        this.annotations = this.annotations.filter((a) => a.id !== id);
    }

    /**
     * Updates annotation data.
     * @param ann - Annotation
     */
    updateAnnotation(ann: Annotation) {
        const i = this.annotations.findIndex((a) => a.id === ann.id);
        if (i >= 0) {
            this.annotations[i] = ann;
        }
        this.vctrl.getAnnotation(`m${ann.id}`)?.setAnnotation(ann);
        this.vctrl.getAnnotation(`t${ann.id}`)?.setAnnotation(ann);
    }

    /**
     * Selects text range or just the element.
     * @todo When annotations can break tags, check annotations from all elements in the selection.
     */
    selectText(par: Element): void {
        if (!this.velpSelection) {
            return;
        }
        const oldElement = this.selectedElement;
        this.selectedElement = par;
        const range = getSelection();
        if (range && range.toString().length > 0) {
            this.selectedArea = range.getRangeAt(0);
        } else {
            this.selectedArea = undefined;
        }

        if (this.selectedArea != null) {
            // Check if selection breaks tags, has annotation as a parent or as a child.
            if (this.isSelectionTagParentsUnequal(this.selectedArea) ||
                this.hasSelectionParentAnnotation(this.selectedArea) ||
                this.hasSelectionChildrenAnnotation(this.selectedArea)) {
                this.selectedArea = undefined;
            }
        }

        const newElement = this.selectedElement;
        this.updateVelpBadge(oldElement, newElement ?? null);
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

    getSelectedAnswerTaskName() {
        const a = this.selectedElement && this.getAnswerInfo(this.selectedElement);
        if (a) {
            const p = TaskId.tryParse(a.task_id);
            if (p.ok) {
                return p.result.name;
            }
        }
    }

    getSelectedBeginning() {
        if (!this.selectedElement) {
            return;
        }
        return truncate(this.selectedElement.querySelector(".parContent")?.textContent?.trim() ?? "", 20);
    }

    createPictureBorder(width: number, height: number): HTMLElement {
        const ind = document.createElement("div");
        ind.style.width = width + "px";
        ind.style.height = height + "px";
        ind.style.border = "1px solid #000000";
        ind.className = "annotation-picture-element";
        return ind;
    }

    /**
     * Adds an annotation with the selected velp's data to
     * the selected text area or element.
     * @todo When the annotations can cross HTML tags, end coordinate needs to be changed according to the end element.
     * @todo Also get the paragraph element (parelement) according to endContainer.
     * @param velp - Velp selected in the `velpSelection` directive
     */
    async useVelp(velp: IVelp) {
        if (this.selectedElement == null) {
            return;
        }

        const newAnnotation = new NewAnnotation(
            velp,
            Users.getCurrent(),
            null,
        );
        let coord: IAnnotationInterval;
        if (this.selectionIsDrawing) {
            const targ = this.selectedElement.querySelector(".drawnAnnotationContainer");
            if (!this.selectedCanvas || !targ) {
                return;
            }
            const velpDrawing = this.selectedCanvas.getDrawing();
            if (Object.keys(velpDrawing).length == 0) {
                return;
            }
            let corners = {x: 0, y: 0, h: 0, w: 0};
            corners = this.selectedCanvas.getCurrentDrawingDimensions();
            const borderElement = this.createPictureBorder(corners.w, corners.h);
            let parelement: Element | null = this.selectedElement;
            while (parelement && !parelement.hasAttribute("t")) {
                parelement = getElementParent(parelement);
            }
            if (!parelement) {
                showMessageDialog("Could not add annotation (parelement missing)");
                return;
            }
            const answerInfo = this.getAnswerInfo(parelement);
            if (answerInfo != null) {
                newAnnotation.answer_id = answerInfo.id;
            } else {
                showMessageDialog("Could not add annotation (answerinfo missing)");
                return;
            }
            targ.appendChild(borderElement);
            const ann = await this.addAnnotation(
                newAnnotation,
                {start: {par_id: parelement.id}, end: {par_id: parelement.id}},
                velp,
            );
            ann.draw_data = velpDrawing;

            const ele = this.compilePopOver(targ, borderElement, ann, AnnotationAddReason.AddingNew) as HTMLElement;
            ele.style.position = "absolute";
            ele.style.left = corners.x + "px";
            ele.style.top = corners.y + "px";

            // TODO: db column for drawdata. Until then assume coord depth 0 means image velp
            coord = {
                start: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") ?? undefined,
                    offset: Math.round(corners.x),
                    node: Math.round(corners.w),
                    depth: 0,
                },
                end: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") ?? undefined,
                    offset: Math.round(corners.y),
                    node: Math.round(corners.h),
                    depth: 0,
                },
            };
            this.addAnnotationToMargin(this.selectedElement, ann, AnnotationAddReason.LoadingExisting, AnnotationPlacement.InMargin);
            await angularWait();

            const saved = await updateAnnotationServer({
                id: ann.id,
                ...ann.getEditableValues(),
                coord,
                drawData: velpDrawing,
            });
            if (saved.ok) {
                const annCopy = saved.result;
                this.updateAnnotation(annCopy);

                this.selectedCanvas.storeDrawing();
            }
            // TODO clear indicators and margins if !saved.ok
            this.selectedElement = undefined;

        } else if (this.selectedArea != null) {

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
            const ann = await this.addAnnotation(
                newAnnotation,
                {start: {par_id: parelement.id}, end: {par_id: parelement.id}},
                velp,
            );
            const added = this.addAnnotationToCoord(this.selectedArea, ann, AnnotationAddReason.AddingNew);
            this.addAnnotationToMargin(this.selectedElement, ann, AnnotationAddReason.LoadingExisting, added ? AnnotationPlacement.InMargin : AnnotationPlacement.InMarginOnly);
            await angularWait();

            const nodeNums = this.getNodeNumbers(this.selectedArea.startContainer, ann.id, innerDiv);
            coord = {
                start: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") ?? undefined,
                    el_path: elementPath,
                    offset: startoffset,
                    depth: elementPath.length,
                    node: nodeNums[0],
                },
                end: {
                    par_id: parelement.id,
                    t: parelement.getAttribute("t") ?? undefined,
                    el_path: elementPath,
                    offset: endOffset,
                    depth: elementPath.length,
                    node: nodeNums[1],
                },
            };
            const saved = await updateAnnotationServer({
                id: ann.id,
                ...ann.getEditableValues(),
                coord,
            });
            if (saved.ok) {
                this.updateAnnotation(saved.result);
            }

            this.selectedArea = undefined;

        } else {

            coord = {
                start: {
                    par_id: this.selectedElement.id,
                    t: this.selectedElement.getAttribute("t") ?? undefined,
                },
                end: {
                    par_id: this.selectedElement.id,
                    t: this.selectedElement.getAttribute("t") ?? undefined,
                },
            };

            const answerInfo = this.getAnswerInfo(this.selectedElement);

            if (answerInfo != null) {
                newAnnotation.answer_id = answerInfo.id;
            }
            const ann = await this.addAnnotation(newAnnotation, coord, velp);
            this.addAnnotationToMargin(this.selectedElement, ann, AnnotationAddReason.AddingNew, AnnotationPlacement.InMarginOnly);
        }
    }

    private async addAnnotation(newAnnotation: NewAnnotation, coord: IAnnotationInterval, velp: IVelp) {
        const json = await to($http.post<Record<string, unknown>>("/add_annotation", {
            answer_id: newAnnotation.answer_id,
            coord: coord,
            doc_id: this.vctrl.item.id,
            points: velp.points,
            velp_id: newAnnotation.velp.id,
            visible_to: velp.visible_to,
        }));
        if (!json.ok) {
            throw Error(json.result.data.error);
        }
        const ann = deserialize(json.result.data, Annotation);
        this.annotations.push(ann);
        return ann;
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
                return ab?.selectedAnswer;
            }
            return;
        }

        const loader = $(start).parents("tim-plugin-loader")[0];
        if (!loader) {
            return;
        }
        const ctrl = this.getAnswerBrowserFromPluginLoader(loader);
        return ctrl?.selectedAnswer;
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
     * Creates the actual (pop over) annotation element.
     */
    createPopOverElement(annotation: Annotation, reason: AnnotationAddReason, placement: AnnotationPlacement) {
        const element = document.createElement("annotation");

        const bindings: IAnnotationBindings = {
            reason,
            placement,
            annotation,
            defaultcomment: reason == AnnotationAddReason.AddingNew ? this.getVelpById(annotation.velp)!.default_comment : "",
        };
        const scope = Object.assign(this.scope.$new(true), bindings);
        for (const k of Object.keys(bindings)) {
            element.setAttribute(`bind-${k}`, k);
        }

        // The getNodeNumbers method uses aid attribute, so we include that. Otherwise it's not needed.
        element.setAttribute("aid", annotation.id.toString());

        return {element, scope};
    }

    isDrawnAnnotation(ann: Annotation): boolean { // placeholder
        return (ann.coord.start.depth == 0 && ann.coord.end.depth == 0);
    }

    /**
     * Shows the annotation (despite the name).
     * @param ac - Annotation to be shown.
     * @param scrollToAnnotation Whether to scroll to annotation if it is not in viewport.
     */
    async toggleAnnotation(ac: AnnotationComponent | Annotation, scrollToAnnotation: boolean) {
        let annotation: Annotation;
        if (ac instanceof AnnotationComponent) {
            annotation = ac.annotation;
        } else {
            annotation = ac;
        }
        const parent = document.getElementById(annotation.coord.start.par_id);
        if (parent == null) {
            log(`par ${annotation.coord.start.par_id} not found`);
            return;
        }


        // We might click a margin annotation, but we still want to open the corresponding inline annotation,
        // if it exists.
        const prefix = ((isFullCoord(annotation.coord.start) && isFullCoord(annotation.coord.end)) || annotation.draw_data)  &&
        ((ac instanceof AnnotationComponent && ac.placement !== AnnotationPlacement.InMarginOnly)
            || ac instanceof Annotation) ? "t" : "m";

        let actrl = this.vctrl.getAnnotation(prefix + annotation.id);
        if (!annotation.answer && !actrl) {
            actrl = this.vctrl.getAnnotation("m" + annotation.id);
        }
        if (actrl) {
            if ((annotation.coord.start.offset == null || actrl.show || !annotation.answer)) {
                actrl.toggleAnnotationShow();
                if (scrollToAnnotation && actrl.show) {
                    actrl.scrollToIfNotInViewport();
                }
                return;
            }
        }

        if (!annotation.answer) {
            log("annotation.answer is null");
            return;
        }
        // Find answer browser and its scope
        // set answer id -> change answer to that
        // query selector element -> toggle annotation
        const loader = parent.getElementsByTagName("TIM-PLUGIN-LOADER")[0];
        if (!loader) {
            log("tim-plugin-loader not found");
            return;
        }
        const taskId = loader.getAttribute("task-id");
        if (!taskId) {
            log("task-id missing");
            return;
        }
        let ab = this.getAnswerBrowserFromPluginLoader(loader) ?? null;

        if (!ab) {
            const loaderCtrl = this.vctrl.getPluginLoader(taskId);
            if (!loaderCtrl) {
                log("getPluginLoader failed");
                return;
            }
            loaderCtrl.loadPlugin();
            ab = await loaderCtrl.abLoad.promise;
            if (!ab) {
                log("plugin has no answerbrowser");
                return;
            }
        }
        const uid = annotation.answer.users[0].id;
        if (this.vctrl.selectedUser.id !== uid) {
            for (const u of this.vctrl.users) {
                if (u.user.id === uid) {
                    await this.vctrl.changeUser(u.user, false);
                    break;
                }
            }
        }

        if (!(ab.review && ab.selectedAnswer && ab.selectedAnswer.id === annotation.answer.id)) {
            ab.review = true;
            await ab.setAnswerById(annotation.answer.id);
        }
        let r = await to(this.vctrl.getAnnotationAsync(prefix + annotation.id));
        if (!r.ok) {
            log("falling back to showing margin annotation");
            r = await to(this.vctrl.getAnnotationAsync("m" + annotation.id));
            if (!r.ok) {
                log("getAnnotationAsync failed even for margin");
                return;
            }
        }
        const actrl2 = r.result;
        actrl2.showAnnotation();
        if (scrollToAnnotation) {
            actrl2.scrollToIfNotInViewport();
        }
    }

    setCanvas(par: Element, answerId: number, canvas: DrawCanvasComponent): void {
        canvas.setClickCallback(this.clickFromCanvas);
        canvas.id = answerId;
        const annotationDrawings = this.getAnnotationsByAnswerId(answerId).reduce((arr: DrawObject[], ann: Annotation) => {
            if (ann.draw_data) {
                arr = arr.concat(ann.draw_data);
            }
            return arr;
        }, []);
        canvas.setPersistentDrawData(annotationDrawings);
    }

    clickFromCanvas = (canvas: DrawCanvasComponent, x: number, y: number) => {
        if (canvas.drawingAnything) {
            this.selectedArea = undefined;
            this.selectionIsDrawing = true;
            const par = $(canvas.canvas.nativeElement).parents(".par")[0] as Element;
            this.selectedElement = par;
            this.selectedCanvas = canvas;
            return;
        } else {
            const anns = this.getAnnotationsByAnswerId(canvas.id);
            for (const a of anns) {
                if (!a.draw_data) {
                    continue;
                }
                if (canvas.isCoordWithinDrawing(a.draw_data, x, y)) {
                    const tanncomp = this.vctrl.getAnnotation(`t${a.id}`);
                    if (!tanncomp) {
                        console.log("couldn't find annotation via vctrl");
                        return;
                    }
                    console.log("found annotation via vctrl");
                    tanncomp.toggleAnnotationShow();
                    // if overlap then iterate
                }
            }
        }
    };
}
