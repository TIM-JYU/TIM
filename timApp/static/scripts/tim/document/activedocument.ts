import {Document} from "tim/document/document";

let activeDocument: Document | null = null;

export function setActiveDocument(d: Document) {
    activeDocument = d;
}

export function getActiveDocument(): Document {
    if (activeDocument == null) {
        throw new Error(
            "Active document was null; getActiveDocument was probably called before setActiveDocument was called"
        );
    }
    return activeDocument;
}
