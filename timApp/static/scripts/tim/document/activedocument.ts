import {TimDocument} from "tim/document/timDocument";

let activeDocument: TimDocument | null = null;

export function setActiveDocument(d: TimDocument) {
    activeDocument = d;
}

export function getActiveDocument(): TimDocument {
    if (activeDocument == null) {
        throw new Error(
            "Active document was null; getActiveDocument was probably called before setActiveDocument was called"
        );
    }
    return activeDocument;
}
