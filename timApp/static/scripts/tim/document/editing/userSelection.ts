import type {ParSelection} from "tim/document/editing/parSelection";
import type {ParContext} from "tim/document/structure/parContext";

export class UserSelection<T extends ParSelection = ParSelection> {
    constructor(public sel: T, public anchor: ParContext) {}
}
