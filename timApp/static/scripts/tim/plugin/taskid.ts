import {iso, Newtype} from "newtype-ts";
import {Result} from "../util/utils";

// Note: this regex allows empty task name because we want to detect that case when parsing.
// It enables giving a more accurate error message.
const taskIdRe = /^((\d+)\.)?([a-zåäöA-ZÅÄÖ0-9_-]*)(\.([a-zA-Z0-9_-]+))?(:([a-zA-Z]*)(:(readonly|readwrite))?)?$/;

export enum DocIdOption {
    Required,
    Optional,
    Forbidden,
}

export enum BlockHintOption {
    Required,
    Optional,
    Forbidden,
}

export enum TypeOption {
    Required,
    Optional,
    Forbidden,
}

export enum TaskIdAccess {
    ReadOnly,
    ReadWrite,
}

const KNOWN_FIELD_NAMES = new Set(["points", "datetime", "ALL"]);

function isValidId(blockHint: string) {
    return blockHint.length === 12; // TODO more accurate check
}

// Denotes a stringified TaskId that has only docId and name parts.
export interface DocIdDotName
    extends Newtype<{readonly TaskDocIdStr: unique symbol}, string> {
    toString(): string;
}

const docIdDotName = iso<DocIdDotName>();

/**
 * Represents a task id. Every plugin that wants to save answers needs to have one.
 */
export class TaskId {
    constructor(
        public name: string,
        public docId?: number,
        public field?: string,
        public blockHint?: string,
        public plugintype?: string,
        public access?: TaskIdAccess
    ) {}

    docTask() {
        if (!this.docId) {
            throw new Error("Task id does not have docId");
        }
        return docIdDotName.wrap(`${this.docId}.${this.name}`);
    }

    docTaskField() {
        const f = this.field ? `.${this.field}` : "";
        return `${this.docTask().toString()}${f}`;
    }

    /**
     * Tries to parse a task id. Never throws.
     */
    static tryParse(
        tid: string,
        docidOpt: DocIdOption = DocIdOption.Required,
        blockHintOpt: BlockHintOption = BlockHintOption.Optional,
        typeOpt: TypeOption = TypeOption.Forbidden
    ): Result<TaskId, string> {
        const match: Array<string | undefined> | null = taskIdRe.exec(tid);
        if (!match) {
            return {ok: false, result: `Task id does not match regex: ${tid}`};
        }
        const [, , docId, name, , blockHintOrField, , plugintype, , rw] = match;
        if (name === undefined) {
            throw new Error(
                "name should've been defined because regex passed?"
            );
        }
        if (!name) {
            return {ok: false, result: `Task id has empty name: ${tid}`};
        }
        if (docidOpt == DocIdOption.Required && !docId) {
            return {
                ok: false,
                result: `Task id does not have required docId: ${tid}`,
            };
        }
        if (docidOpt == DocIdOption.Forbidden && docId) {
            return {ok: false, result: `Task id has forbidden docId: ${tid}`};
        }
        if (typeOpt == TypeOption.Forbidden && plugintype) {
            return {
                ok: false,
                result: `Task id has forbidden plugintype: ${tid}`,
            };
        }
        let field: string | undefined;
        let blockHint: string | undefined;
        if (blockHintOrField && KNOWN_FIELD_NAMES.has(blockHintOrField)) {
            field = blockHintOrField;
        } else if (blockHintOrField) {
            if (!isValidId(blockHintOrField)) {
                return {
                    ok: false,
                    result: `Invalid block id format in task id: ${tid}`,
                };
            }
            blockHint = blockHintOrField;
        } else if (
            !blockHintOrField &&
            blockHintOpt === BlockHintOption.Required
        ) {
            return {
                ok: false,
                result: `Missing required block id in task id: ${tid}`,
            };
        }
        let access: TaskIdAccess | undefined;
        if (rw == "readonly") {
            access = TaskIdAccess.ReadOnly;
        } else if (rw == "readwrite") {
            access = TaskIdAccess.ReadWrite;
        }
        return {
            ok: true,
            result: new TaskId(
                name,
                docId != null ? parseInt(docId, 10) : undefined,
                field,
                blockHint,
                plugintype,
                access
            ),
        };
    }
}
