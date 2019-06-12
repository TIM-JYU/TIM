import * as t from "io-ts";
import {IJsRunnerMarkup} from "../public/javascripts/jsrunnertypes";
import {AliasDataT, UserFieldDataT} from "../servertypes";

function genericTypeError(method: string, parameterDescription: string, v: unknown) {
    return new Error(`${method} called with ${parameterDescription} of type ${typeof v}`);
}

function fieldNameTypeError(method: string, v: unknown) {
    return genericTypeError(method, "fieldName", v);
}

function defaultValueTypeError(method: string, v: unknown) {
    return genericTypeError(method, "default value", v);
}

function valueTypeError(method: string, v: unknown) {
    return genericTypeError(method, "value", v);
}

const checkString = t.string.is;
const checkNumber = t.number.is;
const checkInt = t.Int.is;

const StringOrNumber = t.union([t.string, t.number]);

function ensureStringFieldName(method: string, s: unknown): string {
    if (!checkString(s)) {
        throw fieldNameTypeError(method, s);
    }
    return s;
}

function ensureNumberDefault(method: string, s: unknown): number {
    if (!checkNumber(s)) {
        throw defaultValueTypeError(method, s);
    }
    return s;
}

function ensureIntDefault(method: string, s: unknown): number {
    if (!checkInt(s)) {
        throw defaultValueTypeError(method, s);
    }
    return s;
}

function ensureStringDefault(method: string, s: unknown): string {
    if (!checkString(s)) {
        throw defaultValueTypeError(method, s);
    }
    return s;
}

function ensureNumberLikeValue(method: string, s: unknown): number {
    if (!StringOrNumber.is(s)) {
        throw valueTypeError(method, s);
    }
    if (typeof s === "string") {
        const v = parseFloat(s.replace(",", "."));
        if (isNaN(v)) {
            throw valueTypeError(method, s);
        }
        return v;
    }
    return s;
}

function ensureStringLikeValue(method: string, s: unknown): string {
    if (!StringOrNumber.is(s)) {
        throw valueTypeError(method, s);
    }
    if (typeof s === "number") {
        return s.toString();
    }
    return s;
}

const ABSOLUTE_FIELD_REGEX = /^[0-9]+\./;

class Tools {
    private printP = "";
    private error = "";
    private result: {[index: string]: unknown} = {};

    constructor(
        private data: UserFieldDataT,
        private currDoc: string,
        private markup: IJsRunnerMarkup,
        private aliases: AliasDataT,
    ) {
    }

    private normalizeField(fieldName: string) {
        if (ABSOLUTE_FIELD_REGEX.test(fieldName)) {
            return fieldName;
        } else {
            return this.currDoc + fieldName;
        }
    }

    private normalizeAndGet(fieldName: string) {
        if (fieldName in this.aliases) {
            return this.data.fields[fieldName];
        }
        const fn = this.normalizeField(fieldName);
        return this.data.fields[fn];
    }

    private normalizeAndSet(fieldName: string) {
        if (fieldName in this.aliases) {
            return this.normalizeField(this.aliases[fieldName]);
        }
        return this.normalizeField(fieldName);
    }

    getStudentName(): string {
        return this.data.user.real_name;
    }

    getDouble(fieldName: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName("getDouble", fieldName);
        const def = ensureNumberDefault("getDouble", defa);
        const s = this.normalizeAndGet(f);
        if (checkNumber(s)) {
            return this.handlePossibleNaN(s, s, def);
        }
        if (s === null || s === undefined) {
            return def;
        }
        if (typeof s !== "string") {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        const sp = s.replace(",", ".");
        const r = parseFloat(sp);
        return this.handlePossibleNaN(r, s, def);
    }

    private handlePossibleNaN<T>(r: number, s: unknown, def: T) {
        if (isNaN(r)) {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        return r;
    }

    private reportInputTypeErrorAndReturnDef<T>(s: unknown, def: T) {
        this.reportError(`getDouble found user '${this.data.user.name}' value '${s}' of type ${typeof s}, using default value ${def}`);
        return def;
    }

    getInt(fieldName: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName("getInt", fieldName);
        const def = ensureIntDefault("getInt", defa);
        const s = this.normalizeAndGet(f);
        if (checkInt(s)) {
            return this.handlePossibleNaN(s, s, def);
        }
        if (s === null || s === undefined) {
            return def;
        }
        if (typeof s !== "string") {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        const r = parseInt(s, 10);
        return this.handlePossibleNaN(r, s, def);
    }

    getString(fieldName: unknown, defa: unknown = ""): string {
        const f = ensureStringFieldName("getString", fieldName);
        const def = ensureStringDefault("getString", defa);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        if (!checkNumber(s)) {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        return s.toString();
    }

    getValue(fieldName: unknown, def: unknown = "") {
        const f = ensureStringFieldName("getValue", fieldName);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        return s;
    }

    getSum(fieldName: unknown, start: unknown, end: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName("getSum", fieldName);
        const def = ensureNumberDefault("getSum", defa);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers in getSum method.");
        }
        let sum = 0;
        for (let i = start; i <= end; i++) {
            sum += this.getDouble(f + i.toString(), def);
        }
        return sum;
    }

    setString(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName("setString", fieldName);
        const c = ensureStringLikeValue("setString", content);
        const fn = this.normalizeAndSet(f);
        this.result[fn] = c;
    }

    setInt(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName("setInt", fieldName);
        const c = ensureNumberLikeValue("setInt", content);
        const fn = this.normalizeAndSet(f);
        if (!checkInt(c)) {
            throw valueTypeError("setInt", content);
        }
        this.result[fn] = c;
    }

    setDouble(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName("setDouble", fieldName);
        const c = ensureNumberLikeValue("setDouble", content);
        const fn = this.normalizeAndSet(f);
        this.result[fn] = c;
    }

    getDefaultPoints(): number {
        if (!this.markup.defaultPoints) {
            throw new Error("Default points have not been set");
        }
        return this.markup.defaultPoints;
    }

    getGrade(points: unknown): string | number {
        if (!checkNumber(points)) {
            throw new Error("Points must be number in getGrade method");
        }
        if (!this.markup.gradingScale) {
            throw new Error("gradingScale has not been set");
        }
        const scale = this.markup.gradingScale;
        const values = Object.entries(scale);
        values.sort((a, b) => b[1] - a[1]);
        let grade = this.markup.failGrade || "";
        for (const [currGrade, requiredPoints] of values) {
            if (points >= requiredPoints) {
                grade = currGrade;
                break;
            }
        }
        return grade;
    }

    saveGrade(gradeVal: unknown, points: unknown = this.markup.defaultPoints) {
        const d = this.markup.gradeField || "grade";
        const fn = this.normalizeAndSet(d);
        this.result[fn] = gradeVal;
        const c = this.markup.creditField || "credit";
        const fnc = this.normalizeAndSet(c);
        let p;
        if (points !== undefined) {
            p = ensureNumberLikeValue("saveGrade", points);
            if (!checkInt(p)) {
                throw new Error("saveGrade: points is not int");
            }
        } else {
            p = 0;
        }
        this.result[fnc] = p;
    }

    defineTime(s: unknown): number {
        if (!checkString(s)) {
            throw valueTypeError("defineTime", s);
        }
        // TODO: fix timezone to work locally
        const localDateTime = new Date(s);
        const offset = localDateTime.getTimezoneOffset() * 60;
        return (localDateTime.getTime() / 1000) + offset;
    }

    getDateTime(fieldName: unknown, defa: unknown = NaN): number {
        const f = ensureStringFieldName("getDateTime", fieldName);
        const def = ensureNumberDefault("getDateTime", defa);
        return this.getDouble(fieldName, defa);
    }

    print(...args: any[]) {
        for (const a of args) {
            this.printP += a + " ";
        }
        this.printP += "\n";
    }

    getResult() {
        return {user: this.data.user.id, fields: this.result};
    }

    getPrint() {
        return this.printP;
    }

    getError() {
        return this.error;
    }

    private fatalError(msg: string) {
        // TODO
    }

    private reportError(msg: string) {
        // TODO
    }
}

export default Tools;
