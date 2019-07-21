import * as t from "io-ts";
import {IError, IJsRunnerMarkup, IStatData} from "../public/javascripts/jsrunnertypes";
import {AliasDataT, UserFieldDataT} from "../servertypes";

function genericTypeError(parameterDescription: string, v: unknown) {
    return new Error(`${parameterDescription} has unexpected type: ${typeof v}`);
}

function fieldNameTypeError(v: unknown) {
    return genericTypeError("fieldName", v);
}

function defaultValueTypeError(v: unknown) {
    return genericTypeError("default value", v);
}

function valueTypeError(v: unknown) {
    return genericTypeError("value", v);
}

const checkString = t.string.is;
const checkNumber = t.number.is;
const checkInt = t.Int.is;

const StringOrNumber = t.union([t.string, t.number]);

function ensureStringFieldName(s: unknown): string {
    if (!checkString(s)) {
        throw fieldNameTypeError(s);
    }
    return s;
}

function ensureNumberDefault(s: unknown): number {
    if (!checkNumber(s)) {
        throw defaultValueTypeError(s);
    }
    return s;
}

function ensureIntDefault(s: unknown): number {
    if (!checkInt(s)) {
        throw defaultValueTypeError(s);
    }
    return s;
}

function ensureStringDefault(s: unknown): string {
    if (!checkString(s)) {
        throw defaultValueTypeError(s);
    }
    return s;
}

function ensureNumberLikeValue(s: unknown): number {
    if (!StringOrNumber.is(s)) {
        throw valueTypeError(s);
    }
    if (typeof s === "string") {
        const v = parseFloat(s.replace(",", "."));
        if (isNaN(v)) {
            throw valueTypeError(s);
        }
        return v;
    }
    return s;
}

function ensureStringLikeValue(s: unknown): string {
    if (!StringOrNumber.is(s)) {
        throw valueTypeError(s);
    }
    if (typeof s === "number") {
        return s.toString();
    }
    return s;
}

const ABSOLUTE_FIELD_REGEX = /^[0-9]+\./;

class StatCounter {
    private n = 0;
    private sum = 0;
    private min = 1e100;
    private max = -1e100;
    private k = 0;
    private ex = 0;
    private ex2 = 0;

    constructor() {
    }

    addValue(v: number) {
        if (v === undefined) {
            return;
        }
        // See: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
        if ( this.n == 0 ) { this.k = v; }
        this.n++;
        this.sum += v;
        const d = v - this.k;
        this.ex += d;
        this.ex2 += d * d;
        if (v < this.min) {
            this.min = v;
        }
        if (v > this.max) {
            this.max = v;
        }
    }

    getStat(): IStatData {
        let sd = 0;
        const dd = (this.ex2 -  (this.ex * this.ex) / this.n);
        if ( this.n > 1) { sd =  Math.sqrt(dd / (this.n - 1)); }
        return {
            n: this.n,
            sum: this.sum,
            avg: this.sum / this.n,
            min: this.min,
            max: this.max,
            sd: sd,
        };
    }
}

class Tools {
    private output = "";
    private errors: IError[] = [];
    private result: {[index: string]: unknown} = {};
    public outdata: object = {};

    constructor(
        private data: UserFieldDataT,
        private currDoc: string,
        private markup: IJsRunnerMarkup,
        private aliases: AliasDataT,
        private statCounters: { [fieldname: string]: StatCounter },
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

    private checkAliasAndNormalize(fieldName: string) {
        if (fieldName in this.aliases) {
            return this.normalizeField(this.aliases[fieldName]);
        }
        return this.normalizeField(fieldName);
    }

    getStudentName(): string {
        return this.data.user.real_name;
    }

    getDouble(fieldName: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName(fieldName);
        const def = ensureNumberDefault(defa);
        const s = this.normalizeAndGet(f);
        if (checkNumber(s)) {
            return this.handlePossibleNaN(s, s, def);
        }
        if (s === null || s === undefined  ) {
            return def;
        }
        if (typeof s !== "string") {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        const st = s.trim();
        if ( st == "" ) { return def;  }
        const sp = st.replace(",", ".");
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
        this.reportError(`Found value '${s}' of type ${typeof s}, using default value ${def}`);
        return def;
    }

    getInt(fieldName: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName(fieldName);
        const def = ensureIntDefault(defa);
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
        const f = ensureStringFieldName(fieldName);
        const def = ensureStringDefault(defa);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        // if (!checkString(s)) {  // if number this returns error :-(
        //     return this.reportInputTypeErrorAndReturnDef(s, def);
        // }
        // @ts-ignore
        const st: string = s;
        return st.toString();
    }

    getValue(fieldName: unknown, def: unknown = "") {
        const f = ensureStringFieldName(fieldName);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        return s;
    }

    // private statCounters: { [fieldname: string]: StatCounter } = {};

    addStatDataValue(fieldName: string, value: number) {
        // if ( !this.canStat ) { throw new Error("tools can not stat!, use gtools."); }
        let sc: StatCounter = this.statCounters[fieldName];
        if ( sc === undefined ) { sc = new StatCounter(); this.statCounters[fieldName] = sc; }
        sc.addValue(value);
    }

    addStatData(fieldName: unknown, start: number, end: number, max: unknown = 1e100) {
        // if ( !this.canStat ) { throw new Error("tools can not stat!, use gtools."); }
        const f = ensureStringFieldName(fieldName);
        const maxv = ensureNumberDefault(max);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        for (let i = start; i <= end; i++) {
            const name = f + i.toString();
            let v = this.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            // this.print(name + ": " + v);
            this.addStatDataValue(name, v);
        }
    }

    getStatData(): {[name: string]: IStatData} {
        // if ( !this.canStat ) { throw new Error("tools can not stat!, use gtools."); }
        const result: {[name: string]: IStatData} = {};
        for (const [name, sc] of Object.entries(this.statCounters)) {
            result[name] = sc.getStat();
        }
        return result;
    }

    getSum(fieldName: unknown, start: number, end: number, defa: unknown = 0, max: unknown = 1e100): number {
        const f = ensureStringFieldName(fieldName);
        const def = ensureNumberDefault(defa);
        const maxv = ensureNumberDefault(max);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        let sum = 0;
        for (let i = start; i <= end; i++) {
            sum += Math.min(this.getDouble(f + i.toString(), def), maxv);
        }
        return sum;
    }

    setString(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName(fieldName);
        const c = ensureStringLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        this.result[fn] = c;
    }

    setInt(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName(fieldName);
        const c = ensureNumberLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        if (!checkInt(c)) {
            throw valueTypeError(content);
        }
        this.result[fn] = c;
    }

    setDouble(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName(fieldName);
        const c = ensureNumberLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        this.result[fn] = c;
    }

    getDefaultPoints(): number {
        if (!this.markup.defaultPoints) {
            throw new Error("defaultPoints have not been set.");
        }
        return this.markup.defaultPoints;
    }

    getGrade(points: unknown): string | number {
        if (!checkNumber(points)) {
            throw new Error("points must be number.");
        }
        if (!this.markup.gradingScale) {
            throw new Error("gradingScale has not been set.");
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
        const fn = this.checkAliasAndNormalize(d);
        this.result[fn] = gradeVal;
        const c = this.markup.creditField || "credit";
        const fnc = this.checkAliasAndNormalize(c);
        let p;
        if (points !== undefined) {
            p = ensureNumberLikeValue(points);
            if (!checkInt(p)) {
                throw new Error("points is not integer.");
            }
        } else {
            p = 0;
        }
        this.result[fnc] = p;
    }

    static defineTime(s: unknown): number {
        if (!checkString(s)) {
            throw valueTypeError(s);
        }
        // TODO: fix timezone to work locally
        const localDateTime = new Date(s);
        const offset = localDateTime.getTimezoneOffset() * 60;
        return (localDateTime.getTime() / 1000) + offset;
    }

    getDateTime(fieldName: unknown, defa: unknown = NaN): number {
        const f = ensureStringFieldName(fieldName);
        const def = ensureNumberDefault(defa);
        return this.getDouble(f, def);
    }

     print(...args: unknown[]) {
        let sep = "";
        for (const a of args) {
            this.output += sep + a;
            sep = " ";
        }
        this.output += "\n";
    }

    getResult() {
        return {user: this.data.user.id, fields: this.result};
    }

    getOutput() {
        return this.output;
    }

    clearOutput() {
        this.output = "";
    }

    getErrors() {
        return this.errors;
    }

    private reportError(msg: string) {
        this.errors.push({msg, stackTrace: new Error().stack});
    }
}

export default Tools;
