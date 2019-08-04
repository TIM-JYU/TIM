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

const STAT_ERROR =  "tools can not stat!, use gtools.";

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

function round(c: number, decim: number): number {
    if ( decim == null ) { return c; }
    const mul = Math.pow(10, decim);
    return Math.round(c * mul) / mul;
}

const ABSOLUTE_FIELD_REGEX = /^[0-9]+\./;

interface Point {
    x: number;
    y: number;
}

interface Linear { // y = a + bb
    a: number;
    b: number;
}

class LineFitter {
    // see: http://mathworld.wolfram.com/LeastSquaresFitting.html
    private n = 0;
    private sumX  = 0;
    private sumX2 = 0;
    private sumXY = 0;
    private sumY  = 0;
    private sumY2 = 0;
    private minX = 1e100;
    private maxX = -1e100;
    private minY = 1e100;
    private maxY = -1e100;
    readonly xname: string;
    readonly yname: string;
    private cab: Linear | null = null;

    constructor(xname: string, yname: string) {
        this.xname = xname;
        this.yname = yname;
    }

    add(x: number, y: number): Point {
        if ( !isNaN(x) &&  !isNaN(y) ) {
            this.n++;
            this.sumX += x;
            this.sumX2 += x * x;
            this.sumXY += x * y;
            this.sumY += y;
            this.sumY2 += y * y;
            if (x < this.minX) { this.minX = x; }
            if (x > this.maxX) { this.maxX = x; }
            if (y < this.minY) { this.minY = y; }
            if (y > this.maxY) { this.maxY = y; }
            this.cab = null;
        }
        return {x: x, y: y};
    }

    addxy(xy: any): Point {
        this.add(xy.x, xy.y);
        return xy;
    }

    addField(tools: Tools): Point {
        return this.add(tools.getDouble(this.xname, NaN), tools.getDouble(this.yname, NaN));
    }

    ab(): Linear {
        if ( this.cab ) { return this.cab; }
        const div = this.n * this.sumX2 - this.sumX * this.sumX;
        const a = (this.sumY * this.sumX2  - this.sumX * this.sumXY) / div;
        const b = (this.n * this.sumXY - this.sumX * this.sumY) / div;
        this.cab = {a: a, b: b};
        return this.cab;
    }

    f(x: number): number {
        const ab = this.ab();
        return ab.a + ab.b * x;
    }

    limits() {
        return {minX: this.minX, maxX: this.maxX, minY: this.minY, maxY: this.maxY, n: this.n};
    }

    r2() {
        const ssxx = this.sumX2 - this.sumX * this.sumX / this.n;
        const ssyy = this.sumY2 - this.sumY * this.sumY / this.n;
        const ssxy = this.sumXY - this.sumX * this.sumY / this.n;
        return (ssxy * ssxy) / (ssxx * ssyy);
    }

    r() {
        return Math.sqrt(this.r2());
    }

    r2string(decim: number) {
        return "r² = " + round(this.r2(), decim);
    }

    rstring(decim: number) {
        return "r = " + round(this.r(), decim);
    }

    line(xdecim: number, ydecim: number): Point[] {
        const x1 = round(this.minX, xdecim);
        const x2 = round(this.maxX, xdecim);
        const y1 = round(this.minY, ydecim);
        const y2 = round(this.maxY, ydecim);
        return [{x: x1, y: y1}, {x: x2, y: y2}];
    }
}

class Distribution {
    public labels: number[] = [];
    public data: number[]   = [];
    private n = 0;
    private readonly fieldName: string;

    constructor(n1: number, n2: number, mul: number, fieldName: string) {
        for (let i = n1; i <= n2; i++) {
            this.labels[i] = i * mul;
            this.data[i] = 0;
        }
        this.fieldName = fieldName;
    }

    // Add to closest category
    add(x: number): number {
         if ( isNaN(x) ) { return x; }
         let mini = 0;
         let mind = 1e100;
         for (let i = 0; i < this.labels.length; i++) {
             const d = Math.abs(this.labels[i] - x);
             if ( d <= mind ) { mind = d; mini = i; }
         }
         this.data[mini]++;
         this.n++;
         return x;
    }

    addField(tools: Tools): number {
         const x = tools.getDouble(this.fieldName, NaN);
         return this.add(x);
    }

    get() {
        return { labels: this.labels, data: this.data, n: this.n };
    }
}

class XY {
    public data: object[] = [];
    private readonly xname: string;
    private readonly yname: string;

    constructor(xname: string, yname: string) {
        this.xname = xname;
        this.yname = yname;
    }

    add(x: number, y: number) {
        if ( !isNaN(x) && !isNaN(y) ) {
            this.data.push({x: x, y: y});
        }
        return {x: x, y: y};
    }

    addField(tools: Tools): object {
        return this.add(tools.getDouble(this.xname, NaN), tools.getDouble(this.yname, NaN));
    }
}

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

class Stats {
    private counters: { [fieldname: string]: StatCounter } = {};
    constructor(fields: string | string[]) {
    }
}

class Tools {
    private output = "";
    private errors: IError[] = [];
    private result: {[index: string]: unknown} = {};
    public outdata: object = {};
    public fitters: { [fieldname: string]: LineFitter } = {};
    public dists: { [fieldname: string]: Distribution } = {};
    public xys: { [fieldname: string]: XY } = {};
    public stats: { [name: string]: Stats } = {};
    private statCounters: { [fieldname: string]: StatCounter } = {};
    private usePrintLine: boolean = false; // if used println at least one time then print does not do nl

    constructor(
        private data: UserFieldDataT,
        private currDoc: string,
        private markup: IJsRunnerMarkup,
        private aliases: AliasDataT,
        private canStat: boolean = false,
    ) {
    }

    private checkStatError() {
        if ( !this.canStat ) { throw new Error(STAT_ERROR); }
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

    getRealName(): string {
        return this.data.user.real_name;
    }

    getUserName(): string {
        return this.data.user.name;
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

    getNumber(s: string) {
        const r = parseFloat(s);
        return this.handlePossibleNaN(r, s, 0);
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

    createLimitArray(table: string | string[]): number[][] {
        const res: number[][] = [];
        if ( !(table instanceof Array) ) { table = table.split("\n"); }
        for (const s of table) {
            const parts = s.split(",");
            if ( parts.length < 2 ) { return this.reportInputTypeErrorAndReturnDef(table, []); }
            const limit = this.getNumber(parts[0]);
            const value = this.getNumber(parts[1]);
            res.push([limit, value]);
        }
        return res;
    }

    findLastOf(limits: number[][], c: number, def: number = 0) {
        let res = def;
        for (const r of limits) {
            const limit: number = r[0];
            const value: number = r[1];
            if ( c >= limit ) { res = value; }
        }
        return res;
    }

    findLast(table: string | string[], c: number, def: number = 0) {
        return this.findLastOf(this.createLimitArray(table), c, def);
    }

    createFitter(xname: string, yname: string) {
        this.checkStatError();
        const fitter = new LineFitter(xname, yname);
        this.fitters[xname + "_" + yname] = fitter;
        return fitter;
    }

    createDistribution(n1: number, n2: number, mul: number, fieldName: string) {
        this.checkStatError();
        const dist = new Distribution(n1, n2, mul, fieldName);
        if ( fieldName ) { this.dists[fieldName] = dist; }
        return dist;
    }

    createXY(xname: string, yname: string) {
        this.checkStatError();
        const xy = new XY(xname, yname);
        this.xys[xname + "_" + yname] = xy;
        return xy;
    }

    createStatCounter(name: string, fields: string | string[]) {
        this.checkStatError();
        const stats = new Stats(fields);
        this.stats[name] = stats;
        return stats;
    }

    // private statCounters: { [fieldname: string]: StatCounter } = {};

    addStatDataValue(fieldName: string, value: number) {
        this.checkStatError();
        let sc: StatCounter = this.statCounters[fieldName];
        if ( sc === undefined ) { sc = new StatCounter(); this.statCounters[fieldName] = sc; }
        sc.addValue(value);
    }

    addStatData(tools: Tools, fieldName: unknown, start: number, end: number, max: unknown = 1e100) {
        this.checkStatError();
        const f = ensureStringFieldName(fieldName);
        const maxv = ensureNumberDefault(max);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        for (let i = start; i <= end; i++) {
            const name = f + i.toString();
            let v = tools.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            this.print(name + ": " + v);
            this.addStatDataValue(name, v);
        }
    }

    addStatDataOf(tools: Tools, ...fieldNames: string[]) {
        this.checkStatError();
        const maxv = 1e100;
        for (const name of fieldNames) {
            let v = tools.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            // this.print(name + ": " + v);
            this.addStatDataValue(name, v);
        }
    }

    getStatData(): {[name: string]: IStatData} {
        this.checkStatError();
        const result: {[name: string]: IStatData} = {};
        for (const [name, sc] of Object.entries(this.statCounters)) {
            result[name] = sc.getStat();
        }
        return result;
    }

    r(value: number, decim: number): number {
        if ( !checkInt(decim) ) {
            throw new Error("Parameter 'decim' must be integer.");
        }
        const c = ensureNumberDefault(value);
        const mul = Math.pow(10, decim);
        return Math.round(c * mul) / mul;
    }

    round(value: number, decim: number): number {
        return this.r(value, decim);
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

    getSumOf(...fieldNames: string[]): number {
        const def = 0;
        const maxv = 1e100;
        let sum = 0;
        for (const fn of fieldNames) {
            sum += Math.min(this.getDouble(fn, def), maxv);
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
        if ( !this.usePrintLine ) { this.output += "\n"; }
    }

    println(...args: unknown[]) {
        // For be combatible with Korppi, if only print is used, it prints nl.
        // But if println is used at least one time before print, then print is
        // not printing nl.
        this.usePrintLine = true;
        this.print(args);
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
