import * as t from "io-ts";
import {widenFields} from "tim/util/common";
import {IError, IJsRunnerMarkup, INumbersObject} from "../../shared/jsrunnertypes";
import {AliasDataT, UserFieldDataT} from "../servertypes";

/**
 * From name=alias list returns two lists
 * @param fields list of name=alias pairs
 */
function separateNamesAndAliases(fields: string[]): {names: string[], aliases: string[] } {
    const raliases: string[] = [];
    const rnames: string[] = [];
    for (const f of fields) {
        const parts = f.split("=");
        const fn = parts[0].trim();
        if ( !fn ) { continue; }
        if ( rnames.indexOf(fn) >= 0) { continue; }
        rnames.push(fn);
        if ( parts.length < 2) {
            raliases.push(fn);
        } else {
            raliases.push(parts[1].trim());
        }
    }
    return { names: rnames, aliases: raliases};
}

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

function round(c: number, decim: number): number {
    if ( decim == null || isNaN(decim) ) { return c; }
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

// const dummyGTools: GTools = new GTools(

class WithGtools {
    private gt?: GTools;

    constructor(gtools: GTools) {
        this.gt = gtools;
    }

    public clearGtools() {
        this.gt = undefined;
    }

    get gtools(): GTools {
        if ( !this.gt ) {
            throw new Error("Can not use tools anymore");
        }
        return this.gt;
    }

}

class LineFitter extends WithGtools {
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
    public readonly autoadd: boolean;

    constructor(gtools: GTools, xname: string, yname: string, autoadd: boolean = true) {
        super(gtools);
        this.autoadd = autoadd;
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

    addxy(xy: Point): Point {
        this.add(xy.x, xy.y);
        return xy;
    }

    addField(): Point {
        return this.add(this.gtools.tools.getDouble(this.xname, NaN), this.gtools.tools.getDouble(this.yname, NaN));
    }

    ab(adecim: number = NaN, bdecim: number = NaN): Linear {
        if ( this.cab ) { return this.cab; }
        const div = this.n * this.sumX2 - this.sumX * this.sumX;
        let a = (this.sumY * this.sumX2  - this.sumX * this.sumXY) / div;
        let b = (this.n * this.sumXY - this.sumX * this.sumY) / div;
        if ( !isNaN(adecim) ) { a = round(a, adecim); if ( isNaN(bdecim) ) { bdecim = adecim; }}
        if ( !isNaN(bdecim) ) { b = round(b, bdecim); }
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

    line(xdecim: number = NaN, ydecim: number = NaN): Point[] {
        if ( !isNaN(xdecim) && isNaN(ydecim)) { ydecim = xdecim; }
        const x1 = round(this.minX, xdecim);
        const x2 = round(this.maxX, xdecim);
        const y1 = round(this.f(x1), ydecim);
        const y2 = round(this.f(x2), ydecim);
        return [{x: x1, y: y1}, {x: x2, y: y2}];
    }
}

class Distribution extends WithGtools {
    public labels: number[] = [];
    public data: number[]   = [];
    private n = 0;
    private readonly fieldName: string;
    public readonly autoadd: boolean = true;

    constructor(gtools: GTools, fieldName: string, n1: number, n2: number, mul: number = 1, autoadd: boolean) {
        super(gtools);
        this.autoadd = autoadd;
        if ( mul == 0 ) { mul = 1; }
        for (let i = n1; i * mul <= n2 + 0.000001; i++) {
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

    addField(): number {
         const x = this.gtools.tools.getDouble(this.fieldName, NaN);
         return this.add(x);
    }

    get() {
        return { labels: this.labels, data: this.data, n: this.n };
    }
}

class XY extends WithGtools {
    public data: object[] = [];
    private readonly xname: string;
    private readonly yname: string;
    public readonly fitter: LineFitter;
    public readonly autoadd: boolean;

    constructor(gtools: GTools, xname: string, yname: string, autoadd: boolean = true) {
        super(gtools);
        this.autoadd = autoadd;
        this.xname = xname;
        this.yname = yname;
        this.fitter = new LineFitter(gtools, xname, yname);
    }

    add(x: number, y: number) {
        const pt = {x: x, y: y};
        if ( !isNaN(x) && !isNaN(y) ) {
            this.data.push(pt);
            this.fitter.add(x, y);
        }
        return pt;
    }

    addField(): object {
        return this.add(this.gtools.tools.getDouble(this.xname, NaN), this.gtools.tools.getDouble(this.yname, NaN));
    }

    clearGtools() {
        super.clearGtools();
        this.fitter.clearGtools();
    }
}

const defaultStatHeaders = ["n", "sum", "avg", "min", "max", "sd"];

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

    getStat(): { [hname: string]: number }  {
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

class Stats extends WithGtools {
    private counters: { [fieldname: string]: StatCounter } = {};
    readonly fields: string [] = [];
    readonly aliases: string [] = [];
    readonly autoadd: boolean;

    constructor(gtools: GTools, fields: string | string[], autoadd: boolean = true) {
        super(gtools);
        this.autoadd = autoadd;
        const fa = separateNamesAndAliases(widenFields(fields));
        const flds = fa.names;
        this.aliases = fa.aliases;
        for (const f of flds) {
            this.fields.push(f.trim());
            this.counters[f] = new StatCounter();
        }
    }

    addField() {
        const maxv = 1e100;
        for (const name of this.fields) {
            let v = this.gtools.tools.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            this.addValue(name, v);
        }
    }

    addValue(fieldName: string, value: number, max: number = 1e100) {
        let sc: StatCounter = this.counters[fieldName];
        if ( sc === undefined ) { sc = new StatCounter(); this.counters[fieldName] = sc; }
        if ( isNaN(value) ) { return; }
        const v = Math.min(value, max);
        sc.addValue(v);
    }

    addData(fieldName: string, start: number, end: number, max: unknown = 1e100) {
        const maxv = ensureNumberDefault(max);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        for (let i = start; i <= end; i++) {
            const name = fieldName + i.toString();
            let v = this.gtools.tools.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            this.addValue(name, v);
        }
    }

    addOf(...fieldNames: string[]) {
        const maxv = 1e100;
        const fields = widenFields([...fieldNames]);
        for (const name of fields) {
            let v = this.gtools.tools.getDouble(name, NaN);
            if ( isNaN(v) ) { continue; }
            v = Math.min(v, maxv);
            // this.print(name + ": " + v);
            this.addValue(name, v);
        }
    }

    getData(): {[name: string]: INumbersObject} {
        const result: {[name: string]: INumbersObject} = {};
        for (const [name, sc] of Object.entries(this.counters)) {
            result[name] = sc.getStat();
        }
        return result;
    }

    // noinspection JSUnusedGlobalSymbols
    getForTable(headers: string[] | string = "", decim: number = 2): unknown {
        if ( !headers ) { headers = defaultStatHeaders; }
        if ( !(headers instanceof Array) ) {
            headers = headers.split(";");
        }
        const matrix: unknown[] = [];
        const result = { headers: [""], matrix: matrix };
        for (const hs of headers) {
            if ( hs ) {
                result.headers.push(hs);
            }
        }
        const statData = this.getData();
        let keys = this.fields;
        let alis = this.aliases;
        if (keys.length == 0) { keys = Object.keys(statData); alis = keys; }
        for (let i = 0; i < keys.length; i++ ) {
            const f = keys[i];
            const a = alis[i];
            const stat = statData[f];
            if ( !stat ) { continue; }
            const row: unknown[] = [a];
            for (const hs of headers) {
                if ( !hs ) { continue; }
                const val: number = stat[hs];
                row.push((round(val, decim)));
            }
          matrix.push(row);
        }
        return result;
    }

    // noinspection JSUnusedGlobalSymbols
    getForGraph(fields: string | string[], item: string = "avg", decim: number = 2): unknown {
        const labels: string[] = [];
        const data: number[] = [];
        const result = { labels: labels, data: data };
        let flds = this.fields;
        let alis = this.aliases;
        const statData = this.getData();
        if ( fields ) {
            const fa = separateNamesAndAliases(widenFields(fields));
            flds = fa.names;
            alis = fa.aliases;
        }
        if (flds.length == 0 ) {
            flds = Object.keys(statData);
            alis = flds;
        }
        for (let i = 0; i < flds.length; i++ ) {
            const name = flds[i];
            const a = alis[i];
            const stat = statData[name];
            labels.push(a);
            if ( !stat ) {
                data.push(0);
            } else {
                data.push(round(stat[item], decim));
            }
        }
        return result;
    }
}

const REDOUBLE = /[^0-9,.e\-+]+/g;

export class ToolsBase {
    protected output = "";
    protected errors: IError[] = [];
    protected useDefComplaint: boolean = true;
    public usePrintLine: boolean = false; // if used println at least one time then print does not do nl
    constructor(
        protected currDoc: string,
        protected markup: IJsRunnerMarkup,
        protected aliases: AliasDataT,
    ) {
    }

    getNumber(s: string) {
        const r = parseFloat(s);
        return this.handlePossibleNaN(r, s, 0);
    }

    setUseDefComplaint(b: boolean) {
        this.useDefComplaint = b;
    }

    protected handlePossibleNaN<T>(r: number, s: unknown, def: T) {
        if (isNaN(r)) {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        return r;
    }

    protected reportInputTypeErrorAndReturnDef<T>(s: unknown, def: T) {
        if ( !this.useDefComplaint ) { return def; }
        this.reportError(`Found value '${s}' of type ${typeof s}, using default value ${def}`);
        return def;
    }

    public createLimitArray(table: string | string[]): number[][] {
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

    // noinspection JSMethodCanBeStatic
    public findLastOf(limits: number[][], c: number, def: number = 0) {
        let res = def;
        for (const r of limits) {
            const limit: number = r[0];
            const value: number = r[1];
            if ( c >= limit ) { res = value; }
        }
        return res;
    }

    public findLast(table: string | string[], c: number, def: number = 0) {
        return this.findLastOf(this.createLimitArray(table), c, def);
    }

    // noinspection JSMethodCanBeStatic
    public r(value: unknown, decim: unknown): number {
        if ( !checkInt(decim) ) {
            throw new Error("Parameter 'decim' must be integer.");
        }
        const c = ensureNumberDefault(value);
        const mul = Math.pow(10, decim);
        return Math.round(c * mul) / mul;
    }

    public round(value: unknown, decim: unknown): number {
        return this.r(value, decim);
    }

    // noinspection JSMethodCanBeStatic
    public wf(fields: string | string[]): string[]  {
        return widenFields(fields);
    }

    public print(...args: unknown[]) {
        let sep = "";
        for (const a of args) {
            let as = a;
            if (typeof a !== "string" && !(a instanceof String)) {
                as = JSON.stringify(a);
            }
            this.output += sep + as;
            sep = " ";
        }
        if ( !this.usePrintLine ) { this.output += "\n"; }
    }

    public println(...args: unknown[]) {
        // To be compatible with Korppi, if only print is used, it prints nl.
        // But if println is used at least one time before print, then print is
        // not printing nl.
        this.usePrintLine = true;
        this.print(...args);
        this.output += "\n";
    }

    public getOutput() {
        return this.output;
    }

    public clearOutput() {
        this.output = "";
    }

    public getErrors() {
        return this.errors;
    }

    public reportError(msg: string) {
        this.errors.push({msg, stackTrace: new Error().stack});
    }
}

export class GTools extends ToolsBase {
    public outdata: object = {};
    public fitters: { [fieldname: string]: LineFitter } = {};
    public dists: { [fieldname: string]: Distribution } = {};
    public xys: { [fieldname: string]: XY } = {};
    public stats: { [name: string]: Stats } = {};

    public tools: Tools;

    constructor(
        currDoc: string,
        markup: IJsRunnerMarkup,
        aliases: AliasDataT,
        tools: Tools,
    ) {
        super(currDoc, markup, aliases);
        this.tools = tools;
        this.createStatCounter("GLOBAL", "", false);
    }

    createFitter(xname: string, yname: string, autoadd: boolean = true) {
        const fitter = new LineFitter(this, xname, yname, autoadd);
        this.fitters[xname + "_" + yname] = fitter;
        return fitter;
    }

    createDistribution(fieldName: string, n1: number, n2: number, mul: number = 1, autoadd = true) {
        const dist = new Distribution(this, fieldName, n1, n2, mul, autoadd);
        if ( fieldName ) { this.dists[fieldName] = dist; }
        return dist;
    }

    addToDatas() {
        for (const datas of [this.dists, this.xys, this.fitters, this.stats]) {
            // noinspection JSUnusedLocalSymbols
            Object.entries(datas).forEach(
                ([key, da]) => {
                    const d = da as Distribution | XY | LineFitter | Stats;
                    if (d.autoadd) {
                        d.addField();
                    }
                });
        }
    }

    clearGtools() {
        for (const datas of [this.dists, this.xys, this.fitters, this.stats]) {
            // noinspection JSUnusedLocalSymbols
            Object.entries(datas).forEach(
                ([key, da]) => {
                    const d = da as Distribution | XY | LineFitter | Stats;
                    d.clearGtools();
                 });
        }
    }

    createXY(xname: string, yname: string, autoadd: boolean = true) {
        const xy = new XY(this, xname, yname, autoadd);
        this.xys[xname + "_" + yname] = xy;
        return xy;
    }

    createStatCounter(name: string, fields: string | string[], autoadd: boolean = true) {
        const stats = new Stats(this, fields, autoadd);
        this.stats[name] = stats;
        return stats;
    }

    addStatDataValue(fieldName: string, value: number) {
        this.stats.GLOBAL.addValue(fieldName, value);
    }

    addStatData(fieldName: string, start: number, end: number, max: number = 1e100) {
        this.stats.GLOBAL.addData(fieldName, start, end, max);
    }

    addStatDataOf(...fieldNames: string[]) {
        this.stats.GLOBAL.addOf(...fieldNames);
    }

    getStatData(): {[name: string]: INumbersObject} {
        return this.stats.GLOBAL.getData();
    }

    setTools(tools: Tools) {
        this.tools = tools;
    }
}

export class Tools extends ToolsBase {
    private result: {[index: string]: unknown} = {};
    constructor(
        protected data: UserFieldDataT,
        currDoc: string,
        markup: IJsRunnerMarkup,
        aliases: AliasDataT,
    ) {
        super(currDoc, markup, aliases);
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

    getStudentName(): string {  // alias for Korppi compatibility
        return this.getRealName();
    }

    getUserName(): string {
        return this.data.user.name;
    }

    getLeaveDate() {
        return this.data.groupinfo ? this.data.groupinfo.membership_end : undefined;
    }

    getDouble(fieldName: unknown, defa: unknown = 0): number {
        const f = ensureStringFieldName(fieldName);
        const def = ensureNumberDefault(defa);
        let s;
        try {
            s = this.normalizeAndGet(f);
            if (s === null || s === undefined) {
                return def;
            }
            const st = ("" + s).replace(REDOUBLE, "");
            if (st == "") {
                return def;
            }
            let sp = st.replace(",", ".");
            // this.println("sp1=" + sp);
            if ( sp.startsWith("e") ) { sp = "1" + sp; }
            // this.println("sp2=" + sp);
            const r = parseFloat(sp);
            return this.handlePossibleNaN(r, s, def);
        } catch (e) {
            if ( !this.useDefComplaint ) { return def; }
            this.reportError(`Error: ${e}. Found value '${s}' of type ${typeof s}, using default value ${def}`);
            return def;
        }
    }

    getInt(fieldName: unknown, defa: unknown = 0): number {
        const def = ensureIntDefault(defa);
        // const s = this.normalizeAndGet(f);
        const s = this.getDouble(fieldName, def);
        if (s === null || s === undefined || isNaN(s)) {
            return def;
        }
        const r = Math.trunc(s);
        return this.handlePossibleNaN(r, s, def);
    }

    getString(fieldName: unknown, defa: unknown = ""): string {
        const f = ensureStringFieldName(fieldName);
        const def = ensureStringDefault(defa);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        if (!StringOrNumber.is(s)) {
            return this.reportInputTypeErrorAndReturnDef(s, def);
        }
        return s.toString();
    }

    getValue(fieldName: unknown, def: unknown = "") {
        const f = ensureStringFieldName(fieldName);
        let s = this.normalizeAndGet(f);
        if (s === null || s === undefined) {
            s = def;
        }
        return s;
    }

    getSum(fieldName: unknown, start: unknown, end: unknown, defa: unknown = 0, max: unknown = 1e100): number {
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

    getRBi(fieldName: unknown, start: unknown, end: unknown, defa: number = -1): number {
        const f = ensureStringFieldName(fieldName);
        const def = defa;
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        let id = def;
        for (let i = start; i <= end; i++) {
            const name = f + i.toString();
            const rb = this.getString(name, "");
            if ( rb === "1" ) { id = i; break; }
        }
        return id;
    }

    getRBname(fieldName: unknown, start: unknown, end: unknown, defa: string = ""): string {
        const f = ensureStringFieldName(fieldName);
        const i = this.getRBi(f, start, end, -1);
        if ( i < 0 ) { return defa; }
        return f + i.toString();
    }

    getSumOf(...fieldNames: string[]): number {
        const def = 0;
        const maxv = 1e100;
        let sum = 0;
        const fields = widenFields([...fieldNames]);
        for (const fn of fields) {
            sum += Math.min(this.getDouble(fn, def), maxv);
        }
        return sum;
    }

    getArray(func: (fname: string) => unknown, fieldName: unknown, start: unknown, end: unknown): unknown[] {
        const f = ensureStringFieldName(fieldName);
        if (!(checkInt(start) && checkInt(end))) {
            throw new Error("Parameters 'start' and 'end' must be integers.");
        }
        const a = [];
        for (let i = start; i <= end; i++) {
            a.push(func.call(this, f + i.toString()));
        }
        return a;
    }

    getArrayOf(func: (fname: string) => unknown, ...fieldNames: string[]): unknown[] {
        const a = [];
        const fields = widenFields([...fieldNames]);
        for (const fn of fields) {
            a.push(func.call(this, fn));
        }
        return a;
    }

    setString(fieldName: unknown, content: unknown): void {
        const f = ensureStringFieldName(fieldName);
        const c = ensureStringLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        this.result[fn] = c;
        this.data.fields[fn] = c;
    }

    setInt(fieldName: unknown, content: unknown, maxNotToSave: number = -1000000000): void {
        const f = ensureStringFieldName(fieldName);
        const c = ensureNumberLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        if (!checkInt(c)) {
            throw valueTypeError(content);
        }
        if ( c <= maxNotToSave ) {
            if ( this.getValue(fieldName, "") !== "") { this.setString(fieldName, ""); }
            this.data.fields[fn] = "";
            return;
        }
        this.result[fn] = c;
        this.data.fields[fn] = c;
    }

    setDouble(fieldName: unknown, content: unknown, maxNotToSave: number = -1e100, decim: number = NaN): void {
        const f = ensureStringFieldName(fieldName);
        let c = ensureNumberLikeValue(content);
        const fn = this.checkAliasAndNormalize(f);
        if ( c <= maxNotToSave ) {
            if ( this.getValue(fieldName, "") !== "") { this.setString(fieldName, ""); }
            this.data.fields[fn] = "";
            return;
        }
        if ( !isNaN(decim) ) {
            c = this.round(c, decim);
        }
        this.result[fn] = c;
        this.data.fields[fn] = c;
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

    isBetter(gv1: unknown, gv2: unknown) {
        if ( gv1 == undefined ) { return false; }
        const st1 = String(gv1).toLowerCase();
        const st2 = String(gv2).toLowerCase();
        if ( st1 === "" ) { return false; }
        if ( st1 === "hyl" ) { return false; }
        return st1 > st2;
    }

    saveGrade(gradeVal: unknown, points: unknown = this.markup.defaultPoints) {
        const gf = this.markup.gradeField || "grade";
        const cf = this.markup.creditField || "credit";
        let p;
        if (points !== undefined) {
            p = ensureNumberLikeValue(points);
            if (!checkInt(p)) {
                throw new Error("points is not integer.");
            }
        } else {
            p = -1;
        }
        if ( !this.markup.overrideGrade ) {
            const oldGrade = this.getValue(gf);
            if ( this.isBetter(oldGrade, gradeVal) ) {
                this.print(this.getRealName() + ": not changing grade " + oldGrade + " to " + gradeVal + "\n");
                return;
            }
            const oldPoints = this.getDouble(cf, -1);
            if ( oldPoints > p ) {
                this.print(this.getRealName() + ": not changing credit " + oldPoints + " to " + points + "\n");
                return;
            }
        }
        const fng = this.checkAliasAndNormalize(gf);
        // this.println(this.getRealName() + ": " + gradeVal);
        this.result[fng] = gradeVal;
        if ( !points ) { return; }
        const fnc = this.checkAliasAndNormalize(cf);
        if ( p < 0 ) { p = ""; }
        this.result[fnc] = p;
    }

    // noinspection JSMethodCanBeStatic
    defineTime(s: unknown): number {
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

    getResult() {
        return {user: this.data.user.id, fields: this.result};
    }
}
