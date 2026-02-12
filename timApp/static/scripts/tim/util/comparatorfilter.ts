export const timDateRegex =
    /^(?:\d{4}-\d{2}-\d{2}(?:[ T]\d{2}:\d{2}(?::\d{2})?)?|\d{2}:\d{2}(?::\d{2})?)$/;
export const timNumberRegex = /^-?\d+([.,:]\d+)?([eE]-?\d+)?$/;

/**
 * NumFilter class for filtering numbers.
 *
 * <2     => find all numbers less than 2
 * <=2    => find all numbers less or equal than 2
 * >2     => find all numbers greter than 2
 * >=2    => find all numbers greter or equal than 2
 * =2     => find all numbers equal to 2
 * >2<4   => find all numbers in range ]2,4[
 * >=2<=4 => find all numbers in range [2,4]
 * !>2<4  => find all numbers not in range ]2,4[
 * See: https://regex101.com/r/yfHbaH/3
 */
// const numFilterEx: RegExp = /([<=>!]=?) *(-?[\w.,]*) *(!?) */g;

type NumStr = number | string;

export function getCompareValue(vs: string): [NumStr, boolean] {
    try {
        if (timNumberRegex.test(vs)) {
            const v = parseFloat(vs.replace(",", ".").replace(":", "."));
            if (isNaN(v)) {
                return [vs.toLowerCase(), true];
            }
            return [v, false];
        }
    } catch (e) {}
    return [vs.toLowerCase(), true];
}

type FilterComparator = (a: NumStr, b: NumStr) => boolean;
const filterComparatorOperators = {
    "<": (a: NumStr, b: NumStr) => a < b,
    "<=": (a: NumStr, b: NumStr) => a <= b,
    "=": (a: NumStr, b: NumStr) => a === b,
    "==": (a: NumStr, b: NumStr) => a === b,
    // "!==": (a: NumStr, b: NumStr) => a !== b,
    // "!=": (a: NumStr, b: NumStr) => a !== b,
    // "!": (a: NumStr, b: NumStr) => a !== b,
    ">": (a: NumStr, b: NumStr) => a > b,
    ">=": (a: NumStr, b: NumStr) => a >= b,
};

function buildOperatorRegex(operators: Record<string, unknown>): RegExp {
    const escapedOps = Object.keys(operators)
        .sort((a, b) => b.length - a.length)
        .map((op) => op.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"));
    const opPattern = escapedOps.join("|");

    // Prefix ! optional, operator valinnainen (puuttuva = regex), value valinnainen
    return new RegExp(`(${opPattern})?([^<>=]*)(?=(?:${opPattern})|$)`, "g");
}

const numFilterEx = buildOperatorRegex(filterComparatorOperators);

export class ComparatorFilter {
    values: NumStr[] = [];
    strings: string[] = [];
    funcs: FilterComparator[] = [];
    negate: boolean = false;
    // m: RegExpExecArray |
    // null = [];
    constructor(fltr: string) {
        // this.m = numFilterEx.exec(fltr);
        // if ( !this.m ) { return; }
        if (fltr.startsWith("!")) {
            fltr = fltr.replace("!", "");
            this.negate = true;
        }
        numFilterEx.lastIndex = 0;
        let result = numFilterEx.exec(fltr);
        while (result !== null) {
            // this.negate = this.negate || !!result[1]; // If "!" is present before the operator, set negate to true.
            let op = result[1] as keyof typeof filterComparatorOperators; // We know that it'll be one of the operators.
            const vs = result[2];
            if (!op && vs === "") {
                op = "=";
            }
            if (op) {
                this.funcs.push(filterComparatorOperators[op]);
            } else {
                // If no operator is specified, default to regexp.
                const re = new RegExp(vs, "i");
                this.funcs.push((a: NumStr, _: NumStr) => {
                    return re.test("" + a);
                });
            }
            this.values.push(getCompareValue(vs)[0]);
            this.strings.push(vs.toLowerCase());
            if (fltr === "" || result[0] === "") {
                break;
            }
            result = numFilterEx.exec(fltr);
            if (result == null || result[0] === "") {
                break;
            }
        }
    }

    public static makeNumFilter(s: string): ComparatorFilter {
        return new ComparatorFilter(s);
    }

    public test(s: string): boolean {
        const [n, forceString] = getCompareValue(s);
        const lcs = s.toLowerCase();
        let res = true;
        for (let i = 0; i < this.values.length; i++) {
            let v = this.values[i];
            let ns = n;
            if (forceString) {
                v = this.strings[i];
            } else if (typeof v === "string") {
                ns = lcs;
            }
            if (!this.funcs[i](ns, v)) {
                res = false;
                break;
            }
        }
        return this.negate ? !res : res;
    }
}

export function withComparatorFilters<T>(
    params: Array<uiGrid.IColumnDefOf<T>>
) {
    const matchFn = (
        searchTerm: string,
        cellValue: string | number | null | undefined,
        _row: uiGrid.IGridRowOf<T>,
        _column: uiGrid.IGridColumnOf<T>
    ) => {
        if (cellValue == null) {
            return searchTerm === "" || searchTerm === "=";
        }
        cellValue = cellValue.toString();
        const comp = ComparatorFilter.makeNumFilter(searchTerm);
        return comp.test(cellValue);
    };
    return params.map((param) => ({
        ...param,
        filter: {
            ...param.filter,
            condition: matchFn,
            rawTerm: true,
        },
    }));
}
