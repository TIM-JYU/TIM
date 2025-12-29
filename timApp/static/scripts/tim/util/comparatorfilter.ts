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
 * >2<4!  => find all numbers not in range ]2,4[
 * See: https://regex101.com/r/yfHbaH/3
 */
// FIXME: https://github.com/TIM-JYU/TIM/issues/3538
// const numFilterEx: RegExp = /([<=>!]=?) *(-?[\w.,]*) *(!?) */g;
const numFilterEx: RegExp = /([<=>!]=?) *(-?[\w.,\s]*)/g;
type NumStr = number | string;
type FilterComparator = (a: NumStr, b: NumStr) => boolean;
const filterComparatorOperators = {
    "<": (a: NumStr, b: NumStr) => a < b,
    "<=": (a: NumStr, b: NumStr) => a <= b,
    "=": (a: NumStr, b: NumStr) => a === b,
    "==": (a: NumStr, b: NumStr) => a === b,
    "!==": (a: NumStr, b: NumStr) => a !== b,
    "!=": (a: NumStr, b: NumStr) => a !== b,
    "!": (a: NumStr, b: NumStr) => a !== b,
    ">": (a: NumStr, b: NumStr) => a > b,
    ">=": (a: NumStr, b: NumStr) => a >= b,
};

export class ComparatorFilter {
    values: NumStr[] = [];
    funcs: FilterComparator[] = [];
    negate: boolean = false;
    // m: RegExpExecArray |
    // null = [];
    constructor(fltr: string) {
        // this.m = numFilterEx.exec(fltr);
        // if ( !this.m ) { return; }
        // if ( fltr.indexOf("!") >= 0 ) { fltr = fltr.replace("!", ""); this.negate = true; }
        for (
            let result = numFilterEx.exec(fltr);
            result !== null;
            result = numFilterEx.exec(fltr)
        ) {
            const op = result[1] as keyof typeof filterComparatorOperators; // We know that it'll be one of the operators.
            this.funcs.push(filterComparatorOperators[op]);
            const vs = result[2];
            // if (result[3]) {
            //     this.negate = true;
            // }
            let v: NumStr;
            try {
                v = parseFloat(vs);
                if (isNaN(v)) {
                    v = vs.toLowerCase();
                }
            } catch (e) {
                v = vs.toLowerCase();
            }
            this.values.push(v);
        }
    }

    public static isNumFilter(s: string): boolean {
        if (!s) {
            return false;
        }
        // return ( !!numFilterEx.exec(s) ); // eats first match?
        return "!<=>".includes(s[0]);
    }

    public static makeNumFilter(s: string): ComparatorFilter | null {
        if (!this.isNumFilter(s)) {
            return null;
        }
        return new ComparatorFilter(s);
    }

    public test(s: string): boolean {
        let n: NumStr = "";
        try {
            n = parseFloat(s);
            if (isNaN(n)) {
                n = s.toLowerCase();
            }
        } catch {
            n = s.toLowerCase();
        }
        let res = true;
        for (let i = 0; i < this.values.length; i++) {
            if (!this.funcs[i](n, this.values[i])) {
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
        row: uiGrid.IGridRowOf<T>,
        column: uiGrid.IGridColumnOf<T>
    ) => {
        if (cellValue == null) {
            return searchTerm === "" || searchTerm === "=";
        }
        cellValue = cellValue.toString();
        const comp = ComparatorFilter.makeNumFilter(searchTerm);
        if (comp) {
            return comp.test(cellValue);
        } else {
            return new RegExp(searchTerm, "i").test(cellValue);
        }
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
