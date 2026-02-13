export const timDateRegex =
    /^(?:\d{4}-\d{2}-\d{2}(?:[ T]\d{2}:\d{2}(?::\d{2})?)?|\d{2}:\d{2}(?::\d{2})?)$/;
export const timNumberRegex = /^-?\d+([.,:]\d+)?([eE]-?\d+)?$/;

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
type FilterItem = {
    func: FilterComparator;
    value: NumStr;
    strValue: string;
    isStr: boolean;
};
type FilterList = FilterItem[];

const filterComparatorOperators = {
    "!==": (a: NumStr, b: NumStr) => a !== b,
    "!=": (a: NumStr, b: NumStr) => a !== b,
    "<=": (a: NumStr, b: NumStr) => a <= b,
    "==": (a: NumStr, b: NumStr) => a === b,
    "!!": (a: NumStr, b: NumStr) => a == b,
    ">=": (a: NumStr, b: NumStr) => a >= b,
    "<": (a: NumStr, b: NumStr) => a < b,
    "=": (a: NumStr, b: NumStr) => a === b,
    ">": (a: NumStr, b: NumStr) => a > b,
    "!": (a: NumStr, b: NumStr) => a !== b,
};

const filterComparatorOperatorsKeys = Object.keys(filterComparatorOperators);

function parseFilter(fltr: string): FilterList {
    const ops = filterComparatorOperatorsKeys;

    const results: FilterList = [];

    if (fltr === "") {
        return [
            {
                func: filterComparatorOperators["="],
                value: "",
                strValue: "",
                isStr: true,
            },
        ];
    }

    let i = 0;
    while (i < fltr.length) {
        let func: FilterComparator | null = null;
        let operator = "";

        // is beginning of the operator
        for (const op of ops) {
            if (fltr.startsWith(op, i)) {
                func =
                    filterComparatorOperators[
                        op as keyof typeof filterComparatorOperators
                    ];
                i += op.length;
                operator = op;
                break;
            }
        }

        // Find the index of the next operator
        let nextOpIndex = fltr.length;

        for (const op of ops) {
            const idx = fltr.indexOf(op, i);
            if (idx !== -1 && idx < nextOpIndex) {
                nextOpIndex = idx;
            }
        }

        // Cut the value
        const value = fltr.slice(i, nextOpIndex);
        const strValue = value.toLowerCase();
        const [val, isString] = getCompareValue(value);
        if (value === "" && !func) {
            // If no operator and value is empty, use equality operator
            func = filterComparatorOperators["="];
        }
        if (!func || operator === "!!") {
            const re = new RegExp(value, "i");
            func = (a: NumStr, _: NumStr) => {
                return re.test("" + a);
            };
        }
        if (operator === "!") {
            const re = new RegExp(value, "i");
            func = (a: NumStr, _: NumStr) => {
                return !re.test("" + a);
            };
        }

        results.push({
            func: func,
            value: val,
            strValue: strValue,
            isStr: isString,
        });

        i = nextOpIndex;
    }

    return results;
}
export class ComparatorFilter {
    filterItems: FilterList;
    negate: boolean = false;
    constructor(fltr: string) {
        // Check for negation at the beginning of the filter.
        if (
            fltr.startsWith("!") &&
            !fltr.startsWith("!=") &&
            !fltr.startsWith("!!")
        ) {
            fltr = fltr.slice(1);
            this.negate = true;
        }
        this.filterItems = parseFilter(fltr);
    }

    public static makeNumFilter(s: string): ComparatorFilter {
        return new ComparatorFilter(s);
    }

    public test(s: string): boolean {
        const [n, forceString] = getCompareValue(s);
        const lcs = s.toLowerCase();
        let res = true;
        for (const item of this.filterItems) {
            if (s === "" && !item.isStr) {
                res = false;
                break;
            }
            let v = item.value;
            let ns = n;
            if (forceString) {
                v = item.strValue;
            } else if (item.isStr) {
                ns = lcs;
            }
            if (!item.func(ns, v)) {
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
