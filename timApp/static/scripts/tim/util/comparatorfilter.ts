/**
 * NumFilter class for filtering numbers and strings
 * based on a filter string.
 * The filter string can contain multiple conditions
 * separated by operators.
 * All comparisons is made case-insensitive for strings.
 * The filter string can also be negated by adding a "!" at the end,
 * which negates the whole filter.
 * To get any symbol from ! < = > \ in the filter string,
 * you must escape it with a backslash \.
 * Examples of filter strings and their meanings:
 *
 * >      => find all non-empty strings
 * !      => find all non-empty strings
 * =      => find all empty strings
 * <2     => find all numbers less than 2, but also all non-empty strings
 *           starting with a symbol less than "2" in ASCII order,
 *           for example "1a", "0.5", "1:2" etc.
 * <=2    => find all numbers less or equal than 2
 * >2     => find all numbers greter than 2 also all
             non empty strings starting with a number greter than 2,
             for example "2a", "3.5", "4:5" etc.
             And also "cat" because it starts with "c"
             that is greter than "2" in string comparison.
 * \D     => only numeric values (like 3.2 5 0.5 -1 etc.) but not strings
 * \N     => only integer values (like 3 5 -1 etc.) but not strings and not decimal numbers
 * \N>2   => only integer values greater than 2, but not strings and not decimal numbers
 * \N!    => only non-integer string (like 3.2 0.5 cat etc.)
 * \D!    => only no number string (like 3% 0€ cat etc.)
 * !\d+>2 => find all numbers not greter than 2 and all strings,
 * >=2    => find all numbers greter or equal than 2
 * =2     => find all numbers equal to 2
 * >2<4   => find all numbers in range ]2,4[
 * >=2<=4 => find all numbers in range [2,4]
 * >2<4!  => find all numbers not in range ]2,4[ (note postfix ! negates the whole filter)
 * a      => find all strings that match the regex a
 * !a     => find all strings that do not match the regex a
 * !a!b   => find all strings that do not match the regex a and do not match the regex b
 * a!b    => find all strings that match the regex a and do not match the regex b
 * a|b    => find all strings that match the regex a or match the regex b
 * \!a    => find all strings that match the regex !a
 * =a     => find all strings that are equal to a
 * !=a    => find all strings that are not equal to a
 * !!a    => find all strings that match the regex a (same as a)
 * !a!    => find all strings that match the regex a (same as a)
 * See: https://regex101.com/r/yfHbaH/3
 */

export const timDateRegex =
    /^(?:\d{4}-\d{2}-\d{2}(?:[ T]\d{2}:\d{2}(?::\d{2})?)?|\d{2}:\d{2}(?::\d{2})?)$/;
export const timNumberRegex = /^-?\d+([.,:]\d+)?([eE]-?\d+)?$/;

type NumStr = number | string;

const ESC = "\u001F"; // some unlikely character to be used as escape character
const ESCAPABLE = "!<=>\\"; // keep \\ at the end for ENCODE_RE
const ENCODE_RE = new RegExp("\\\\([" + ESCAPABLE + "\\])", "g");
const DECODE_RE = new RegExp(ESC + "(\\d)", "g");
const DOUBLE_RE = /^[-+]?\d+([.,:]\d+)?([eE]-?\d+)?$/;
const INT_RE = /^[-+]?\d+$/;

function encodeEscapes(s: string): string {
    return s.replace(ENCODE_RE, (_, ch: string) => {
        const idx = ESCAPABLE.indexOf(ch);
        return idx >= 0 ? ESC + String(idx) : ch;
    });
}

function decodeEscapes(s: string): string {
    return s.replace(DECODE_RE, (_, code: string) => {
        return ESCAPABLE[Number(code)] ?? "";
    });
}

/**
 * Tries to parse the string as a number to get a comparable value.
 * If it succeeds to get a number, returns the number.
 * If it fails, returns the string itself as lowercase.
 * The second return value indicates whether the
 * original string is a string (true) or a number (false).
 * The function also handles some common number formats,
 * such as using comma or colon as decimal separator.
 * @param vs The string to parse.
 * @returns A tuple where the first element is the parsed
 *   number or the original string as lowercase,
 *   and the second element is a boolean indicating whether the original
 *   string is a string (true) or a number (false).
 */
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
type FilterOp = {
    func: FilterComparator;
    params: number; // number of parameters the operator takes (1 for unary operators like \N, 2 for binary operators like >)
};

const filterComparatorOperators: Record<string, FilterOp> = {
    "!==": {func: (a: NumStr, b: NumStr) => a !== b, params: 2},
    "!\\N": {func: (a: NumStr, _b: NumStr) => !INT_RE.test("" + a), params: 1}, // changed to !regexp
    "!\\D": {
        func: (a: NumStr, _b: NumStr) => !DOUBLE_RE.test("" + a),
        params: 1,
    }, // changed to !regexp
    "\\N": {func: (a: NumStr, _b: NumStr) => INT_RE.test("" + a), params: 1}, // changed to !regexp
    "\\D": {func: (a: NumStr, _b: NumStr) => DOUBLE_RE.test("" + a), params: 1}, // changed to !regexp
    "!=": {func: (a: NumStr, b: NumStr) => a !== b, params: 2},
    "<=": {func: (a: NumStr, b: NumStr) => a <= b, params: 2},
    "==": {func: (a: NumStr, b: NumStr) => a === b, params: 2},
    "!!": {func: (a: NumStr, b: NumStr) => a == b, params: 2}, // changed to regexp, se below
    ">=": {func: (a: NumStr, b: NumStr) => a >= b, params: 2},
    "<": {func: (a: NumStr, b: NumStr) => a < b, params: 2},
    "=": {func: (a: NumStr, b: NumStr) => a === b, params: 2},
    ">": {func: (a: NumStr, b: NumStr) => a > b, params: 2},
    "!": {func: (a: NumStr, b: NumStr) => a !== b, params: 2}, // changed to !regexp
};

const filterComparatorOperatorsKeys = Object.keys(filterComparatorOperators);

/**
 * Parses the filter string into a list of filter items,
 * where each item contains a comparator function,
 * a value to compare against, and information about
 * whether the value is a string or a number.
 * @param fltr The filter string to parse.
 * @returns A list of filter items parsed from the filter string.
 */
function parseFilter(fltr: string): FilterList {
    const ops = filterComparatorOperatorsKeys;

    const results: FilterList = [];

    if (fltr === "") {
        return [
            {
                func: filterComparatorOperators["="].func,
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
        let params = 2;

        // is beginning of the operator
        for (const op of ops) {
            if (fltr.startsWith(op, i)) {
                const oper = filterComparatorOperators[op];
                func = oper.func;
                i += op.length;
                operator = op;
                params = oper.params;
                break;
            }
        }

        let nextOpIndex = fltr.length;
        if (params < 2) {
            nextOpIndex = i;
        } else {
            // Find the index of the next operator
            for (const op of ops) {
                const idx = fltr.indexOf(op, i);
                if (idx !== -1 && idx < nextOpIndex) {
                    nextOpIndex = idx;
                }
            }
        }

        // Cut the value
        const value = decodeEscapes(fltr.slice(i, nextOpIndex));
        const strValue = value.toLowerCase();
        const [val, isString] = getCompareValue(value);
        if (value === "" && !func) {
            // If no operator and value is empty, use equality operator
            func = filterComparatorOperators["="].func;
        }
        if (!func || operator === "!" || operator === "!!") {
            let safeRegex = value;
            if (value.endsWith("\\") && !value.endsWith("\\\\")) {
                safeRegex += "\\";
            }
            const re = new RegExp(safeRegex, "i");

            // Valitaan valmiiksi oikea funktio
            func =
                operator === "!"
                    ? (a: NumStr, _: NumStr) => !re.test("" + a)
                    : (a: NumStr, _: NumStr) => re.test("" + a);
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
        fltr = encodeEscapes(fltr);
        // Check for negation at the end of the filter.
        if (fltr.endsWith("!")) {
            fltr = fltr.slice(0, -1); // remove the last !
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
