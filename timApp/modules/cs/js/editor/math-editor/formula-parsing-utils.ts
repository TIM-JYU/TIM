/**
 * Utility functions for parsing formulas.
 *
 * @author Janne Lahti
 * @license MIT
 * @date 27.4.2023
 */

import type {IEditor} from "../editor";
import type {FieldType, OldContent} from "./formula-editor.component";
import {FormulaType, FormulaPropertyList} from "./formula-types";

/**
 * Tuple which contains the type of formula, and its start and end index.
 */
export type FormulaTuple = [FormulaType, number, number];

/**
 * Objects to handle groups of information.
 */
export type NumPair = [number, number];
export type StringPair = [string, string];
export type StringTrio = [string, string, string];
type StringType = [string, FormulaType];
export type StringBool = [string, boolean];

/**
 * Splits string into two parts at cursor location if possible.
 * @param editor Editor containing the string to split.
 * @return String in parts.
 */
export function parseOldContent(editor: IEditor): OldContent {
    if (!editor.cursorPosition) {
        return {
            before: editor.content,
            editing: "",
            after: "",
        };
    }
    const cursorI = editor.cursorPosition();
    return {
        before: editor.content.slice(0, cursorI),
        editing: "",
        after: editor.content.slice(cursorI),
    };
}

/**
 * Finds the line in which a cursor is in a text.
 * @param editorContent Text where the line is looked for.
 * @param cursorLocation Cursor position as zero based index.
 * @return String of line with cursor.
 */
export function getCurrentLine(
    editorContent: string,
    cursorLocation: number
): string {
    let startI = cursorLocation;
    let endI = cursorLocation;
    if (editorContent[startI] === "\n") {
        startI--;
    }
    while (startI >= 0 && editorContent[startI] !== "\n") {
        startI--;
    }
    while (endI < editorContent.length && editorContent[endI] !== "\n") {
        endI++;
    }
    return editorContent.slice(startI + 1, endI);
}

/**
 * Finds $ and $$ syntax parenthesis from a string.
 * @param text String where the parenthesis are looked for.
 * @return Array containing types and indexes of parenthesis.
 */
function findParenthesisFromString(text: string): FormulaTuple[] {
    let currentIndex = 0;
    let stack = [-1, -1];
    const allParenthesis: FormulaTuple[] = [];

    // count all parenthesis from the beginning
    while (true) {
        // find next $ symbol
        currentIndex = text.indexOf("$", currentIndex);
        // stop if no $ symbol is found
        if (currentIndex < 0) {
            break;
        }
        // skip \$ symbols
        if (text.charAt(currentIndex - 1) === "\\") {
            currentIndex++;
            continue;
        }
        // multiline
        if (text.charAt(currentIndex + 1) === "$") {
            // opening does not exist
            if (stack[1] < 0) {
                // set opening to current
                stack[1] = currentIndex;

                // opening exists
            } else {
                // set closing to current
                allParenthesis.push([
                    FormulaType.Multiline,
                    stack[1],
                    currentIndex + 1,
                ]);
                // reset stack
                stack = [-1, -1];
            }
            currentIndex += 2;
            // inline
        } else {
            // opening does not exist
            if (stack[0] < 0) {
                // if possible set opening to current
                if (text.charAt(currentIndex + 1) !== " ") {
                    stack[0] = currentIndex;
                }
                // opening exists
            } else {
                // if possible set closing to current
                if (text.charAt(currentIndex - 1) !== " ") {
                    allParenthesis.push([
                        FormulaType.Inline,
                        stack[0],
                        currentIndex,
                    ]);
                    // reset stack
                    stack = [-1, -1];
                } else {
                    // if possible set opening to current
                    if (text.charAt(currentIndex + 1) !== " ") {
                        stack[0] = currentIndex;
                    }
                }
            }
            currentIndex++;
        }
    }
    return allParenthesis;
}

/**
 * Finds current formula from list of parenthesis.
 * @param allParenthesis List of parenthesis with types and indexes.
 * @param cursorIndex Index of the cursor.
 * @return Parenthesis in which the cursor is, or undefined if not in formula.
 */
function parseCurrentFormula(
    allParenthesis: FormulaTuple[],
    cursorIndex: number
): FormulaTuple {
    let beginning = 0;
    // check if cursor is inside any parenthesis
    for (const [formulaType, startI, endI] of allParenthesis) {
        // parenthesis are after cursor
        if (startI > cursorIndex) {
            return [FormulaType.NotDefined, beginning, startI - 1];
        }
        // parenthesis are before cursor
        if (endI < cursorIndex) {
            beginning = endI + 1;
            continue;
        }
        return [formulaType, startI, endI];
    }
    return [FormulaType.NotDefined, beginning, -1];
}

/**
 * Find the type of begin-end-style formula.
 * @param formula String where the formula is looked for.
 * @param searchIndex Zero based index of the last begin end keyword.
 * @return Type of formula.
 */
function beginEndKeyword(formula: string, searchIndex: number): FormulaType {
    let finalType = FormulaType.NotDefined;
    let finalIndex = Number.MAX_SAFE_INTEGER;
    // list begin-end-style formula types
    const types: StringType[] = FormulaPropertyList.filter(
        (type) => type.beginEndKeyword.length > 0
    ).map((type) => [type.beginEndKeyword, type.type]);
    // check which type is first after search index
    for (const type of types) {
        const foundIndex = formula.indexOf(type[0], searchIndex);
        if (foundIndex > 0 && foundIndex < finalIndex) {
            finalIndex = foundIndex;
            finalType = type[1];
        }
    }
    return finalType;
}

/**
 * Finds all begin and end syntax matrices from a string.
 * @param formula String where the matrices are looked for.
 * @return Array containing indexes of matrices.
 */
export function findMatrixFromString(formula: string): FormulaTuple[] {
    // temporary index variables
    let bIndex = 0;
    let eIndex = 0;
    // temporary stacks for found indexes
    const bStack: number[] = [];
    const eStack: number[] = [];
    const typeStack: FormulaType[] = [];
    // list of found matrices
    const allMatrices: FormulaTuple[] = [];

    while (true) {
        // find next begin and end keywords
        bIndex = formula.indexOf("\\begin{", bIndex);
        eIndex = formula.indexOf("\\end{", eIndex);
        // stop if no end is found
        if (eIndex < 0) {
            // add possible keywords from stacks to actual list
            if (eStack.length > 0) {
                allMatrices.push([
                    typeStack[typeStack.length - 1],
                    bStack[bStack.length - 1],
                    eStack[eStack.length - 1],
                ]);
            }
            break;
        }
        // try to find ends to existing begins, if no more begins are found
        if (bIndex < 0) {
            let i = bStack.length - 1;
            // run loop until all begins have end pairs, or no more ends are found
            while (i >= 0) {
                eIndex = formula.indexOf("\\end{", eIndex);
                if (eIndex < 0) {
                    break;
                }
                eStack.push(eIndex);
                typeStack.push(beginEndKeyword(formula, eIndex));
                eIndex += 5;
                i--;
            }
            // add only the outermost beginning and end to list
            allMatrices.push([
                typeStack[typeStack.length - 1],
                bStack[i + 1],
                eStack[eStack.length - 1],
            ]);
            break;
        }
        // begin and end are found, but begin is first
        if (bIndex < eIndex) {
            // try to add keywords from stacks to actual list
            if (eStack.length > 0) {
                if (
                    formula
                        .slice(eStack[eStack.length - 1], bIndex)
                        .includes("\n")
                ) {
                    allMatrices.push([
                        typeStack[typeStack.length - 1],
                        bStack[bStack.length - 1],
                        eStack[eStack.length - 1],
                    ]);
                    // reset stacks
                    typeStack.length = 0;
                    bStack.length = 0;
                    eStack.length = 0;
                    bStack.push(bIndex);
                } else {
                    // ignore begin and remove last end if there is no line break after last matrix
                    eStack.pop();
                }
            } else {
                // add begin to its stack if end stack is empty
                bStack.push(bIndex);
            }
            bIndex += 7;
        } else {
            // add end to stack only if it has a beginning pair
            if (bStack.length > eStack.length) {
                // remove inner begin and end from stacks
                if (eStack.length > 0) {
                    bStack.pop();
                    eStack.shift();
                }
                eStack.push(eIndex);
                typeStack.push(beginEndKeyword(formula, eIndex));
            }
            eIndex += 5;
        }
    }
    // shift indexes from the start of keyword to actual start and end of matrix
    for (const i of allMatrices.keys()) {
        const beginNewLine = formula.lastIndexOf("\n", allMatrices[i][1]);
        if (beginNewLine < 0) {
            allMatrices[i][1] = 0;
        } else {
            allMatrices[i][1] = beginNewLine + 1;
        }
        const endNewLine = formula.indexOf("\n", allMatrices[i][2]);
        if (endNewLine < 0) {
            allMatrices[i][2] = formula.length - 1;
        } else {
            allMatrices[i][2] = endNewLine - 1;
        }
    }
    return allMatrices;
}

/**
 * Tries to parse current formula at cursor location in editor content.
 * @param editorContent Text where formula is parsed.
 * @param cursorLocation Cursor position as zero based index.
 * @return Tuple containing formula info or undefined.
 */
export function parseEditedFormula(
    editorContent: string,
    cursorLocation: number
): FormulaTuple {
    // check if cursor is inside a $ syntax formula
    const text = editorContent;
    const allParenthesis = findParenthesisFromString(text);
    let currentFormula = parseCurrentFormula(allParenthesis, cursorLocation);

    if (currentFormula[0] === FormulaType.NotDefined) {
        if (currentFormula[2] < 0) {
            currentFormula[2] = text.length - 1;
        }
        // cursor wasn't inside a $ syntax formula
        // check if cursor is inside a begin-end formula
        const matrices: FormulaTuple[] = findMatrixFromString(
            text.slice(currentFormula[1], currentFormula[2] + 1)
        ).map((formulaTuple) => [
            formulaTuple[0],
            formulaTuple[1] + currentFormula[1],
            formulaTuple[2] + currentFormula[1],
        ]);
        currentFormula = parseCurrentFormula(matrices, cursorLocation);
    }
    return currentFormula;
}

/**
 * Parses parenthesis and formula contents from string.
 * @param editing Formula to parse.
 * @param start Parenthesis at start.
 * @param end Parenthesis at end.
 * @return String array containing start, end and formula between them.
 */
export function parseExistingParenthesis(
    editing: string,
    start: string,
    end: string
): StringTrio {
    const finalParenthesis = ["", ""];
    let formula = editing.slice(start.length, -end.length);
    let trimmed = formula.trimStart();
    let lenDiff = formula.length - trimmed.length;
    finalParenthesis[0] = editing.slice(0, start.length + lenDiff);
    formula = trimmed;

    trimmed = formula.trimEnd();
    lenDiff = formula.length - trimmed.length;
    finalParenthesis[1] = editing.slice(editing.length - end.length - lenDiff);
    return [finalParenthesis[0], trimmed, finalParenthesis[1]];
}

/**
 * Check if parsed formula has inner formula that should be handled.
 * @param formula Formula to be checked.
 * @param searchStart Keyword string to search from start of formula.
 * @param searchEnd Keyword string to search from end of formula.
 * @return Array containing inner start, end and formula.
 * If no inner formula was found, return array with
 * empty start and end, and original formula.
 */
export function checkInnerFormula(
    formula: string,
    searchStart: string,
    searchEnd: string
): StringTrio {
    let parts: StringTrio = ["", formula, ""];
    const start = formula.indexOf(searchStart);
    const end = formula.lastIndexOf(searchEnd);
    if (0 <= start && start < end) {
        parts = parseExistingParenthesis(formula, searchStart, searchEnd);
    }
    return parts;
}

/**
 * Trim a specific character from start and end of a string.
 * https://stackoverflow.com/questions/26156292/trim-specific-character-from-a-string
 * @param str String to be trimmed.
 * @param startChar Character to trim from start.
 * @param endChar Character to trim from end.
 * @return Trimmed string.
 */
function trimChar(str: string, startChar: string, endChar: string): string {
    let start = 0;
    let end = str.length;
    while (start < end && str[start] === startChar) {
        ++start;
    }
    while (end > start && str[end - 1] === endChar) {
        --end;
    }
    return start > 0 || end < str.length ? str.slice(start, end) : str;
}

/**
 * Splits text into lines of LaTeX.
 * @param formula Text to split.
 * @param allMatrices Indexes of possible matrices inside text.
 * @return Array containing the lines of LaTeX.
 */
export function getMultilineFormulaLines(
    formula: string,
    allMatrices: NumPair[]
): FieldType[] {
    // split all line breaks if no matrices exists
    if (allMatrices.length < 1) {
        return formula.split("\n").map((line) => {
            return {latex: trimChar(line, "&", "\\")};
        });
    } else {
        let allFields: string[] = [];
        // split and add lines before first matrix
        allFields = allFields.concat(
            formula.slice(0, allMatrices[0][0]).split("\n")
        );
        // add first matrix
        allFields.push(formula.slice(allMatrices[0][0], allMatrices[0][1] + 1));
        for (let i = 0; i < allMatrices.length - 1; i++) {
            // split and add lines between matrices
            allFields = allFields.concat(
                formula
                    .slice(allMatrices[i][1] + 1, allMatrices[i + 1][0])
                    .split("\n")
            );
            // add matrix after the lines
            allFields.push(
                formula.slice(allMatrices[i + 1][0], allMatrices[i + 1][1] + 1)
            );
        }
        // split and add lines after last matrix
        allFields = allFields.concat(
            formula
                .slice(
                    allMatrices[allMatrices.length - 1][1] + 1,
                    formula.length
                )
                .split("\n")
        );
        // remove empty lines and return trimmed lines
        allFields = allFields.filter((line) => line.length > 0);
        return allFields.map((line) => {
            return {latex: trimChar(line, "&", "\\")};
        });
    }
}

/**
 * Formats formula to LaTeX string.
 * @param formulaType Type of the formula that should be formatted.
 * @param fields Array containing the LaTeX of the formulas.
 * @param existingParenthesis Start and end marks for edited formula.
 * @param useExistingParenthesis True adds existing parenthesis around
 * formula and false adds defaults based on type.
 * @return Formatted string or undefined if resulting formula is empty.
 */
export function formatLatex(
    formulaType: FormulaType,
    fields: FieldType[],
    existingParenthesis: StringPair,
    useExistingParenthesis: boolean
): string | undefined {
    // Get properties for current formula type. Return undefined if not found.
    const formulaProperties = FormulaPropertyList.find(
        (type) => type.type === formulaType
    );
    if (!formulaProperties) {
        return undefined;
    }
    // single line formulas
    if (formulaProperties.join.length < 1) {
        // Single line formulas should only have 1 field.
        // Return undefined if that is empty.
        const latex = fields[0].latex;
        if (latex.length === 0) {
            return undefined;
        }
        // return with existing or default start and end marks
        return useExistingParenthesis
            ? `${existingParenthesis[0]}${latex}${existingParenthesis[1]}`
            : `${formulaProperties.start}${latex}${formulaProperties.end}`;
    }
    // Array with latex of formula lines. Filter out empty lines.
    const formulaLatexList = fields
        .map((field) => field.latex)
        .filter((text) => text.length > 0);
    // Join lines with defined string. Return undefined if joined string is empty.
    const latex = formulaLatexList.join(formulaProperties.join);
    if (latex.length === 0) {
        return undefined;
    }
    // return with existing or default start and end marks
    return useExistingParenthesis
        ? `${existingParenthesis[0]}${latex}${existingParenthesis[1]}`
        : `${formulaProperties.start}${latex}${formulaProperties.end}`;
}
