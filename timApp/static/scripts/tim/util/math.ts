/* Utility math functions for dealing with numbers, ranges, etc.
 * NOTE: Keep this file free from external dependencies.
 */

/**
 * Clamps a number value to within the given range of values.
 * @param num value to clamp
 * @param min minimum return value
 * @param max maximum return value
 */
export function clamp(num: number, min: number, max: number) {
    return num < min ? min : num > max ? max : num;
}
