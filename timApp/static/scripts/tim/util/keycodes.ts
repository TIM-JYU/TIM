export const KEY_1 = 49;
export const KEY_2 = 50;
export const KEY_3 = 51;
export const KEY_4 = 52;
export const KEY_5 = 53;
export const KEY_B = 66;
export const KEY_DOWN = 40;
export const KEY_ENTER = 13;
export const KEY_ESC = 27;
export const KEY_I = 73;
export const KEY_LEFT = 37;
export const KEY_O = 79;
export const KEY_RIGHT = 39;
export const KEY_S = 83;
export const KEY_TAB = 9;
export const KEY_UP = 38;
export const KEY_Y = 89;
export const KEY_F2 = 113;

export function isArrowKey(keyCode: number) {
    switch (keyCode) {
        case KEY_DOWN:
        case KEY_UP:
        case KEY_LEFT:
        case KEY_RIGHT:
            return true;
        default:
            return false;
    }
}
