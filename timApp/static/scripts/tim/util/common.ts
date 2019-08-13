// This module is meant to contain code that is usable in both client and server.
// So do not, for example, define anything DOM-related in this module.

const TASK_PROG = /([\w.]*)\( *(\d*) *, *(\d*) *\)(.*)/;

/**
 * Return fields widened, so string "d(1,4);dsum" comes out as
 * a list ["d1, "d2", "d3", "dsum"].
 * @param fields string/list to widen
 */
export function widenFields(fields: string | string[]): string[] {
    let fields1: string[] = [];
    if (!(fields instanceof Array)) {
        fields = fields.split(";");
    }
    for (const field of fields) {
        const parts = field.split(";");
        fields1 = fields1.concat(parts);
    }

    const rfields: string[] = [];
    for (const ffield of fields1) {
        const field = ffield.trim();
        if (!field) {
            continue;
        }
        const parts = field.split("=");
        let a = "";
        const tf = parts[0].trim();
        if (parts.length > 1) {
            a = parts[1].trim();
        }
        const m = TASK_PROG.exec(tf);
        if (!m) {
            rfields.push(field);
            continue;
        }

        const tb = m[1];
        const n1 = parseInt(m[2], 10);
        const n2 = parseInt(m[3], 10);
        const te = m[4];

        for (let i = n1; i <= n2; i++) {
            let tn = tb + i + te;
            if (!tb) {
                tn = "";
            }
            if (a) {
                tn += "=" + a + i;
            }
            rfields.push(tn);
        }
    }
    return rfields;
}
