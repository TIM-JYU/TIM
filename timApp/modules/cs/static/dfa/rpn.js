/*!
* Class for deterministic finite automate
*/
class DFA {

    /*!
     * Convert string to dfa
     * \fn constructor(s)
     * \param string s DFA as a string representation
     * \param dict params, syntaxes what syntaxes area allowed
     * \return JSON resulting DFA structure
     */
    constructor(s, params) {
        // regexps for various syntaxes
        // syntax 1:  0: S1 -> S2
        // syntax 2:  S1 0-> S2
        // syntax 3:  +S1 S3 S2       https://regex101.com/r/uowlQQ/latest
        // syntax Table:  = | a b c   https://regex101.com/r/xwijfX/latest
        // syntax Trow:   1 | 2 3 4   https://regex101.com/r/Tdv6Qs/latest
        // start node: ->S1
        const re1 = /^ *([^ >:+-]+): *([^ >:+-]+) *-?>? *([^ >:+-]+) *$/;
        const re2 = /^ *([^ >:+-]+) +([^ >:+-]+) *(->|-|>| ) *([^ >:+-]+) *$/;
        const re3 = /^\+ *([^ |;,]+)[ |;,]+(.*) *$/;
        const reTable = /^ *= *\| *(.*)$/;
        const reTrow = /^ *(\S+) * *\| *(.*)$/;
        const reStart = /^ *->? *([^ >:+-]+)$/;
        const rePos = /^ *(\S*) *\/ *(\S+)$/;

        params = params || {};
        let syntaxes = params["syntaxes"] || "123";
        let allowstar = params["allowstar"] || false;

        this.arcs = [];
        this.nodes = {};
        this.positions = {};
        this.first = null;
        this.errors = "";
        this.staticerrors = "";
        let errors = "";
        this.columns = [0,1];
        this.params = params;
        let columns = this.columns;

        let firstFound = undefined;
        let lastUsed = undefined;
        let dfa = this;
        let colnames = "";

        // Helper functions
        function addLNode(s) { // remembers last name
            lastUsed = addNode(s);
            return lastUsed;
        }


        function addNode(s) {
            let accept = false;
            if (s.indexOf('*') >= 0) accept = true;
            s = s.replace("*", "").trim();
            if (s === ".") s = lastUsed;
            if (s === undefined) s = "???";
            let node = dfa.nodes[s];
            if (!node) node = {name: s, arcs: {}, error: "", cnt: 0};
            if (accept) node.accept = true;
            dfa.nodes[s] = node;
            if (!firstFound) firstFound = s;
            return s;
        }


        function addArc(value, from, to) {
            let label;
            if (typeof value === 'string' || value instanceof String) {
                value = value.trim();
                label = value.replace(/_/g, " ");
                value = value.replace(/_/g, "");
            } else {
                label = "" + value;
            }
            if (colnames && !columns.includes(value) && value !== "*" && value !== -1) {
                errors += "Illegal transition: " + value + "\n";
                return;
            }
            const arc = {value: value, label: label, from: from, to: to};
            dfa.arcs.push(arc);
            if (value === -1) return;
            const node = dfa.nodes[from];
            if (node.arcs[value]) {
                node.error += "Node " + node.name + ": dublicate transition " + value + "\n";
                node.dublicate = true;
            }
            node.arcs[value] = arc;
        }


        // Checking lines from string representation
        for (let line of s.split("\n")) {
            line = line.trim();
            if (!line) continue; // forget empty lines
            if (line.startsWith("--")) continue; // forget lines with ---
            if (line.startsWith("#")) continue; // forget lines with #
            if (line.startsWith("//")) continue; // forget lines with //

            let r = reTable.exec(line);
            if (r && colnames === "") { // Table column headers, only first accepted
                colnames = r[1].trim().replace(/[ ,;|]+/g, " ");
                this.columns = colnames.split(" ");
                columns = this.columns;
                continue;
            }

            r = re3.exec(line); // syntax 3: +S1 S2 S3
            if (!r) r = reTrow.exec(line); // syntax: Trow 1 | 2 3 4
            if (r && syntaxes.includes("3")) {
                const from = r[1];
                const to = r[2].trim().replace(/[ ,;|]+/g, " ").split(" ");
                const n = Math.min(to.length, this.columns.length);
                const f = addLNode(from);
                for (let i=0; i<n; i++) {
                    const t = addNode(to[i]);
                    addArc(this.columns[i], f, t);
                }
                continue;
            }

            r = reStart.exec(line); // syntax ->1
            if (r) { // start node
                const n = addLNode(r[1])
                addArc(-1, 'startpoint', n);
                this.first = this.nodes[n];
                continue;
            }

            r = re1.exec(line);
            if (r && syntaxes.includes("1")) { // syntax 1: 1: S1 -> S2
                const v = r[1];
                const f = addLNode(r[2]);
                const t = addNode(r[3]);
                addArc(v, f, t);
                continue;
            }

            r = re2.exec(line);
            if (r && syntaxes.includes("2")) { // syntax 2: S1 1-> S2
                const v = r[2];
                const f = addLNode(r[1]);
                const t = addNode(r[4]);
                let vs = v.split(",");
                for (let val of vs)
                    addArc(val, f, t);
                continue;
            }

            r = rePos.exec(line);
            if (r) { // syntax: S1/3,2
                let n = r[1];
                const p = r[2];
                if (n === "" || n.includes(">")) n = "startpoint";
                this.positions[n] = p;
                continue;
            }
            this.staticerrors += 'Illegal line: ' + line + "\n";
        }


        // check if there is no start node
        if (!this.first && firstFound) {
            addArc(-1, 'startpoint', firstFound);
            this.first = this.nodes[firstFound];
        }


        // check if self transitions are needed
        for (let n in this.nodes) {
            const node = this.nodes[n];
            let count = 0;
            let starFound = false;
            let cols = colnames;
            for (let a in node.arcs) {
                count++;
                if (a === "*") {
                    starFound = true;
                    break;
                }
                cols = cols.replace(a, "");
            }
            cols = cols.trim();
            if (cols !== "" && !starFound) {
                if (allowstar)
                    addArc("*", n, n);
                else {
                    node.error += "Node " + n + ": missing transitions: " + cols + "\n";
                }
            }
            if (node.error) this.errors += node.error;
        }
        this.staticerrors += errors;
    }


    /*!
     * Check if input s is accepted by this dfa
     * \param s input to check
     * \return boolean true if accpeted
     */
    accepts(s) {
        this.cnt = 0;
        for (const node of Object.values(this.nodes)) node.cnt = 0;
        for (const arc of Object.values(this.arcs)) arc.cnt = 0;
        let active = this.first;
        if (!active) return false;
        active.cnt = 1;
        this.cnt++;
        for (let value of s) {
            let activeArc = active.arcs[value];
            if (!activeArc) activeArc = active.arcs['*'];
            if (!activeArc) return false;
            active = this.nodes[activeArc.to];
            activeArc.cnt++;
            if (active.cnt === 0) this.cnt++;
            active.cnt++;

        }
        if (!active || !active.accept) return false;
        return active.accept;
    }
} // DFA

