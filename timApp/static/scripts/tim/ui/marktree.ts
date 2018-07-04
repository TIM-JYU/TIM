/* MarkTree JavaScript code
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Miika Nurminen, 12.7.2004.
 *
 * 20090302/mn: MarkTree now supports clicking from bullets in Opera. Clicking in text (not links) expands/collapses
 * tree items
 */

let lastnode: Element | null = null;
let listnodes: NodeListOf<HTMLLIElement> = null!;

// up, left, down, right, keypress codes
// ijkl
// var keys = new Array(105,106,107,108);
// num arrows
// var keys = new Array(56,52,50,54);
// wasd
// var press2 = new Array(119,97,115,100);
const press = [47, 45, 42, 43];

// keydown codes
//  var keys2=new Array(87,65,83,68);
const keys = [38, 37, 40, 39];

function is_exp(n: Element | null) {
    if (n == null) { return false; }
    return ((n.className == "exp") || (n.className == "exp_active"));
}

function is_col(n: Element | null) {
    if (n == null) { return false; }
    return ((n.className == "col") || (n.className == "col_active"));
}

function is_basic(n: Element | null) {
    if (n == null) { return false; }
    return ((n.className == "basic") || (n.className == "basic_active"));
}

function is_list_node(n: Element | null) {
    if (n == null) { return false; }
    if (n.className == null) { return false; }
    if ((is_exp(n)) ||
        (is_col(n)) ||
        (is_basic(n))) {
        return true;
    } else { return false; }
}

function set_lastnode(n: Element) {
    /*var d = new Date();
     var t_mil = d.getMilliseconds();*/
    // testattu nopeuksia explorerilla, ei merkittäviä eroja
    if (lastnode == n) { return; }
    /*  deactivate(lastnode)
     lastnode=n;
     activate(lastnode);*/

    /*
     if (is_active(lastnode) >= 0)
     toggle_class(lastnode);*/
    lastnode = n;
    /*
     if (!(is_active(lastnode) >= 0))
     toggle_class(lastnode);*/

    /*var d2 = new Date();
     var t_mil2 = d2.getMilliseconds();*/
}

function getsub(li: Element) {
    if (li.children.length == 0) { return null; }
    for (let c = 0; c < li.children.length; c++) {
        if ((li.children[c].className == "sub") || (li.children[c].className == "subexp")) {
            return li.children[c];
    }
        }
}

function find_listnode_recursive(li: Element): Element | null {
    if (is_list_node(li)) { return li; }
    if (li.children.length == 0) { return null; }
    let result = null;
    for (let c = 0; c < li.children.length; c++) {
        result = find_listnode_recursive(li.children[c]);
        if (result != null) { return result; }
    }
    return null;
}

function next_child_listnode(li: Element | null) {
    if (li == null) {
        return null;
    }
    let result = null;
    for (let i = 0; i < li.children.length; i++) {
        result = find_listnode_recursive(li.children[i]);
        if (result != null) { return result; }
    }
    return null;
}

function parent_listnode_rec(li: Element, recursive: number): Element | null {
    // added 12.7.2004 to prevent IE error when readonly mode==true
    if (li == null) { return null; }
    let n: Element | null = li;
    while (recursive > 0) {
        n = n.parentElement;
        if (n == null) { return null; }
        if (is_list_node(n)) { return n; }
        recursive--;
    }
    return null;
}

function onClickHandler(evt: MouseEvent) {
    // cross-browser way to detect right click.
    // from: http://www.quirksmode.org/js/events_properties.html
    // this is actually needed only in Gecko-based browsers
    const e: MouseEvent = evt;
    let rightclick;
    if (e.which) { rightclick = (e.which == 3); } else if (e.button) { rightclick = (e.button == 2); }
    if (rightclick) { return true; }

    let temp;
    if (lastnode == null) {
        listnodes = document.getElementsByTagName("li");
        lastnode = listnodes[1];
        temp = listnodes[1];
    }

    // event.srcElement is needed for explorer
    let target = evt.target as Element;
    if ((target.nodeName.toLowerCase() == "a") || (target.nodeName.toLowerCase() == "ul")) { return true; }
    if (!is_list_node(target)) {
        const ptarget = parent_listnode_rec(target, 1);
        if (ptarget == null) { return true; }
        target = ptarget;
    }

    toggle(target);
    set_lastnode(target);
    // return true;

    // we handled the event - stop any default actions
    // from: exlorertree,  http://www.scss.com.au/scripts/explorertree.js
    /*  evt.returnValue = false;
     if (evt.preventDefault) {
     evt.preventDefault();
     }*/
    return true;

}

function setSubClass(node: Element, name: string) {
    const sub = getsub(node);
    if (sub == null) { return; }

    // to prevent bug in ie when expanding empty list
    if ((name == "subexp") && (next_child_listnode(node) == null)) { return; }

    sub.className = name;
}

function toggle(target: Element) {
    if (!is_list_node(target)) { return; }
    if (is_col(target)) {
        target.className = "exp";
        setSubClass(target, "sub");
    } else if (is_exp(target)) {
        target.className = "col";
        setSubClass(target, "subexp");
    }

}
export function addEvents() {
    document.onclick = onClickHandler;
}
