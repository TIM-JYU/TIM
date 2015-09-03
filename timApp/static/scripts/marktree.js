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
 * 20090302/mn: MarkTree now supports clicking from bullets in Opera. Clicking in text (not links) expands/collapses tree items
 */


/* cross-browser (tested with ie5, mozilla 1 and opera 5) keypress detection */

function get_keycode(evt) {
    // IE
    code = document.layers ? evt.which
        : document.all ? event.keyCode // event.keyCode!=evt.keyCode!
        : evt.keyCode;

    if (code == 0)
        code = evt.which; // for NS
    return code;
}

var lastnode = null;
var last_focused_node = null;
var listnodes = null;
var list_index = 1;
var lastnodetype = ''; // determines if node is a link, input or text;

// up, left, down, right, keypress codes
//ijkl
//var keys = new Array(105,106,107,108);
//num arrows
//var keys = new Array(56,52,50,54);
//wasd
// var press2 = new Array(119,97,115,100);
var press = new Array(47, 45, 42, 43);

// keydown codes
//  var keys2=new Array(87,65,83,68);
var keys = new Array(38, 37, 40, 39);

// keyset 1 = keydown, otherwise press
function checkup(keyset, n) {
    if (keyset == 1) return (n == keys[0]);
    return ((n == press[0]) /*|| (n==press2[0])*/)
}

function checkdn(keyset, n) {
    if (keyset == 1) return (n == keys[2]);
    return ((n == press[2]) /*|| (n==press2[2])*/)
}

function checkl(keyset, n) {
    if (keyset == 1) return (n == keys[1]);
    return ((n == press[1]) /*|| (n==press2[1])*/)
}

function checkr(keyset, n) {
    if (keyset == 1) return (n == keys[3]);
    return ((n == press[3]) /*|| (n==press2[3])*/)
}


function is_exp(n) {
    if (n == null) return false;
    return ((n.className == 'exp') || (n.className == 'exp_active'));
}

function is_col(n) {
    if (n == null) return false;
    return ((n.className == 'col') || (n.className == 'col_active'));
}

function is_basic(n) {
    if (n == null) return false;
    return ((n.className == 'basic') || (n.className == 'basic_active'));
}


/* returns i>=0 if true */
function is_active(node) {
    if (node.className == null) return false
    return node.className.indexOf('_active');
}

function toggle_class(node) {
    if ((node == null) || (node.className == null)) return;
    str = node.className;
    result = "";
    i = str.indexOf('_active');
    if (i > 0)
        result = str.substr(0, i);
    else
        result = str + "_active";
    node.className = result;
    return node;
}

/*
 function activate(node) {
 node.style.backgroundColor='#eeeeff';
 }

 function deactivate(node) {
 node.style.backgroundColor='#ffffff';
 }*/

function is_list_node(n) {
    if (n == null) return false;
    if (n.className == null) return false;
    if ((is_exp(n)) ||
        (is_col(n)) ||
        (is_basic(n)))
        return true; else return false;
}


function get_href(n) {
    alist = n.attributes;
    if (alist != null) {
        hr = alist.getNamedItem('href');
        if (hr != null) return hr.nodeValue;
    }
    if (n.childNodes.length == 0) return '';
    for (var i = 0; i < n.childNodes.length; i++) {
        s = get_href(n.childNodes[i]);
        if (s != '') return s;
    }
    return '';
}

function get_link(n) {
    if (n == null) return null;
    if (n.style == null) return null;

    // disabling uncontrolled recursion to prevent error messages on IE
    // when trying to focus to invisible links (readonly mode)
    if ((n.nodeName.toLowerCase() == 'ul') && (n.className == 'sub')) return null;

    if (n.nodeName.toLowerCase() == 'a') return n;
    if (n.childNodes.length == 0) return null;
    for (var i = 0; i < n.childNodes.length; i++) {
        s = get_link(n.childNodes[i]);
        if (s != null) return s;
    }
    return null;
}

function set_lastnode(n) {
    /*var d = new Date();
     var t_mil = d.getMilliseconds();*/
// testattu nopeuksia explorerilla, ei merkittäviä eroja
    if (lastnode == n) return;
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

function next_list_node() {
    tempIndex = list_index;
    while (tempIndex < listnodes.length - 1) {
        tempIndex++;
        var x = listnodes[tempIndex];
        if (is_list_node(x)) {
            list_index = tempIndex;
            return;
        }
    }
}

function prev_list_node() {
    tempIndex = list_index;
    while (tempIndex > 0) {
        tempIndex--;
        var x = listnodes[tempIndex];
        if (is_list_node(x)) {
            list_index = tempIndex;
            return;
        }
    }
}


function getsub(li) {
    if (li.childNodes.length == 0) return null;
    for (var c = 0; c < li.childNodes.length; c++)
        if ((li.childNodes[c].className == 'sub') || (li.childNodes[c].className == 'subexp'))
            return li.childNodes[c];
}

function find_listnode_recursive(li) {
    if (is_list_node(li)) return li;
    if (li.childNodes.length == 0) return null;
    result = null;
    for (var c = 0; c < li.childNodes.length; c++) {
        result = find_listnode_recursive(li.childNodes[c]);
        if (result != null) return result;
    }
    return null;
}

function next_child_listnode(li) {
    var result = null;
    for (var i = 0; i < li.childNodes.length; i++) {
        result = find_listnode_recursive(li.childNodes[i]);
        if (result != null) return result;
    }
    return null;
}

function next_actual_sibling_listnode(li) {
    if (li == null) return null;
    var temp = li;
    while (1) {
        var n = temp.nextSibling;
        if (n == null) {
            n = parent_listnode(temp);
            return next_actual_sibling_listnode(n);
        }
        if (is_list_node(n)) return n;
        temp = n;
    }
}

function next_sibling_listnode(li) {
    if (li == null) return null;
    var result = null;
    var temp = li;
    if (is_col(temp)) {
        result = next_child_listnode(temp);
        if (result != null) return result;
    }
    while (1) {
        var n = temp.nextSibling;
        if (n == null) {
            n = parent_listnode(temp);
            return next_actual_sibling_listnode(n);
        }
        if (is_list_node(n)) return n;
        temp = n;
    }
}

function last_sibling_listnode(li) {
    if (li == null) return null;
    var temp = li;
    var last = null;
    while (1) {
        var n = temp.nextSibling;
        if (is_list_node(temp))
            last = temp;
        if (n == null) {
            if (is_col(last)) return last_sibling_listnode(next_child_listnode(last));
            else return last;
        }
        temp = n;
    }
}

function prev_sibling_listnode(li) {
    if (li == null) return null;
    var temp = li;
    var n = null;
    while (1) {
        n = temp.previousSibling;
        if (n == null) {
            return parent_listnode(li);
        }
        if (is_list_node(n)) {
            if ((is_col(n)) && (next_child_listnode(n) != null)) {
                return last_sibling_listnode(next_child_listnode(n));
            }
            else {
                return n;
            }
        }
        temp = n;
    }
}


function parent_listnode_rec(li, recursive) {
    // added 12.7.2004 to prevent IE error when readonly mode==true
    if (li == null) return null;
    n = li;
    while (recursive > 0) {
        n = n.parentNode;
        if (n == null) return null;
        if (is_list_node(n)) return n;
        recursive--;
    }
    return null;
}


function parent_listnode(li) {
    return parent_listnode_rec(li, 20);
    // added 12.7.2004 to prevent IE error when readonly mode==true
    /*  if (li==null) return null;
     n=li;
     while (1) {
     n=n.parentNode;
     if (n==null) return null;
     if (is_list_node(n)) return n;
     }*/
}

function getVisibleParents(id) {
    var n = document.getElementById(id);
    while (1) {
        expand(n);
        n = parent_listnode(n);
        if (n == null) return;
    }
}

function onClickHandler(evt) {
    // cross-browser way to detect right click. 
    // from: http://www.quirksmode.org/js/events_properties.html
    // this is actually needed only in Gecko-based browsers
    var e, rightclick;
    if (!evt) var e = window.event; // IE
    else e = evt;
    if (e.which) rightclick = (e.which == 3);
    else if (e.button) rightclick = (e.button == 2);
    if (rightclick) return true;

    if (lastnode == null) {
        listnodes = document.getElementsByTagName('li');
        lastnode = listnodes[1];
        temp = listnodes[1];
    }

    // event.srcElement is needed for explorer
    var target = evt ? evt.target : event.srcElement;
    if ((target.nodeName.toLowerCase() == 'a') || (target.nodeName.toLowerCase() == 'ul')) return true;
    if (!is_list_node(target)) {
        target = parent_listnode_rec(target, 1);
        if (target == null) return true;
    }

    toggle(target);
    set_lastnode(target);
    // return true;

    // we handled the event - stop any default actions
    //from: exlorertree,  http://www.scss.com.au/scripts/explorertree.js
    /*  evt.returnValue = false;
     if (evt.preventDefault) {
     evt.preventDefault();
     }*/
    return true;

}


function expand(node) {
    if (!is_exp(node)) return;
    //		  if (next_child_listnode(lastnode)!=null) {

    if (node.className == 'exp_active')
        node.className = 'col_active';
    else
        node.className = 'col';

    setSubClass(node, 'subexp');
    //			    }

}

function collapse(node) {
    if (!is_col(node)) return;

    if (node.className == 'col_active')
        node.className = 'exp_active'
    else
        node.className = 'exp';

    setSubClass(node, 'sub');
    //getsub(node).className='sub';

}

function setSubClass(node, name) {
    sub = getsub(node);
    if (sub == null) return;

    //to prevent bug in ie when expanding empty list
    if ((name == 'subexp') && (next_child_listnode(node) == null)) return;

    sub.className = name;
}

function toggle(target) {
    if (!is_list_node(target)) return;
    if (is_col(target)) {
        target.className = 'exp';
        setSubClass(target, 'sub');
    }
    else if (is_exp(target)) {
        target.className = 'col';
        setSubClass(target, 'subexp');
    }

}

function expandAll(node) {
    if (node.className == 'exp') {
        node.className = 'col';
        setSubClass(node, 'subexp');
        //getsub(node).className='subexp';
    }
    var i;
    if (node.childNodes != null)
    //if (node.hasChildNodes())
        for (i = 0; i < node.childNodes.length; i++)
            expandAll(node.childNodes[i]);
}

function collapseAll(node) {
    if (node.className == 'col') {
        node.className = 'exp';
        setSubClass(node, 'sub');
        //getsub(node).className='sub';
    }
    var i;
    if (node.childNodes != null)
    // for opera   if (node.hasChildNodes())
        for (i = 0; i < node.childNodes.length; i++)
            collapseAll(node.childNodes[i]);
}


function unFocus(node) {
    // unfocuses potential link that is to be hidden (if a==null there is no link so it should not be blurred).
    // tested with mozilla 1.7, 12.7.2004. /mn (
    intemp = parent_listnode(node);
    //      a = get_link(intemp);     // added 6.4. to get keyboard working with
    // moved before collapse to prevent an error message with IE when readonly==true
    //      if (a!=null) a.blur(); // netscape after collapsing a focused node
    // the above doesn't always work on readonly mode, so added an explicit focus
    // variable
    if (last_focused_node != null) last_focused_node.blur();
    last_focused_node = null;
    return intemp;
}


// mode: 0==keypress, 1==keyup
function keyfunc(evt, mode) {
    var c = get_keycode(evt);
    var temp = null;
    var a = null;

    if (lastnode == null) {
        listnodes = document.getElementsByTagName('li');
        lastnode = listnodes[1];
        temp = listnodes[1];
    }

    if (checkup(mode, c)) { // i
        temp = prev_sibling_listnode(lastnode);
    }
    else if (checkdn(mode, c)) { // k
        temp = next_sibling_listnode(lastnode);
    }
    else if (checkr(mode, c)) { // l
        expand(lastnode);
        a = get_link(lastnode);
        if (a != null) {
            a.focus();
            last_focused_node = a;
        }
        else {/*self.focus(); last_focused_node=self;*/
        }
    }
    else if (checkl(mode, c)) { // j
        if (is_col(lastnode)) {
            unFocus(lastnode);
            collapse(lastnode);
        }
        else {
            temp = unFocus(lastnode);
            collapse(temp);
        }
        //if (temp==null) lastnode.focus(); // forces focus to correct div (try mozilla typesearch) (doesn't seem to work -mn/6.4.2004)
    }
    else return;
    if (temp != null) set_lastnode(temp);
    return true;
}

/*
 function keytest(evt) {
 return keyfunc(evt, 1);
 }


 function presstest(evt) {
 return keyfunc(evt, 0);
 }*/

/*
 // Gecko selects text when bullets are clicked on - stop it!
 // taken from: explorertree: http://www.scss.com.au/scripts/explorertree.js
 */
function stopGeckoSelect(evt) {
    if (!evt) return true; //only needed in gecko
    //var evt = window.event;
    if (evt.preventDefault) {
        evt.preventDefault();
    }
    return true;
}

function initEvents(currentElement) {
    if (!currentElement.childNodes || currentElement.childNodes.length == 0) return;
    for (var i = 0; i < currentElement.childNodes.length; i++) {
        var item = currentElement.childNodes[i];
        if (item.nodeName.toLowerCase() == 'li') {
            //item.onclick=onClickHandler;
        }
        else {
            if (item.nodeName.toLowerCase() == 'ul') {
                //item.onclick=onClickHandler;
                //item.onmousedown=stopGeckoSelect;
            }
        }
        initEvents(item);
    }
}

function addEvents() {
    initEvents(document);
    document.onclick = onClickHandler;
}

/*document.onkeypress = presstest;
 document.onkeyup = keytest;*/
window.onload = addEvents;
  