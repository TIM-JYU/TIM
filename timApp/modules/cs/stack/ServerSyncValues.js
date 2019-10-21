function getClosestParentWithSelectorOrOptional(elem, selector, optional) {
// Find closest upward parent that matches either selector or optional selector.
// if both are empty, return document

    if ( !selector && ! optional ) return document;

	// Element.matches() polyfill
	if (!Element.prototype.matches) {
	    Element.prototype.matches =
	        Element.prototype.matchesSelector ||
	        Element.prototype.mozMatchesSelector ||
	        Element.prototype.msMatchesSelector ||
	        Element.prototype.oMatchesSelector ||
	        Element.prototype.webkitMatchesSelector ||
	        function(s) {
	            var matches = (this.document || this.ownerDocument).querySelectorAll(s),
	                i = matches.length;
	            while (--i >= 0 && matches.item(i) !== this) {}
	            return i > -1;
	        };
	}

	// Get the closest matching element
	for ( ; elem && elem !== document; elem = elem.parentNode ) {
		if ( selector && elem.matches( selector ) ) return elem;
		if ( optional && elem.matches( optional )) return elem;
	}
	return null;
}

function findParentElementFromScript(scriptId, parentTopSelector, parentDivSelector) {
    // scriptId: id for script where to start
    // parentTopSelector : string with top selector to find element where to start looking inputs
    // parentDivSelector: string used as a selector with parentTopSelector to find what element comes first
    //               when scanning up from current script context
    try {
        var parentElem;
        var elem = document.scripts[document.scripts.length - 1]; // document.currentScript does not work???
        if ( !elem ) return document.querySelector(parentDivSelector) || document;
        if ( scriptId && scriptId != elem.Id ) elem = document.getElementById(scriptId);
        if ( !elem ) return document.querySelector(parentDivSelector) || document;
        return getClosestParentWithSelectorOrOptional(elem, parentTopSelector, parentDivSelector) || document;
    } catch (ex) {
        return document;
    }
}


function ServerSyncValues(parentElement, iframeSelector, prefix, debugName, dname, options)  {
    // Create an communication object for iframe that is already on the document.
    // parentElement:  element that is used to find inputs
    // iframeSelector: selector fo find iframe
    // prefix: prefiese for inputs // TODO: make search more general?
    // debugName: name (id) for possible debug input to show debug results
    // dname: string to prefix debug texts like S1
    // options: {
    //    sendInputs: list of inputs whos values send in first contact,
    //    initObject: object whos value send on first contact
    // }

    var self = this;
    this.parentElem = parentElement;
    this.dname = dname;
    this.inputs = {};
    if ( !options ) options = {};

    this.jsxFrame = self.parentElem.querySelector(iframeSelector);
    if ( debugName )
        this.debugElem = self.parentElem.querySelector(debugName);
    else
        this.debugElem = null;
    this.channel = new MessageChannel();

    this.debug = function(s, dir) {
        if ( !self.debugElem ) return;
        console.log(self.dname + ' ' + dir + ':' + s);
        if ( dir === 'r' ) self.debugElem.value = s;
    }

    self.debug('Start port initialize ' + parentElement.id + ' ' + iframeSelector, 'i');

    this.send = function(obj) {
        self.channel.port1.postMessage(obj);
        self.debug(JSON.stringify(obj), 's');
    }

    function getValue(inp) {
        if ( !inp ) return '';
        var value = inp.input.value;
        if ( !value ) return '';
        try {
            return JSON.parse(inp.input.value);
        } catch (e) {
            return value;
        }

    }

    function onChange(e)  {
        try {
            var inp = getInput(e.target.servname);
            var val = getValue(inp);
            self.debug(inp.servname + " = " + val, 'c');

            if (val === inp.lastValue) return;
            if ( e.port == self.channel.port1 ) return; /* ei samaan kanavaan takaisin */
            self.send([{cmd: 'set', name: inp.servname, value: val}]);
            inp.lastValue = val;
        } catch (err) {
            return;
        }
    }

    var getInput = function(name) {
        var inp = self.inputs[name];
        if ( inp ) return inp;
        var input = self.parentElem.querySelector('#'+prefix+name);
        if ( !input ) return null;
        inp = {input: input, lastValue: ''};
        self.inputs[name] = inp;
        inp.servname = name;
        input.servname = name;
        input.addEventListener('input', onChange);
        input.addEventListener('change', onChange);
        return inp;
    }

    this.channel.port1.onmessage = function(e) {
        self.debug(JSON.stringify(e.data), 'r');
        var retCmd = [];
        for (var i=0; i<e.data.length; i++) {
            cmd = e.data[i];
            var name = cmd.name;
            var inp = getInput(name);
            if ( !inp ) continue;
            switch (cmd.cmd) {
                case 'get':
                    var value = getValue(inp);
                    retCmd.push({cmd: 'set', name: name, value: value});
                    break;
                case 'set':
                    var val = JSON.stringify(cmd.value);
                    if ( inp.input.value === val ) return;
                    inp.input.value = val;
                    var ev = new Event('change');
                    ev.port = self.channel.port1;
                    inp.input.dispatchEvent(ev);
                    break;
                case 'setVisibility':
                    inp.input.style.display = cmd.value;
                    break;
            }
        }
        if ( retCmd.length ) self.send(retCmd);
    };

    this.sendValues = function() {
        var retCmd = [];
        for (var name in self.inputs) {
            var value = JSON.parse(self.inputs[name].value);
            retCmd.push({cmd: 'set', name: name, value: value});
        }
        if ( retCmd.length ) self.send(retCmd);
    };


    var values = {};
    if ( options.sendInputs ) {
        var names = options.sendInputs.split(",");
        for (var i = 0; i<names.length; i++) {
            var name = names[i].trim();
            var inp = getInput(name);
            values[name] = getValue(inp);
        }
    }
    var init = {};
    if ( values ) init.values = values;
    if ( options.initObject ) init.initObject = options.initObject;

    this.jsxFrame.addEventListener("load", function() {
        self.debug(iframeSelector + ' loaded', 'i');
        self.jsxFrame.contentWindow.postMessage(init , '*', [self.channel.port2]);
    });

};
