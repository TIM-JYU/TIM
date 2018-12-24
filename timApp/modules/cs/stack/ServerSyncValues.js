function ServerSyncValues(elem, frameName, prefix, debugName, dname, inputs)  {
    var self = this;
    this.elem = elem;
    this.dname = dname;
    this.inputs = {};
    this.jsxFrame = elem.querySelector(frameName);
    if ( debugName )
        this.debugElem = elem.querySelector(debugName);
    else
        this.debugElem = null;
    this.channel = new MessageChannel();

    this.debug = function(s, dir) {
        if ( !self.debugElem ) return;
        console.log(self.dname + ' ' + dir + ':' + s);
        if ( dir === 'r' ) self.debugElem.value = s;
    }

    self.debug('Start port initialize' + frameName, 'i');

    this.send = function(obj) {
        self.channel.port1.postMessage(obj);
        self.debug(JSON.stringify(obj), 's');
    }

    function getValue(inp) {
        if ( !inp ) return '';
        if ( !inp.input.value ) return '';
        return JSON.parse(inp.input.value);
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
        var input = elem.querySelector('#'+prefix+name);
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


    var values = "Init";
    if ( inputs ) {
        values = {};
        var names = inputs.split(",");
        for (var i = 0; i<names.length; i++) {
            var name = names[i].trim();
            var inp = getInput(name);
            values[name] = getValue(inp);
        }
    }

    this.jsxFrame.addEventListener("load", function() {
        self.debug(frameName + ' loaded', 'i');
        self.jsxFrame.contentWindow.postMessage(values , '*', [self.channel.port2]);
    });

};
