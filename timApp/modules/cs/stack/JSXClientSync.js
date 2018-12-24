itemsToSync = {};

Number.prototype.round = function(places) {
    if ( places < 0 ) return this;
    return +(Math.round(this + "e+" + places)  + "e-" + places);
}

debug = null; // {value: ''};
function JSXClientSync(port, dname, inputs) {
    this.port = port;
    var self = this;
    this.dname = dname || 'c';

    this.debug = function(s, dir) {
        if ( !debug ) return;
        console.log(this.dname + ' ' + dir + ':' + s);
        if ( dir === 'r' ) debug.value = s;
    }

    self.debug('Message port initialized.', 'i');

    this.send = function(obj) {
        self.port.postMessage(obj);
        self.debug(JSON.stringify(obj), 's');
    }

    port.onmessage = function(e) {
        // debug = document.querySelector('#debug');
        self.debug(JSON.stringify(e.data), 'r');
        retCmd = [];
        for (var i=0; i<e.data.length; i++) {
                try {
                cmd = e.data[i];
                var name = cmd.name;
                bind = itemsToSync[name];
                if ( !bind ) break;
                switch (cmd.cmd) {
                    case 'get':
                        self.send([{cmd: 'set', name: name, value: bind.getf()}]);
                    break;
                    case 'set':
                        var sval = JSON.stringify(cmd.value);
                        if ( sval === bind.lastValue ) break;
                        bind.lastValue = sval;
                        bind.setf(cmd.value);
                        if ( bind.wait1 ) bind.wait1(sval);
                        bind.wait1 = null;
                        if ( bind.elem ) {
                            if (bind.elem.board) bind.elem.board.update();
                            if (bind.elem.update) bind.elem.update();
                        }
                        break;
                }
            } catch ( e ) {
                return;
            }
        }
        if ( retCmd.length ) port2.postMessage(retCmd);
    }

    this.is = function(obj, name) {
       if ( obj.constructor != Object ) return false;
       var v = obj[name];
       if ( typeof v === "undefined" ) return false;
       return true;
    }

    this.bind_elem = function(name, elem, setf, getf, event) {
        var bind = {name: name, elem: elem, value: JSON.stringify(getf()), setf: setf, getf: getf}
        itemsToSync[name] = bind;

        if ( self.is(inputs,name) ) setf(inputs[name]);
        else self.send([{cmd: 'get', name: name}]);
        bind.lastValue = JSON.stringify(getf());

        if ( event ) elem.on(event, onChange);
        else elem.board.on("update", onChange);

        function onChange() {
            var val = getf();
            var sval = JSON.stringify(val);
            self.debug(name + " = " +sval, 'c');
            if ( sval === bind.lastValue ) return;
            self.send([{cmd: 'set', name: name, value: val}]);
            bind.lastValue = sval;
        }
        return bind;
    };

    this.bind_point = function(name, elem, options) {
        options = options || {};
        var event = options.event;
        var des = options.des;
        if (typeof des === 'undefined')  des = 2;
        var getf = function() { return [elem.X().round(des), elem.Y().round(des)];};
        var setf = function(val) { if ( val ) elem.moveTo(val); };
        self.bind_elem(name, elem, setf, getf, event);
    };

    this.bind_slider = function(name, elem, options) {
        options = options || {};
        var event = options.event;
        var des = options.des;
        if (typeof des === 'undefined')  des = 2;
        var getf = function() { return elem.Value().round(des); };
        var setf = function(val) { 
            if ( !isNaN(parseFloat(val)) ) elem.setValue(val);
        };
        self.bind_elem(name, elem, setf, getf, event);
    };

    this.bind_var = function(name, options) {
        options = options || {};
        var event = options.event || "change";
        var input = document.createElement("input");
        input.value = '""'; // empty JSON
        input.on = function(event, onChange) {
            input.addEventListener(event, onChange);
        };
        // var elem = { value: "", on: function(event, onChange){}};
        var getf = function() { return JSON.parse(input.value); };
        var setf = function(val) { input.value = JSON.stringify(val); };
        var bind = self.bind_elem(name, input, setf, getf, event);
        bind.wait1 = options.wait1;
        return input;
    };

    this.set = function(name, val) {
        bind = itemsToSync[name];
        if ( !bind ) return;
        bind.setf(val);
    };

    this.get = function(name, val) {
        bind = itemsToSync[name];
        if ( bind ) return bind.getf();
        return self.is(inputs,name) ? inputs[name] : '';
    };

    this.find_input_id = function(divid, name) {
        return name;
    };
}

