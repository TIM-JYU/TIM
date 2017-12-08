/******************************************************************************/
var event_listeners = [];
/******************************************************************************/
var EventListener = function(event_src_id, event_type, callback_context, callback_function, callback_args) {
    this.src_id = event_src_id;
    this.type = event_type;
    this.callback_context = callback_context;
    this.callback_function = callback_function;
    this.callback_args = callback_args;
};
/******************************************************************************/
function onEvent(src_id, type) {
    //alert(src_id + ' ' + type);
    for(var i = 0; i < event_listeners.length; ++i) {
	if ( event_listeners[i].src_id == src_id &&
	     event_listeners[i].type == type ) {
	    if ( event_listeners[i].callback_context == null )
		event_listeners[i].callback_context = window;
	    if ( event_listeners[i].callback_args == null )
		event_listeners[i].callback_context[event_listeners[i].callback_function]();
	    else
		event_listeners[i].callback_context[event_listeners[i].callback_function](event_listeners[i].callback_args);
	}
    }
}


/******************************************************************************/
function svg_elem(tag) {
    return jQuery(document.createElementNS('http://www.w3.org/2000/svg', tag));
}

/******************************************************************************/
var SVG_knob = function(id, cx, cy, color) {
    this.id = id;
    this.cx = cx;
    this.cy = cy;

    this.circle = svg_elem('circle');
    this.circle.attr('id', id + '_circle');
    this.circle.attr('cx','0');
    this.circle.attr('cy','0');
    this.circle.attr('r', '40');
    this.circle.attr('fill', 'grey');
    this.circle.attr('stroke', 'black');
    this.circle.attr('stroke-width', '3');
    this.circle.attr('cursor','pointer');

    this.rect = svg_elem('rect');
    this.rect.attr('id', id + '_rect');
    this.rect.attr('x', '-7');
    this.rect.attr('width', '14');
    this.rect.attr('y', '-60');
    this.rect.attr('height', '120');
    this.rect.attr('rx', 5);
    this.rect.attr('fill', color);
    this.rect.attr('stroke', 'black');
    this.rect.attr('stroke-width', '3');
    this.rect.attr('cursor','pointer');

    this.group = svg_elem('g');
    this.group.attr('id', id);
    this.group.attr('onClick', "onEvent('"+id+"','onClick')");

    this.turnOff();
};
SVG_knob.prototype.appendTo = function(target) {
    this.group.appendTo(target);
    this.circle.appendTo('#'+this.group.attr('id'));
    this.rect.appendTo('#'+this.group.attr('id'));
}

SVG_knob.prototype.toggle = function() {
    if ( this.on ) this.turnOff();
    else           this.turnOn();
}

SVG_knob.prototype.turnOff = function() {
    var a = 0.7071;  var b = -a;
    var c = a;       var d =  a;
    var e = this.cx; var f = this.cy;
    this.group.attr('transform', 'matrix('+a+','+b+','+c+','+d+','+this.cx+','+this.cy+')');
    this.on = false;
    onEvent(this.id,'onOff');
}
SVG_knob.prototype.turnOn = function() {
    var a = 1; var b =  0;
    var c = 0; var d =  1;
    var e = this.cx; var f = this.cy;
    this.group.attr('transform', 'matrix('+a+','+b+','+c+','+d+','+this.cx+','+this.cy+')');
    this.on = true;
    onEvent(this.id,'onOn');
};
/******************************************************************************/



/******************************************************************************/
var buffer_sim = null;
/******************************************************************************/
var BufferSim = function(weak_acid_label, salt_label, salt_ion_label, n_HCl, n_weak_acid, n_salt, n_NaOH) {
    this.weak_acid = weak_acid_label;
    this.salt = salt_label;
    this.salt_ion = salt_ion_label;
    this.log_scale = true;
    this.V        = 0.1;      // 100ml
    this.n_weak_acid  = n_weak_acid;
    this.n_salt       = n_salt;
    this.n_HCl    = n_HCl;
    this.n_NaOH   = n_NaOH;
};
BufferSim.prototype.create = function(svg_id) {
    this.stream = svg_elem('line');
    this.stream.attr('id', 'bf_stream');
    this.stream.attr('x1', '200');
    this.stream.attr('y1', '350');
    this.stream.attr('x2', '200');
    this.stream.attr('y2', '860');
    this.stream.attr('stroke', '#0080ff');
    this.stream.attr('stroke-width', '10');
    this.stream.attr('opacity', '0.0');
    this.stream.appendTo('#'+svg_id);

    var tube1 = svg_elem('path');
    tube1.attr('d','M180,0L220,0L220,300L210,350L190,350L180,300z');
    tube1.attr('fill','url(#bf_tube_grad)');
    tube1.attr('stroke','black');
    tube1.attr('stroke-width','1');
    tube1.appendTo("#"+svg_id);

    var tube2 = svg_elem('path');
    tube2.attr('d','M280,0L320,0L320,300L310,350L290,350L280,300z');
    tube2.attr('fill','url(#bf_tube_grad)');
    tube2.attr('stroke','black');
    tube2.attr('stroke-width','1');
    tube2.appendTo("#"+svg_id);

    var tube3 = svg_elem('path');
    tube3.attr('d','M380,0L420,0L420,300L410,350L390,350L380,300z');
    tube3.attr('fill','url(#bf_tube_grad)');
    tube3.attr('stroke','black');
    tube3.attr('stroke-width','1');
    tube3.appendTo("#"+svg_id);

    var tube4 = svg_elem('path');
    tube4.attr('d','M480,0L520,0L520,300L510,350L490,350L480,300z');
    tube4.attr('fill','url(#bf_tube_grad)');
    tube4.attr('stroke','black');
    tube4.attr('stroke-width','1');
    tube4.appendTo("#"+svg_id);

    var wire = svg_elem('path');
    wire.attr('d','M200,840 C 140,430 190,425 90,430 C 0,450 0,890 90,890 L 550,890 C 650,890 600,850 650,850');
    wire.attr('fill','none');
    wire.attr('stroke','black');
    wire.attr('stroke-width','10');
    wire.attr('stroke-linecap','round');
    wire.appendTo("#"+svg_id);

    this.solution = svg_elem('path');
    var h = 870 - this.V * 300;
    this.solution.attr('d','M140,'+h+'L140,860L150,880L540,880L550,860L550,'+h+'z')
    this.solution.attr('fill','black');//#80ffff');
    this.solution.attr('stroke','none');
    this.solution.attr('stroke-width','5');
    this.solution.attr('opacity','.8');
    this.solution.attr('stroke-linecap','round');
    this.solution.appendTo("#"+svg_id);

    var glass = svg_elem('path');
    glass.attr('d','M100,440L130,470L140,510L140,860L150,880L540,880L550,860L550,450L560,430L120,430z')
    glass.attr('fill','url(#bf_glass_grad)');
    glass.attr('stroke','#000080');
    glass.attr('stroke-width','5');
    glass.attr('opacity','.8');
    glass.attr('stroke-linecap','round');
    glass.appendTo("#"+svg_id);


    var tablet = svg_elem('path');
    tablet.attr('d','M650,200L1550,200L1550,890L650,890z')
    tablet.attr('fill','#808080');
    tablet.attr('stroke','black');
    tablet.attr('stroke-width','5');
    tablet.attr('stroke-linecap','round');
    tablet.appendTo("#"+svg_id);

    var screen = svg_elem('path');
    screen.attr('d','M680,230L1520,230L1520,860L680,860z')
    screen.attr('fill','white');
    screen.attr('stroke','black');
    screen.attr('stroke-width','5');
    screen.attr('stroke-linecap','round');
    screen.appendTo("#"+svg_id);
 

    // text
    this.pH_text = svg_elem('text');
    this.pH_text.attr('x', '750');
    this.pH_text.attr('y', '350');
    this.pH_text.attr('font-size', '60');
    this.pH_text.attr('font-family', 'Arial');
    this.pH_text.html('pH = 7.00');
    this.pH_text.appendTo("#"+svg_id);

    this.V_text = svg_elem('text');
    this.V_text.attr('x', '1150');
    this.V_text.attr('y', '350');
    this.V_text.attr('font-size', '60');
    this.V_text.attr('font-family', 'Arial');
    this.V_text.html('V = 0.100 l');
    this.V_text.appendTo("#"+svg_id);

    /*
    this.pH_text = svg_elem('text');
    this.pH_text.attr('x', '1330');
    this.pH_text.attr('y', '300');
    this.pH_text.attr('font-size', '26');
    this.pH_text.attr('font-family', 'Arial');
    this.pH_text.html('pH = 7.00');
    this.pH_text.appendTo("#"+svg_id);

    this.V_text = svg_elem('text');
    this.V_text.attr('x', '1330');
    this.V_text.attr('y', '380');
    this.V_text.attr('font-size', '26');
    this.V_text.attr('font-family', 'Arial');
    this.V_text.html('V = 0.100 l');
    this.V_text.appendTo("#"+svg_id);
    */

    /*
    this.log_text = svg_elem('text');
    this.log_text.attr('x', '720');
    this.log_text.attr('y', '410');
    this.log_text.attr('font-size', '26');
    this.log_text.attr('font-family', 'Arial');
    this.log_text.text('LOG');
    this.log_text.appendTo("#"+svg_id);

    this.lin_text = svg_elem('text');
    this.lin_text.attr('x', '805');
    this.lin_text.attr('y', '395');
    this.lin_text.attr('font-size', '26');
    this.lin_text.attr('font-family', 'Arial');
    this.lin_text.text('LIN');
    this.lin_text.appendTo("#"+svg_id);
    */

    this.axis1_text = svg_elem('text');
    this.axis1_text.attr('x', '695');
    this.axis1_text.attr('y', '530');
    this.axis1_text.attr('font-size', '30');
    this.axis1_text.attr('font-family', 'Arial');
    this.axis1_text.text('-');
    this.axis1_text.appendTo("#"+svg_id);

    this.axis2_text = svg_elem('text');
    this.axis2_text.attr('x', '695');
    this.axis2_text.attr('y', '650');
    this.axis2_text.attr('font-size', '30');
    this.axis2_text.attr('font-family', 'Arial');
    this.axis2_text.text('-');
    this.axis2_text.appendTo("#"+svg_id);

    this.axis3_text = svg_elem('text');
    this.axis3_text.attr('x', '695');
    this.axis3_text.attr('y', '770');
    this.axis3_text.attr('font-size', '30');
    this.axis3_text.attr('font-family', 'Arial');
    this.axis3_text.text('-');
    this.axis3_text.appendTo("#"+svg_id);

    this.yline1 = svg_elem('line');
    this.yline1.attr('x1','780');
    this.yline1.attr('y1','760');
    this.yline1.attr('x2','1490');
    this.yline1.attr('y2','760');
    this.yline1.attr('stroke','black');
    this.yline1.attr('stroke-width','2');
    this.yline1.appendTo("#"+svg_id);

    this.yline2 = svg_elem('line');
    this.yline2.attr('x1','780');
    this.yline2.attr('y1','640');
    this.yline2.attr('x2','1490');
    this.yline2.attr('y2','640');
    this.yline2.attr('stroke','black');
    this.yline2.attr('stroke-width','2');
    this.yline2.appendTo("#"+svg_id);

    this.yline3 = svg_elem('line');
    this.yline3.attr('x1','780');
    this.yline3.attr('y1','520');
    this.yline3.attr('x2','1490');
    this.yline3.attr('y2','520');
    this.yline3.attr('stroke','black');
    this.yline3.attr('stroke-width','2');
    this.yline3.appendTo("#"+svg_id);

    /*    
    this.table = svg_elem('rect');
    this.table.attr('x', '900');
    this.table.attr('y', '260');
    this.table.attr('width', '590');
    this.table.attr('height', '180');
    this.table.attr('fill', 'none');
    this.table.attr('stroke', 'black');
    this.table.attr('stroke-width', '3');
    this.table.appendTo("#"+svg_id);


    this.H3O_label = svg_elem('text');
    this.H3O_label.attr('x', '950');
    this.H3O_label.attr('y', '300');
    this.H3O_label.attr('font-size', '26');
    this.H3O_label.attr('font-family', 'Arial');
    this.H3O_label.html('[H3O+]');
    this.H3O_label.appendTo("#"+svg_id);

    this.OH_label = svg_elem('text');
    this.OH_label.attr('x', '950');
    this.OH_label.attr('y', '340');
    this.OH_label.attr('font-size', '26');
    this.OH_label.attr('font-family', 'Arial');
    this.OH_label.html('[OH-]');
    this.OH_label.appendTo("#"+svg_id);


    this.weak_acid_label = svg_elem('text');
    this.weak_acid_label.attr('x', '950');
    this.weak_acid_label.attr('y', '380');
    this.weak_acid_label.attr('font-size', '26');
    this.weak_acid_label.attr('font-family', 'Arial');
    this.weak_acid_label.html('[' + this.weak_acid +']');
    this.weak_acid_label.appendTo("#"+svg_id);

    this.salt_label = svg_elem('text');
    this.salt_label.attr('x', '950');
    this.salt_label.attr('y', '420');
    this.salt_label.attr('font-size', '26');
    this.salt_label.attr('font-family', 'Arial');
    this.salt_label.html('[' + this.salt_ion + ']');
    this.salt_label.appendTo("#"+svg_id);


    this.H3O_text = svg_elem('text');
    this.H3O_text.attr('x', '1130');
    this.H3O_text.attr('y', '300');
    this.H3O_text.attr('font-size', '26');
    this.H3O_text.attr('font-family', 'Arial');
    this.H3O_text.html('= 0.00 M');
    this.H3O_text.appendTo("#"+svg_id);

    this.OH_text = svg_elem('text');
    this.OH_text.attr('x', '1130');
    this.OH_text.attr('y', '340');
    this.OH_text.attr('font-size', '26');
    this.OH_text.attr('font-family', 'Arial');
    this.OH_text.html('= 0.00 M');
    this.OH_text.appendTo("#"+svg_id);

    this.weak_acid_text = svg_elem('text');
    this.weak_acid_text.attr('x', '1130');
    this.weak_acid_text.attr('y', '380');
    this.weak_acid_text.attr('font-size', '26');
    this.weak_acid_text.attr('font-family', 'Arial');
    this.weak_acid_text.html('= 0.00 M');
    this.weak_acid_text.appendTo("#"+svg_id);

    this.salt_text = svg_elem('text');
    this.salt_text.attr('x', '1130');
    this.salt_text.attr('y', '420');
    this.salt_text.attr('font-size', '26');
    this.salt_text.attr('font-family', 'Arial');
    this.salt_text.html('= 0.00 M');
    this.salt_text.appendTo("#"+svg_id);


    this.info_text = svg_elem('text');
    this.info_text.attr('x', '700');
    this.info_text.attr('y', '100');
    this.info_text.attr('font-size', '26');
    this.info_text.attr('font-family', 'Arial');
    this.info_text.html('You added 100ml of water.');
    this.info_text.appendTo("#"+svg_id);

    for(var j = 0; j < 4; ++j) {
	var dot = svg_elem('circle');
	dot.attr('cx', 930);
	dot.attr('cy', 293+j*40);
	dot.attr('r', 5);
	dot.attr('fill', ['red','blue','orange','green'][j]);
	dot.attr('stroke', 'black');
	dot.attr('stroke-width', '2');
	dot.attr('opacity', '0.7');
	dot.appendTo('#'+svg_id);
    }
    */

    this.HCl_bar_label = svg_elem('text');
    this.HCl_bar_label.attr('x', '650');
    this.HCl_bar_label.attr('y', '50');
    this.HCl_bar_label.attr('width', '0');
    this.HCl_bar_label.attr('height', '30');
    this.HCl_bar_label.attr('fill', 'red');
    this.HCl_bar_label.attr('font-size', '26');
    this.HCl_bar_label.html('0 x 10ml x 0.1M HCl');
    this.HCl_bar_label.appendTo("#"+svg_id);

    this.HCl_bar = svg_elem('rect');
    this.HCl_bar.attr('x', '980');
    this.HCl_bar.attr('y', '25');
    this.HCl_bar.attr('width', '0');
    this.HCl_bar.attr('height', '30');
    this.HCl_bar.attr('fill', 'red');
    this.HCl_bar.appendTo("#"+svg_id);

    this.weak_acid_bar_label = svg_elem('text');
    this.weak_acid_bar_label.attr('x', '650');
    this.weak_acid_bar_label.attr('y', '85');
    this.weak_acid_bar_label.attr('width', '0');
    this.weak_acid_bar_label.attr('height', '30');
    this.weak_acid_bar_label.attr('fill', 'orange');
    this.weak_acid_bar_label.attr('font-size', '26');
    this.weak_acid_bar_label.html('0 x 10ml x 1M ' + this.weak_acid);
    this.weak_acid_bar_label.appendTo("#"+svg_id);

    this.weak_acid_bar = svg_elem('rect');
    this.weak_acid_bar.attr('x', '980');
    this.weak_acid_bar.attr('y', '60');
    this.weak_acid_bar.attr('width', '0');
    this.weak_acid_bar.attr('height', '30');
    this.weak_acid_bar.attr('fill', 'orange');
    this.weak_acid_bar.appendTo("#"+svg_id);

    this.salt_bar_label = svg_elem('text');
    this.salt_bar_label.attr('x', '650');
    this.salt_bar_label.attr('y', '120');
    this.salt_bar_label.attr('width', '0');
    this.salt_bar_label.attr('height', '30');
    this.salt_bar_label.attr('fill', 'green');
    this.salt_bar_label.attr('font-size', '26');
    this.salt_bar_label.html('0 x 10ml x 1M ' + this.salt);
    this.salt_bar_label.appendTo("#"+svg_id);

    this.salt_bar = svg_elem('rect');
    this.salt_bar.attr('x', '980');
    this.salt_bar.attr('y', '95');
    this.salt_bar.attr('width', '0');
    this.salt_bar.attr('height', '30');
    this.salt_bar.attr('fill', 'green');
    this.salt_bar.appendTo("#"+svg_id);

    this.NaOH_bar_label = svg_elem('text');
    this.NaOH_bar_label.attr('x', '650');
    this.NaOH_bar_label.attr('y', '155');
    this.NaOH_bar_label.attr('width', '0');
    this.NaOH_bar_label.attr('height', '30');
    this.NaOH_bar_label.attr('fill', 'blue');
    this.NaOH_bar_label.attr('font-size', '26');
    this.NaOH_bar_label.html('0 x 10ml x 0.1M NaOH');
    this.NaOH_bar_label.appendTo("#"+svg_id);

    this.NaOH_bar = svg_elem('rect');
    this.NaOH_bar.attr('x', '980');
    this.NaOH_bar.attr('y', '130');
    this.NaOH_bar.attr('width', '0');
    this.NaOH_bar.attr('height', '30');
    this.NaOH_bar.attr('fill', 'blue');
    this.NaOH_bar.appendTo("#"+svg_id);

    var knob1 = new SVG_knob('bf_knob1', '200','100', 'red');
    var knob2 = new SVG_knob('bf_knob2', '300','200', 'orange');
    var knob3 = new SVG_knob('bf_knob3', '400','100', 'green');
    var knob4 = new SVG_knob('bf_knob4', '500','200', 'blue');

    var knobs = [knob1, knob2, knob3, knob4];

    for(var i = 0; i < knobs.length; ++i) {
	knobs[i].appendTo("#"+svg_id);

	event_listeners.push(new EventListener(knobs[i].id, 'onClick', knobs[i], 'turnOn', null));
	event_listeners.push(new EventListener(knobs[i].id, 'onOn', window, 'delayedTurnOff', knobs[i]));
    }

    event_listeners.push(new EventListener(knob1.id, 'onOn', this, 'add_HCl', null));
    event_listeners.push(new EventListener(knob2.id, 'onOn', this, 'add_weak_acid', null));
    event_listeners.push(new EventListener(knob3.id, 'onOn', this, 'add_salt', null));
    event_listeners.push(new EventListener(knob4.id, 'onOn', this, 'add_NaOH', null));


    var knob5 = new SVG_knob('bf_knob5', '750','310', '#e0e0e0');
    knob5.appendTo("#"+svg_id);
    event_listeners.push(new EventListener(knob5.id, 'onClick', knob5, 'toggle', null));
    event_listeners.push(new EventListener(knob5.id, 'onOn', this, 'setLogScale', true));
    event_listeners.push(new EventListener(knob5.id, 'onOff', this, 'setLogScale', false));

    
    this.data_points = [];
    this.data_points_group = svg_elem('g');
    this.data_points_group.attr('id','data_points_group');
    this.data_points_group.appendTo('#'+svg_id);
    
    this.update();
    knob5.turnOn();

    jQuery("#bf_knob5").hide();
};

BufferSim.prototype.setLogScale = function(log_scale) {
    this.log_scale = log_scale;
    if ( this.log_scale ) {
	this.axis1_text.html("&nbsp;1e-1");
	this.axis2_text.html("&nbsp;1e-7");
	this.axis3_text.text("1e-14");
    }
    else {
	this.axis1_text.html("&nbsp;0.30");
	this.axis2_text.html("&nbsp;0.15");
	this.axis3_text.html("&nbsp;0.00");
    }
    this.plot();
}
    
BufferSim.prototype.add_HCl = function() {
    this.stream.attr('x1','200');
    this.stream.attr('x2','200');
    this.stream.attr('opacity','0.8');
    //this.info_text.html('You added 10ml of 0.1M HCl.');

    this.V += 0.01;
    this.n_HCl += 0.1 * 0.01; // 0.1 M * 10 ml
    this.update();
}
BufferSim.prototype.add_NaOH = function() {
    this.stream.attr('x1','500');
    this.stream.attr('x2','500');
    this.stream.attr('opacity','0.8');
    //this.info_text.html('You added 10ml of 0.1M NaOH.');

    this.V += 0.01;
    this.n_NaOH += 0.1 * 0.01; // 0.1 M * 10 ml
    this.update();
}
BufferSim.prototype.add_weak_acid = function() {
    this.stream.attr('x1','300');
    this.stream.attr('x2','300');
    this.stream.attr('opacity','0.8');
    //this.info_text.html('You added 10ml of 1M ' + this.weak_acid + '.');

    this.V += 0.01;
    this.n_weak_acid += 1.0 * 0.01; // 1 M * 10 ml
    this.update();
}
BufferSim.prototype.add_salt = function() {
    this.stream.attr('x1','400');
    this.stream.attr('x2','400');
    this.stream.attr('opacity','0.8');
    //this.info_text.html('You added 10ml of 1M ' + this.salt + '.');

    this.V += 0.01;
    this.n_salt += 1.0 * 0.01; // 1 M * 10 ml
    this.update();
}

BufferSim.prototype.update = function() {
    // NOT USING THIS, BECAUSE DOESN'T WORK FOR FULL pH RANGE
    // ICE table HA + H2O <=> A- + H3O+
    // if [HCl]_0 > [NaOH]_0
    //    |      [HA]      |      [A-]      |         [H3O+]         |
    // I  | [weak_acid]_0      | [NaHCO3]_0     | [HCl]_0 - [NaOH]_0     |
    // C  |      x         |     - x        |        - x             |
    // E  | [weak_acid]_0 + x  | [NaHCO3]_0 - x | [HCl]_0 - [NaOH]_0 - x |
    //
    // Ka = [H3O+] [A-] / [HA]
    //    = ([HCl]_0 - [NaOH]_0 - x) * ([NaHCO3]_0 - x) / ([H2CO3]_0 + x)
    //    = (P - x) * (S - x) / (A + x)
    // Ka * (A + x) = (P - x) * (S - x)
    // Ka * A + Ka * x = P * S - (P+S) * x + x**2
    // 0 = x**2 - (P+S+Ka) * x - Ka * A
    // x = .5*(P+S+Ka) +- .5 * sqrt[ (P+S+Ka)**2 + 4 * Ka * A ]  
    /*
    var Ka = 2.5e-4; // carbonic acid
    var Ka = 10**-4.76 = 1.7378e-5; // acetic acid 
    var P = (this.n_HCl - this.n_NaOH) / this.V;
    var S = this.n_NaHCO3 / this.V;
    var A = this.n_H2CO3 / this.V;
    var x1 = .5*(P+S+Ka) + .5 * Math.sqrt( (P+S+Ka)*(P+S+Ka) + 4 * Ka * A );
    var x2 = .5*(P+S+Ka) - .5 * Math.sqrt( (P+S+Ka)*(P+S+Ka) + 4 * Ka * A );
    */

    /////////////////////////////////////////////////////////////////////
    // This is better...
    // HA + H2O   <=> A- + H3O+
    // H2O + H2O  <=> H3O+ + OH- 
    // HCl + H2O  <=> H3O+ + Cl-
    // NaOH + H2O <=> OH- + Na+

    // KA*    =   [A-][H3O+] / [HA][H2O]
    // KH2O*  =  [H3O+][OH-] / [H2O][H2O]
    // KHCl*  =  [Cl-][H3O+] / [HCl][H2O]
    // KNaOH* =   [Na+][OH-] / [NaOH][H2O]

    // KA     =   [A-][H3O+] / [HA]
    // KH2O   =  [H3O+][OH-]
    // KHCl   =  [Cl-][H3O+] / [HCl]
    // KNaOH  =   [Na+][OH-] / [NaOH]

    // [H3O+]_0 = [HCl]_0
    // [OH-]_0  = [NaOH]_0
    // [A-]_0 = [NaHCO3]_0 or [CH3COONa]
    // [HA]_0 = [H2CO3]_0 or [CH3COOH]
    

    // HA + H2O <=> H3O+ + A-
    // 2 H2O <=> H3O+ + OH-

    //         H3O+ | OH- | HA | A-
    //  I       H     OH    HA   A      
    //  C      x+y     y    -x   x
    //  E     H+x+y  OH+y  HA-x A+x

    // Ka   = (A+x)(H+x+y) / (HA-x)
    // 0    = -Ka HA + A (H+y) + (Ka+H+A+y) x + x**2
    // x = - .5 (Ka + H + A + y) +/- .5 sqrt( (Ka+H+A+y)**2 + 4 (Ka HA - A (H+y)) )
    // Kw   = (H+x+y) (OH+y) = 1e-14
    // 0    = -Kw + (H+x) OH + (H+OH+x) y + y**2
    // y = - .5 (H+OH+x) +- .5 sqrt( (H+OH+x)**2 + 4 (Kw - (H+x) OH) ) 
    //
    // a pair of quadratic equations:
    // x = - .5 (Ka + H + A + y) +- .5 sqrt( (Ka+H+A+y)**2 + 4 (Ka HA - A (H+y)) )
    // y = - .5 (H+OH+x) +- .5 sqrt( (H+OH+x)**2 + 4 (Kw - (H+x) OH) ) 
    // start with y = 0 and solve x, iterate x and y
    
    // NOPE, go further and use third order
    //
    // first: z = x+y => x = z - y
    // then:  p = [H+] + z and q = [OH-] + y 
    // finally: use Kw = p q => q = Kw/p, and solve p
    //
    // p**3 + p**2 ( Ka + [OH-] + [A-] - [H+] ) 
    //      + p ( Ka [OH-] - Ka ( [HA] + [H+] ) - Kw )
    //      - Ka Kw = 0
    //
    
    //var Ka = 2.5e-4;  // carbonic acid
    var Ka = 1.7378e-5; // acetic acid pKa = 4.76
    var Kw = 1e-14;

    var H  = this.n_HCl / this.V;
    var OH = this.n_NaOH / this.V;

    var A = this.n_salt / this.V;
    var HA = this.n_weak_acid / this.V;

    var a = 1;
    var b = Ka + OH + A - H;
    var c = Ka * (OH - HA - H) - Kw;
    var d = -Ka * Kw;
    var cubic_ctx = { 'a': a, 'b': b, 'c': c, 'd': d };
    var p0 = 1e-15;
    for(var i = 0; i < 15; ++i) {
	if ( cubic(p0, cubic_ctx) > 0 ) {
	    break;
	}
	p0 *= 10;
    }
    console.log(p0 + ' ' + cubic(p0, cubic_ctx));
    //var pa = p0/10.
    //var pb = p0;
    //var result = bisection_solver(cubic, cubic_ctx, pa, pb, 1e-30, 1000);
    //p0 = result.x;
    var result2 = newton_solver(cubic, dcubic, cubic_ctx, p0, 1e-30, 1e-7, 100);
    var p = result2.x;
    var q = Kw / p;

    var H_eq  = p;
    var OH_eq = q;
    var HA_eq = HA - (p-H) + (q-OH);
    var A_eq  = A  + (p-H) - (q-OH);

    console.log("pH: " + (-Math.log10(H_eq)));
    console.log("HA: " + HA_eq);
    console.log("A-: " + A_eq);


    this.data_points.push([H_eq, OH_eq, HA_eq, A_eq]);

    this.pH_text.html('pH = ' + toFixed(-Math.log10(H_eq),2));
    //this.H3O_text.html('= ' + toRoundedExponential(H+x+y,2) + ' M');
    //this.OH_text.html('= ' + toRoundedExponential(OH+y,2) + ' M');
    //this.weak_acid_text.html('= ' + toRoundedExponential(HA-x,2) + ' M');
    //this.salt_text.html('= ' + toRoundedExponential(A+x,2) + ' M');
    this.V_text.html('V = ' + toFixed(this.V,3) + ' l');
    this.plot();

    if ( this.V > 1.2 ) {
	alert("Don't spill. Resetting.");
	buffer_sim_reset();
    }

    // console.log( ("pHH = " + (-Math.log10(Ka) + Math.log10(A/HA))).replace('NaN','-'));
}

/******************************************************************************/
function toFixed(x,n) {
    var p = Math.floor(Math.log10(Math.abs(x)));
    var str = '';
    if ( x < 0 ) str += '-';
    for(var i = 0; i > Math.max(p,-n-1); --i) {
	str += '0';
	if ( i == 0 ) str += '.';
    }
    for(var i = p; i >= -n; --i) {
	str += '' + (Math.floor(Math.abs(x)*Math.pow(10,-i)) % 10);
	if ( i == 0 ) str += '.';
    }
    return str;
}
function toRoundedExponential(x,n) {
    if ( x*x < 1e-100 ) return toFixed(x,n);
    var p = Math.floor(Math.log10(x));
    return toFixed(x * Math.pow(10.0,-p), n) + 'e' + p;
}

/******************************************************************************/
BufferSim.prototype.plot = function() {
    var h = 870 - this.V * 300;
    this.solution.attr('d','M140,'+h+'L140,860L150,880L540,880L550,860L550,'+h+'z')

    var xoff = 790;
    var dx   = 10;
    var yoff = 500;
    var dy   = -20;

    jQuery('#data_points_group').empty();
    var ioff = Math.max(0, this.data_points.length-70); 
    for(var i = ioff; i < this.data_points.length; ++i) {
	for(var j = 0; j >= 0; --j) {
	    x = i-ioff;
	    var y = this.data_points[i][j];
	    if ( this.log_scale ) {
		if ( y <= 0.0 ) continue;
		y = -Math.log(y)/Math.log(10.)-14;
	    }
	    else
		y = y*40-13;

	    if ( yoff+dy*y < 200 ) continue;

	    var dot = svg_elem('circle');
	    dot.attr('cx', xoff+dx*x);
	    dot.attr('cy', yoff+dy*y);
	    dot.attr('r', 5);
	    dot.attr('fill', ['red','blue','orange','green'][j]);
	    dot.attr('stroke', 'black');
	    dot.attr('stroke-width', '2');
	    dot.attr('opacity', '0.7');
	    dot.attr('title', toRoundedExponential(this.data_points[i][j], 3));
	    dot.appendTo('#data_points_group');
	}
    }
    var pH = this.data_points[this.data_points.length-1][0];
    pH = -Math.log(pH)/Math.log(10.);
    jQuery("#pH").html('<h2>'+Math.round(pH*1e2)/1e2+'</h2>');

    this.HCl_bar.attr('width',       this.n_HCl       * .5e4);
    this.NaOH_bar.attr('width',      this.n_NaOH      * .5e4);
    this.weak_acid_bar.attr('width', this.n_weak_acid * .5e4);
    this.salt_bar.attr('width',      this.n_salt      * .5e4);

    this.HCl_bar_label.html(Math.round(this.n_HCl/(0.1*.01)) + ' x 10ml x 0.1M HCl');
    this.NaOH_bar_label.html(Math.round(this.n_NaOH/(0.1*.01)) + ' x 10ml x 0.1M NaOH');
    this.weak_acid_bar_label.html(Math.round(this.n_weak_acid/(1.0*.01)) + ' x 10ml x 1M ' + this.weak_acid);
    this.salt_bar_label.html(Math.round(this.n_salt/(1.0*.01)) + ' x 10ml x 1M ' + this.salt);
}

/******************************************************************************/
function buffer_sim_init() {
    buffer_sim = new BufferSim('CH3COOH','CH3COONa', 'CH3COO-', 0, 0, 0, 0);
    buffer_sim.create("buffer_sim_svg");
}
function buffer_sim_reset() {
    buffer_sim = null;
    jQuery('#buffer_sim_svg').empty();
    event_listeners = [];
    buffer_sim_init();
}

/******************************************************************************/
function delayedTurnOff(knob) { setTimeout(turnOff, 300, knob); }
function turnOff(knob) { 
    jQuery("#bf_stream").attr('opacity','0.0');
    knob.turnOff(); 
}
