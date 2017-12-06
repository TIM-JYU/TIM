var SimpleLineChart = (function () {
    function SimpleLineChart(svg_id, xlabel, x_min, x_max, ylabel, y_min, y_max, show_line, show_markers) {
        if (show_line === void 0) { show_line = true; }
        if (show_markers === void 0) { show_markers = false; }
        this.svg_id = svg_id;
        this.xlabel = xlabel;
        this.x_min = x_min;
        this.x_max = x_max;
        this.ylabel = ylabel;
        this.y_min = y_min;
        this.y_max = y_max;
        this.show_line = show_line;
        this.show_markers = show_markers;
        this.width = 0;
        this.height = 0;
        this.x_off = 0;
        this.y_off = 0;
        this.width = jQuery("#" + this.svg_id).width();
        this.height = jQuery("#" + this.svg_id).height();
        var offset_factor = .1;
        this.x_off = 2. * this.width * offset_factor;
        this.y_off = this.height * offset_factor;
        this.chart = d3.selectAll("#" + this.svg_id).append("g")
            .attr("class", "main")
            .attr("transform", "translate(" + this.x_off + "," + this.y_off + ") scale(" + (1 - 3 * offset_factor) + ")")
            .style({ "font-size": "18pt" });
        // y axis
        this.y_scale = d3.scale.linear()
            .domain([this.y_min, this.y_max])
            .range([this.height, 0]);
        this.chart.append("g")
            .attr("class", "yaxis")
            .call(d3.svg.axis()
            .scale(this.y_scale)
            .orient("left")
            .ticks(5));
        d3.selectAll("svg#" + this.svg_id).selectAll("g.yaxis").selectAll("line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this.chart.append("text")
            .attr("class", "ylabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * -.22 + "," + this.height / 2 + ") rotate(-90)")
            .text(this.ylabel);
        // x axis
        this.x_scale = d3.scale.linear()
            .domain([this.x_min, this.x_max])
            .range([0, this.width]);
        this.chart.append("g")
            .attr("class", "xaxis")
            .attr("transform", "translate(0," + this.y_scale(this.y_min) + ")")
            .call(d3.svg.axis()
            .scale(this.x_scale)
            .orient("bottom")
            .ticks(5));
        d3.selectAll("svg#" + this.svg_id).selectAll("g.xaxis").selectAll("line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this.chart.append("text")
            .attr("class", "xlabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * .5 + "," + this.height * 1.2 + ")")
            .text(this.xlabel);
        // line
        var data = new Array();
        data.push([0.0, 0.0]);
        var line = this.chart
            .append('svg:path')
            .attr('id', 'graph');
        var line_style = [];
        line_style['fill'] = 'none';
        if (this.show_line) {
            line_style['stroke'] = 'blue';
            line_style['stroke-width'] = '2';
        }
        if (this.show_markers) {
            line_style['marker-start'] = 'url(#marker)';
            line_style['marker-mid'] = 'url(#marker)';
            line_style['marker-end'] = 'url(#marker)';
        }
        console.log(line_style);
        d3.selectAll("svg#" + this.svg_id).selectAll("path#graph").style(line_style);
    }
    SimpleLineChart.prototype.update = function (x_data, y_data) {
        var data = new Array();
        for (var i = 0; i < x_data.length; ++i) {
            var x = x_data[i];
            if (x >= this.x_min && x <= this.x_max) {
                var y = y_data[i];
                if (y < this.y_min)
                    continue; //y = this.y_min;
                if (y > this.y_max)
                    continue; //y = this.y_max;
                data.push([x, y]);
            }
        }
        var x_scale = this.x_scale;
        var y_scale = this.y_scale;
        var line_generator = d3.svg.line()
            .x(function (d) {
            return x_scale(d[0]);
        })
            .y(function (d) {
            return y_scale(d[1]);
        });
        var line = d3.selectAll("#" + this.svg_id + " path#graph")
            .attr("d", line_generator(data));
    };
    return SimpleLineChart;
})();
var SimpleLineAndDotChart = (function () {
    function SimpleLineAndDotChart(svg_id, xlabel, x_min, x_max, ylabel, y_min, y_max, show_line, show_markers) {
        if (show_line === void 0) { show_line = true; }
        if (show_markers === void 0) { show_markers = false; }
        this.svg_id = svg_id;
        this.xlabel = xlabel;
        this.x_min = x_min;
        this.x_max = x_max;
        this.ylabel = ylabel;
        this.y_min = y_min;
        this.y_max = y_max;
        this.show_line = show_line;
        this.show_markers = show_markers;
        this.width = 0;
        this.height = 0;
        this.x_off = 0;
        this.y_off = 0;
        this.width = jQuery("#" + this.svg_id).width();
        this.height = jQuery("#" + this.svg_id).height();
        var offset_factor = .1;
        this.x_off = 2. * this.width * offset_factor;
        this.y_off = this.height * offset_factor;
        this.chart = d3.selectAll("#" + this.svg_id).append("g")
            .attr("class", "main")
            .attr("transform", "translate(" + this.x_off + "," + this.y_off + ") scale(" + (1 - 3 * offset_factor) + ")")
            .style({ "font-size": "18pt" });
        // y axis
        this.y_scale = d3.scale.linear()
            .domain([this.y_min, this.y_max])
            .range([this.height, 0]);
        this.chart.append("g")
            .attr("class", "yaxis")
            .call(d3.svg.axis()
            .scale(this.y_scale)
            .orient("left")
            .ticks(5));
        d3.selectAll("svg#" + this.svg_id).selectAll("g.yaxis").selectAll("line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this.chart.append("text")
            .attr("class", "ylabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * -.22 + "," + this.height / 2 + ") rotate(-90)")
            .text(this.ylabel);
        // x axis
        this.x_scale = d3.scale.linear()
            .domain([this.x_min, this.x_max])
            .range([0, this.width]);
        this.chart.append("g")
            .attr("class", "xaxis")
            .attr("transform", "translate(0," + this.y_scale(this.y_min) + ")")
            .call(d3.svg.axis()
            .scale(this.x_scale)
            .orient("bottom")
            .ticks(5));
        d3.selectAll("svg#" + this.svg_id).selectAll("g.xaxis").selectAll("line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this.chart.append("text")
            .attr("class", "xlabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * .5 + "," + this.height * 1.2 + ")")
            .text(this.xlabel);
        // line
        var data = new Array();
        data.push([0.0, 0.0]);
        var line = this.chart
            .append('svg:path')
            .attr('id', 'graph_line');
        var line_style = [];
        line_style['fill'] = 'none';
        line_style['stroke'] = 'blue';
        line_style['stroke-width'] = '2';
        d3.selectAll("svg#" + this.svg_id).selectAll("svg path#graph_line").style(line_style);
        // line
        var dots = this.chart
            .append('svg:path')
            .attr('id', 'graph_dots');
        var dots_style = [];
        dots_style['fill'] = 'none';
        dots_style['stroke'] = 'none';
        dots_style['marker-start'] = 'url(#marker)';
        dots_style['marker-mid'] = 'url(#marker)';
        dots_style['marker-end'] = 'url(#marker)';
        d3.selectAll("svg#" + this.svg_id).selectAll("svg path#graph_dots").style(dots_style);
    }
    SimpleLineAndDotChart.prototype.update_line = function (x_data, y_data) {
        var data = new Array();
        for (var i = 0; i < x_data.length; ++i) {
            var x = x_data[i];
            if (x >= this.x_min && x <= this.x_max) {
                var y = y_data[i];
                if (y < this.y_min)
                    continue; //y = this.y_min;
                if (y > this.y_max)
                    continue; //y = this.y_max;
                data.push([x, y]);
            }
        }
        console.log(data);
        var x_scale = this.x_scale;
        var y_scale = this.y_scale;
        var line_generator = d3.svg.line()
            .x(function (d) {
            return x_scale(d[0]);
        })
            .y(function (d) {
            return y_scale(d[1]);
        });
        var line = d3.select("#" + this.svg_id + " path#graph_line")
            .attr("d", line_generator(data));
    };
    SimpleLineAndDotChart.prototype.update_dots = function (x_data, y_data) {
        var data = new Array();
        for (var i = 0; i < x_data.length; ++i) {
            var x = x_data[i];
            if (x >= this.x_min && x <= this.x_max) {
                var y = y_data[i];
                if (y < this.y_min)
                    continue; // y = this.y_min;
                if (y > this.y_max)
                    continue; // y = this.y_max;
                data.push([x, y]);
            }
        }
        var x_scale = this.x_scale;
        var y_scale = this.y_scale;
        var line_generator = d3.svg.line()
            .x(function (d) {
            return x_scale(d[0]);
        })
            .y(function (d) {
            return y_scale(d[1]);
        });
        var line = d3.select("#" + this.svg_id + " path#graph_dots")
            .attr("d", line_generator(data));
    };
    return SimpleLineAndDotChart;
})();
