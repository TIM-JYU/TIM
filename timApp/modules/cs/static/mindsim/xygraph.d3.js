var XYG_Axis = (function () {
    function XYG_Axis(label, min, max, auto) {
        if (label === void 0) { label = ""; }
        if (min === void 0) { min = 0; }
        if (max === void 0) { max = 1; }
        if (auto === void 0) { auto = false; }
        this.label = label;
        this.min = min;
        this.max = max;
        this.auto = auto;
    }
    return XYG_Axis;
})();
var XYG_Layout = (function () {
    function XYG_Layout(x_offset, y_offset, x_fraction, y_fraction, font_size) {
        this.x_offset = x_offset;
        this.y_offset = y_offset;
        this.x_fraction = x_fraction;
        this.y_fraction = y_fraction;
        this.font_size = font_size;
    }
    return XYG_Layout;
})();
var XYG_PlotStyle = (function () {
    function XYG_PlotStyle(line_color, line_width, line_type, marker_color, marker_size, marker_type) {
        if (line_color === void 0) { line_color = "rgba(0,0,0,1.0)"; }
        if (line_width === void 0) { line_width = 2; }
        if (line_type === void 0) { line_type = "solid"; }
        if (marker_color === void 0) { marker_color = "rgba(255,0,0,1.0)"; }
        if (marker_size === void 0) { marker_size = 4; }
        if (marker_type === void 0) { marker_type = "circle"; }
        this.line_color = line_color;
        this.line_width = line_width;
        this.line_type = line_type;
        this.marker_color = marker_color;
        this.marker_size = marker_size;
        this.marker_type = marker_type;
    }
    return XYG_PlotStyle;
})();
var XYGraph = (function () {
    function XYGraph(svg_id, x_axis, y_axis, layout, styles) {
        this.svg_id = svg_id;
        this.x_axis = x_axis;
        this.y_axis = y_axis;
        this.layout = layout;
        this.styles = styles;
        this._lines = null;
        this._dots = null;
        this.width = 0;
        this.height = 0;
        this.width = jQuery("svg#" + this.svg_id).width();
        this.height = jQuery("svg#" + this.svg_id).height();
        this._graph = d3.select("svg#" + this.svg_id);
        this._plot_area = this._graph.append("g")
            .attr("class", "area")
            .style({ "cursor": "crosshair" });
        this._plot_area.attr("transform", "translate("
            + (this.layout.x_offset * this.width) + ","
            + ((1 - this.layout.y_fraction) * this.height - this.layout.y_offset * this.height) + ")");
        for (var i = 0; i < styles.length; ++i) {
            this.createMarkers(i);
            this.createLineAndDots(i);
        }
        this.updateAxes();
    }
    ////////////////////////////////////////////////////////////////////////////
    XYGraph.prototype.updateAxes = function () {
        var w = this.width * this.layout.x_fraction;
        var h = this.height * this.layout.y_fraction;
        this._x_scale = d3.scale.linear()
            .domain([this.x_axis.min, this.x_axis.max])
            .range([0, w]);
        this._y_scale = d3.scale.linear()
            .domain([this.y_axis.min, this.y_axis.max])
            .range([h, 0]);
        this._plot_area.select("g.x_axis").remove();
        this._plot_area.select("g.y_axis").remove();
        var xscale = this._x_scale;
        this._plot_area.append("g")
            .attr("class", "x_axis axis")
            .attr("transform", "translate(0," + (h) + ")")
            .call(d3.svg.axis()
            .scale(xscale)
            .orient("bottom")
            .ticks(5));
        var yscale = this._y_scale;
        this._plot_area.append("g")
            .attr("class", "y_axis axis")
            .call(d3.svg.axis()
            .scale(yscale)
            .orient("left")
            .ticks(5));
        d3.select("svg#" + this.svg_id).selectAll("g.axis").selectAll("line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this._graph.select("text.x_label").remove();
        this._graph.select("text.y_label").remove();
        this._graph.append("text")
            .attr("class", "x_label label")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + (this.width * this.layout.x_offset + w * .5) + "," + (this.height - 0.5 * this.layout.font_size) + ")")
            .text(this.x_axis.label);
        this._graph.append("text")
            .attr("class", "y_label label")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + (this.layout.font_size) + "," + ((1 - this.layout.y_fraction) * this.height - this.layout.y_offset * this.height + h * .5) + ") rotate(-90)")
            .text(this.y_axis.label);
    };
    ////////////////////////////////////////////////////////////////////////////
    XYGraph.prototype.updateData = function (x_data, y_data) {
        var xmin = Number.NaN;
        var xmax = Number.NaN;
        var ymin = Number.NaN;
        var ymax = Number.NaN;
        for (var i = 0; i < y_data.length; ++i) {
            for (var j = 0; j < x_data[i].length; ++j) {
                if (isNaN(xmin))
                    xmin = x_data[i][j];
                if (isNaN(xmax))
                    xmax = x_data[i][j];
                if (isNaN(ymin))
                    ymin = y_data[i][j];
                if (isNaN(ymax))
                    ymax = y_data[i][j];
                xmin = Math.min(xmin, x_data[i][j]);
                xmax = Math.max(xmax, x_data[i][j]);
                ymin = Math.min(ymin, y_data[i][j]);
                ymax = Math.max(ymax, y_data[i][j]);
            }
        }
        var dx = xmax - xmin;
        if (this.x_axis.auto) {
            this.x_axis.min = xmin - 0.1 * dx;
            this.x_axis.max = xmax + 0.1 * dx;
        }
        var dy = ymax - ymin;
        if (this.y_axis.auto) {
            this.y_axis.min = ymin - 0.1 * dy;
            this.y_axis.max = ymax + 0.1 * dy;
        }
        if (this.x_axis.auto || this.y_axis.auto)
            this.updateAxes();
        var xscale = this._x_scale;
        var yscale = this._y_scale;
        var line_generator = d3.svg.line()
            .x(function (d) {
            return xscale(d[0]);
        })
            .y(function (d) {
            return yscale(d[1]);
        });
        var xy_data = new Array();
        for (var i = 0; i < y_data.length; ++i) {
            xy_data[i] = new Array();
            for (var j = 0; j < x_data[i].length; ++j) {
                var x = x_data[i][j];
                var y = y_data[i][j];
                //if ( y < this.y_axis.min ) y = this.y_axis.min;
                //if ( y > this.y_axis.max ) y = this.y_axis.max;
                if (x < this.x_axis.min || x > this.x_axis.max)
                    continue;
                xy_data[i].push([x, y]);
            }
        }
        var layout = this.layout;
        var plot_area = this._plot_area;
        this._plot_area
            .on("click", function (d) {
            //d3.selectAll("text.xyg_tooltip").remove();
            var cur_pos = d3.mouse(this);
            var x = xscale.invert(cur_pos[0]);
            var y = yscale.invert(cur_pos[1]);
            var xp = Math.log(Math.abs(x) + 1e-15) / Math.log(10.);
            var yp = Math.log(Math.abs(y) + 1e-15) / Math.log(10.);
            /*
            plot_area.append("text")
                .attr("class","xyg_tooltip")
                .attr("x", xscale(xmax+dx*0.1))
                .attr("y", yscale(ymax+dy*0.1)+layout.font_size)
                .attr("text-anchor", "end")
                .text(x.toFixed(Math.ceil(Math.max(0,-xp+2)))
                  + ", "
                  + y.toFixed(Math.ceil(Math.max(0,-yp+2))));
            */
            alert(x.toFixed(Math.ceil(Math.max(0, -xp + 2)))
                + ", "
                + y.toFixed(Math.ceil(Math.max(0, -yp + 2))));
        })
            .on("mouseout", function (d) {
            //jQuery("text.xyg_tooltip").fadeOut(3000,'swing');
        });
        for (var i = 0; i < y_data.length; ++i) {
            var line = d3.select("svg#" + this.svg_id + " path#xyg_line_" + i)
                .attr("d", line_generator(xy_data[i]));
        }
    };
    ////////////////////////////////////////////////////////////////////////////
    XYGraph.prototype.createLineAndDots = function (index) {
        var line = this._plot_area.append('svg:path')
            .attr('id', 'xyg_line_' + index);
        var style = [];
        style['fill'] = 'none';
        if (this.styles[index].line_type == 'none')
            style['stroke'] = 'none';
        else {
            style['stroke'] = this.styles[index].line_color;
            if (this.styles[index].line_type.indexOf('dashed:') == 0) {
                style['stroke-dasharray'] = this.styles[index].line_type.substring(7);
            }
        }
        style['stroke-width'] = this.styles[index].line_width;
        style['marker-start'] = 'url(#xyg_marker_' + index + ')';
        style['marker-mid'] = 'url(#xyg_marker_' + index + ')';
        style['marker-end'] = 'url(#xyg_marker_' + index + ')';
        d3.select("svg#" + this.svg_id).selectAll("path#xyg_line_" + index).style(style);
    };
    ////////////////////////////////////////////////////////////////////////////
    XYGraph.prototype.createMarkers = function (index) {
        var style = this.styles[index];
        var raw_svg = jQuery("svg#" + this.svg_id)[0];
        var defs = document.createElementNS(raw_svg.namespaceURI, 'defs');
        var marker = document.createElementNS(raw_svg.namespaceURI, 'marker');
        marker.setAttribute('id', 'xyg_marker_' + index);
        marker.setAttribute('markerWidth', 2 * style.marker_size);
        marker.setAttribute('markerHeight', 2 * style.marker_size);
        marker.setAttribute('refX', style.marker_size);
        marker.setAttribute('refY', style.marker_size);
        marker.setAttribute('stroke', 'black');
        marker.setAttribute('stroke-width', .25 * style.marker_size);
        marker.setAttribute('fill', style.marker_color);
        marker.setAttribute('orient', 'auto');
        if (style.marker_type == 'circle') {
            var tmp = document.createElementNS(raw_svg.namespaceURI, 'circle');
            tmp.setAttribute('cx', style.marker_size);
            tmp.setAttribute('cy', style.marker_size);
            tmp.setAttribute('r', style.marker_size * .75);
            marker.appendChild(tmp);
        }
        defs.appendChild(marker);
        raw_svg.appendChild(defs);
    };
    return XYGraph;
})();
