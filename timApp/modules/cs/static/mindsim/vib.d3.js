//var data = [1,3,4,2,0];
//var Emax = 5;
var SimpleBarChart = (function () {
    function SimpleBarChart(svg_id, x_labels, y_max, xlabel, ylabel, invert_color) {
        this.svg_id = svg_id;
        this.x_labels = x_labels;
        this.y_max = y_max;
        this.xlabel = xlabel;
        this.ylabel = ylabel;
        this.invert_color = invert_color;
        this.width = 0;
        this.height = 0;
        this.x_max = 0;
        this.x_off = 0;
        this.y_off = 0;
        this.width = jQuery("#" + this.svg_id).width();
        this.height = jQuery("#" + this.svg_id).height();
        this.x_max = x_labels.length;
        var offset_factor = .1;
        this.x_off = 1.5 * this.width * offset_factor;
        this.y_off = this.height * offset_factor;
        this.chart = d3.select("#" + this.svg_id).append("g")
            .attr("transform", "translate(" + this.x_off + "," + this.y_off + ") scale(" + (1 - 3 * offset_factor) + ")")
            .style({ "font-size": "18pt" });
        // y axis
        this.y_scale = d3.scale.linear()
            .domain([0, this.y_max])
            .range([this.height, 0]);
        this.chart.append("g")
            .attr("class", "yaxis")
            .call(d3.svg.axis()
            .scale(this.y_scale)
            .orient("left")
            .ticks(5));
        d3.selectAll("g.yaxis line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        this.chart.append("text")
            .attr("class", "ylabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * -.1 + "," + this.height / 2 + ") rotate(-90)")
            .text(this.ylabel);
        // x axis
        this.x_scale = d3.scale.linear()
            .domain([0, this.x_max])
            .range([0, this.width]);
        this.chart.append("g")
            .attr("class", "xaxis")
            .attr("transform", "translate(0," + this.y_scale(0) + ")")
            .call(d3.svg.axis()
            .scale(this.x_scale)
            .orient("bottom")
            .ticks(5));
        d3.selectAll("g.xaxis line, path").style({ 'fill': 'none', 'stroke': 'black', 'stroke-width': '2' });
        d3.selectAll("g.xaxis g.tick").style({ 'opacity': '0.0' });
        this.chart.append("text")
            .attr("class", "xlabel")
            .attr("text-anchor", "middle")
            .attr("transform", "translate(" + this.width * .5 + "," + this.height * 1.2 + ")")
            .text(this.xlabel);
        // data
        var bar_spacing = this.width / this.x_max;
        ;
        var bar_width = bar_spacing * .6;
        var y_scale = this.y_scale;
        var y_max = this.y_max;
        var data = new Array();
        for (var i = 0; i < this.x_labels.length; ++i)
            data.push(0.0);
        var bars = this.chart.selectAll(".bar")
            .data(data);
        bars.exit().remove();
        bars.enter().append("rect")
            .attr("class", "bar")
            .attr("x", function (value, index) {
            return "" + (index * bar_spacing
                + (bar_spacing - bar_width) * .5);
        })
            .attr("width", function (value, index) {
            return "" + bar_width;
        })
            .attr("y", function (value, index) {
            return "" + (y_scale(Math.max(value, 0.01 * y_max)));
        })
            .attr("height", function (value, index) {
            return "" + (y_scale(y_max - Math.max(value, 0.01 * y_max)));
        })
            .attr("fill", function (value, index) {
            var r = Math.floor((255 * value) / y_max);
            var b = Math.floor(255 - (255 * value) / y_max);
            if (invert_color)
                return "rgb(" + b + ",0," + r + ")";
            else
                return "rgb(" + r + ",0," + b + ")";
        })
            .attr("stroke", "rgba(0,0,0,.5)")
            .attr("stroke-width", "3pt");
        var x_labels = this.x_labels;
        var labels = this.chart.selectAll(".label")
            .data(data);
        labels.enter().append("text")
            .attr("class", "label")
            .attr("x", function (value, index) {
            return "" + ((index + .5) * bar_spacing);
        })
            .attr("width", function (value, index) {
            return "" + bar_width;
        })
            .attr("y", function (value, index) {
            return "" + (y_scale(-y_max * 0.04));
        })
            .attr("dy", ".75em")
            .text(function (value, index) { return "" + x_labels[index]; })
            .style({ 'text-anchor': 'middle' });
        var vlabels = this.chart.selectAll(".vlabel")
            .data(data);
        vlabels.enter().append("text")
            .attr("class", "vlabel")
            .attr("x", function (value, index) {
            return "" + ((index + .5) * bar_spacing);
        })
            .attr("width", function (value, index) {
            return "" + bar_width;
        })
            .attr("y", function (value, index) {
            return "" + (y_scale(y_max));
        })
            .attr("dy", "-.75em")
            .text(function (value, index) { return "" + value.toFixed(1); })
            .style({ 'text-anchor': 'middle',
            'font-size': '15pt' });
        vlabels.exit().remove();
        if (x_labels.length > 11) {
            labels.style({ 'display': 'none' });
            vlabels.style({ 'display': 'none' });
        }
    }
    SimpleBarChart.prototype.update = function (data) {
        var bar_spacing = this.width / this.x_max;
        ;
        var bar_width = bar_spacing * .6;
        var y_scale = this.y_scale;
        var y_max = this.y_max;
        var invert_color = this.invert_color;
        var bars = this.chart.selectAll(".bar").data(data)
            .attr("x", function (value, index) {
            return "" + (index * bar_spacing
                + (bar_spacing - bar_width) * .5);
        })
            .attr("width", function (value, index) {
            return "" + bar_width;
        })
            .attr("y", function (value, index) {
            return "" + (y_scale(Math.max(value, 0.01 * y_max)));
        })
            .attr("height", function (value, index) {
            return "" + (y_scale(y_max - Math.max(value, 0.01 * y_max)));
        })
            .attr("fill", function (value, index) {
            var r = Math.floor((255 * value) / y_max);
            var b = Math.floor(255 - (255 * value) / y_max);
            if (invert_color)
                return "rgb(" + b + ",0," + r + ")";
            else
                return "rgb(" + r + ",0," + b + ")";
        })
            .attr("stroke", "rgba(0,0,0,.5)")
            .attr("stroke-width", "3pt");
        var vlabels = this.chart.selectAll(".vlabel").data(data)
            .text(function (value, index) { return "" + value.toFixed(1); });
    };
    return SimpleBarChart;
})();
;
