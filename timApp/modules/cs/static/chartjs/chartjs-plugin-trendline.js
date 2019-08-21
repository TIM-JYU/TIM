/*!
 * chartjs-plugin-trendline.js
 * Version: 0.1.1
 *
 * Copyright 2017 Marcus Alsterfjord
 * Released under the MIT license
 * https://github.com/Makanz/chartjs-plugin-trendline/blob/master/README.md
 *
 * Mod by: vesal: accept also xy-data so works with scatter
 */
var pluginTrendlineLinear = {
    beforeDraw: function(chartInstance) {
        let yScale;
        let xScale;
        for (let axis in chartInstance.scales) {
            if ( axis[0] == 'x')
                xScale = chartInstance.scales[axis];
            else
                yScale = chartInstance.scales[axis];
            if ( xScale && yScale ) break;
        }
        let ctx = chartInstance.chart.ctx;

        chartInstance.data.datasets.forEach(function(dataset, index) {
            if (dataset.trendlineLinear && chartInstance.isDatasetVisible(index)) {
                let datasetMeta = chartInstance.getDatasetMeta(index);
                addFitter(datasetMeta, ctx, dataset, xScale, yScale);
            }
        });

        ctx.setLineDash([]);
    }
};

function addFitter(datasetMeta, ctx, dataset, xScale, yScale) {
    let style = dataset.trendlineLinear.style || dataset.borderColor;
    let lineWidth = dataset.trendlineLinear.width || dataset.borderWidth;
    let lineStyle = dataset.trendlineLinear.lineStyle || "solid";

    style = (style !== undefined) ? style : "rgba(169,169,169, .6)";
    lineWidth = (lineWidth !== undefined) ? lineWidth : 3;

    let fitter = new LineFitter();
    let lastIndex = dataset.data.length - 1;
    let startPos = 0;
    let endPos = 0;
    if ( lastIndex > 0) {
        startPos = datasetMeta.data[0]._model.x;
        endPos = datasetMeta.data[lastIndex]._model.x;
    }

    let xy = false;
    if ( dataset.data && typeof dataset.data[0] === 'object') xy = true;

    dataset.data.forEach(function(data, index) {
        if ( xy ) fitter.add(data.x, data.y);
        else fitter.add(index, data);
    });

    let x1 = xScale.getPixelForValue(fitter.minx);
    let x2 = xScale.getPixelForValue(fitter.maxx);
    let y1 = yScale.getPixelForValue(fitter.f(fitter.minx));
    let y2 = yScale.getPixelForValue(fitter.f(fitter.maxx));
    if ( !xy ) { x1 = startPos; x2 = endPos; }

    ctx.lineWidth = lineWidth;
    if (lineStyle === "dotted") { ctx.setLineDash([2, 3]); }
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.lineTo(x2, y2);
    ctx.strokeStyle = style;
    ctx.stroke();
}

Chart.plugins.register(pluginTrendlineLinear);

function LineFitter() {
    this.count = 0;
    this.sumX = 0;
    this.sumX2 = 0;
    this.sumXY = 0;
    this.sumY = 0;
    this.minx = 1e100;
    this.maxx = -1e100;
}

LineFitter.prototype = {
    'add': function (x, y) {
        this.count++;
        this.sumX += x;
        this.sumX2 += x * x;
        this.sumXY += x * y;
        this.sumY += y;
        if ( x < this.minx ) this.minx = x;
        if ( x > this.maxx ) this.maxx = x;
    },
    'f': function (x) {
        let det = this.count * this.sumX2 - this.sumX * this.sumX;
        let offset = (this.sumX2 * this.sumY - this.sumX * this.sumXY) / det;
        let scale = (this.count * this.sumXY - this.sumX * this.sumY) / det;
        return offset + x * scale;
    }
};

/**
 * Simple is object check.
 * @param item
 * @returns {boolean}
 */
function isObject(item) {
  return (item && typeof item === 'object' && !Array.isArray(item));
}

/**
 * Deep merge two objects.
 * @param target object where to merge
 * @param source object where from merge
 * @forcechar char for starting the attribute name when no deepcopy is done, instead object reference
 */
function mergeDeep(target, source, forcechar) {
  if (isObject(target) && isObject(source)) {
    if ( !forcechar ) forcechar = '!';
    for (let key in source) {
      if ( key.startsWith('_') ) { continue; }
      if ( key.startsWith(forcechar) ) {
          target[key.substring(1)] = source[key];
          continue;
      }
      if (isObject(source[key])) {
        // if (!target[key]) Object.assign(target, { [key]: {} });
        if (!target[key]) target[key] =  {};
        mergeDeep(target[key], source[key], forcechar);
      } else {
        // Object.assign(target, { [key]: source[key] });
        target[key] = source[key];
      }
    }
  }
  return target;
}

