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
        let ctx = chartInstance.ctx;
        if ( !ctx && chartInstance.chart)
            ctx = chartInstance.chart.ctx;
        if ( !ctx ) return;

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

// TIM jsframe function for ChartJS
var TIMJS = {};

TIMJS.basicoptions = {
    'type': 'bar',
    'data': {
        'labels': [1,2,3,4,5,6],
        'datasets': [
            {
                'label': '',
                'lineTension': 0,
                'fill': false,
                'backgroundColor': 'rgba(0,0,255,0.5)',
                'borderColor': '#0000ff',
                'borderWidth': 1,
                'data': [4,5,6,2,3,10],
            },
        ]
    },
    'options': {
        'responsive': true,
        'legend': { 'display': false, 'position': 'right', },
        'title': { 'display': true, 'text': '' },
        'scales': {
            'xAxes': [{ 'position': 'bottom', 'scaleLabel': {'labelString': '', 'display': true}}],
            'yAxes': [{'type': 'linear', 'position': 'left', 'scaleLabel': {'labelString': '', 'display': true}, 'display': true, 'ticks': { 'min': 0,  }}],
         }
    }
};

TIMJS.COLORS = [
    '#4dc9f6',
    '#f67019',
    '#f53794',
    '#537bc4',
    '#acc236',
    '#166a8f',
    '#00a950',
    '#58595b',
    '#8549ba'
];

TIMJS.color = Chart.helpers.color;

function pros(od) {
    // return od;
    let max = Math.max(...od);
    let a = [];
    for (let i=0; i<od.length; i++) {
       a[i] = Math.round(od[i]*1000/max)/10;
    }
    // console.log(max, a);
    return a;
}

/**
 * Add dtata to dastest from datas-object
 * @param datasets where to add
 * @param datas where from copy
 * @param keys keys to take from datas
 * @param dopros add prosentual value instead totaö value
 */
function addData(datasets, datas, keys, dopros) {
    let ci = 0;
    for (const v of keys) {
       let d = 	{
            lineTension: 0,
            label: ''+v+  (dopros ? ' %' : ''),
            fill: false,
            backgroundColor: TIMJS.color(TIMJS.COLORS[ci]).alpha(0.5).rgbString(),
            borderColor: TIMJS.COLORS[ci++],
            // borderDash: [3,10],
            borderWidth: 1,
        };
        if ( ci > TIMJS.COLORS.length ) ci = 0;
        let od = datas[v];
        if ( !od || od.length === 0) continue;
        d.data = dopros ? pros(od) : od;
        datasets.push(d);
    }
}


TIMJS.globaldata =  {};
TIMJS.chart = null;
TIMJS.originalData = {};
TIMJS.initData =  {};
TIMJS.options = TIMJS.basicoptions;

window.onload = function() {
    if ( TIMJS.initData ) TIMJS.globaldata = TIMJS.initData; // mergeDeep(globaldata, window.initData);
    TIMJS.setData(TIMJS, TIMJS.globaldata);
};


/**
 * Be sure that there is n items in datasets, if not clone previous item
 * @param datasets data sets array for data
 * @param n number of items needed at least
 */
function ensureDataSets(datasets, n) {
   let diff = n - datasets.length;
   if ( diff <= 0 ) return;
   for (let i = diff; i < n; i++ ) {
       datasets[i] = {};
       mergeDeep(datasets[i], datasets[i-1]);
   }
}

/**
 * Set chart data from data, needs global variables globaldata, chart, originaldata
 * @param P global data needed to work
 * @param data data to be added or merged to chart
 */
TIMJS.setData = function(P, data) {
  try {
    P.globaldata = data;
    // noinspection JSUnresolvedVariable
      if ( P.originalData ) {
        let newData = {};
        if ( P.chart ) P.originalData.datas = null; // prevent another add
        mergeDeep(newData, P.originalData, '#'); // do not loose possible !
        mergeDeep(newData, data);
        data = newData;
    }
    if ( !P.chart ) {
        let ar = data.aspectRatio || data.options && data.options.aspectRatio;
        if ( ar ) P.options.options.aspectRatio = ar;
        let ctx = document.getElementById('canvas').getContext('2d');
        P.chart = new Chart(ctx,P.options);
    }
    let datasets = P.chart.config.data.datasets;
    let coptions = P.chart.config.options;
    let dopros = data.dopros || false;
    let fieldindex = data.fieldindex || 0;

    if ( data.type ) {
        P.chart.config.type = data.type;
        if ( data.type === "scatter" || data.linearx) {
           coptions.scales.xAxes = [{ type: 'linear', position: "bottom", scaleLabel: {labelString: "", display: true}}];
           coptions.scales.yAxes = [{ type: 'linear', position: "left",   scaleLabel: {labelString: "", display: true}}];
        }
        if ( data.type === "scatter" ) {
           datasets[0].showLine = false; // for version 2.8.0
        }
    }
    if ( data.labels ) P.chart.data.labels = data.labels;
    if ( data.data ) {
       fieldindex++;
       datasets[0].data = dopros ? pros(data.data) : data.data;
    }
    if ( data.data2 || data.label2 || data.dataopt2 ) {
        ensureDataSets(datasets, 2);
        datasets[1].backgroundColor = 'rgba(0,127,0,0.5)';
		datasets[1].borderColor = '#080';
    }
    if ( data.data2 ) {
       if ( fieldindex === 1 ) fieldindex = 2;
       datasets[1].data = dopros ? pros(data.data2) : data.data2;
    }
    if ( data.label ) datasets[0].label = data.label;
    if ( data.title) P.chart.options.title.text = data.title; // Tämä pitää olla näin
    if ( data.xlabel ) coptions.scales.xAxes[0].scaleLabel.labelString = data.xlabel;
    if ( data.ylabel) coptions.scales.yAxes[0].scaleLabel.labelString = data.ylabel;
    if ( data.label && datasets.length > 1 ) coptions.legend.display = true;
    if ( data.label2 ) {
        coptions.legend.display = true;
        datasets[1].label = data.label2;
    }
    if (typeof data.legend != "undefined") {
        // noinspection EqualityComparisonWithCoercionJS
        if ( data.legend == false ) {
           coptions.legend.display = false;
        // noinspection EqualityComparisonWithCoercionJS
        } else if ( data.legend == true ) {
           coptions.legend.display = true;
        } else {
           coptions.legend.display = true;
           coptions.legend.position = data.legend;
        }
    }
    let fdata = data.fielddata || P.fieldData;
    if ( fdata ) {
       ensureDataSets(datasets, fieldindex+1);
  	   if ( fieldindex === 0 ) P.data.labels = fdata.graphdata.labels;
       datasets[fieldindex].data = fdata.graphdata.data;
       datasets[fieldindex].backgroundColor = 'rgba(255,0,0,0.5)';
  	   datasets[fieldindex].borderColor = '#F00';
    }
    if ( data.trend ) {  // TODO: for every data
        let w = data.trend;
        if ( w === true ) w = 2;
        datasets[0].trendlineLinear = {
            'style': "rgba(255,105,180, .8)",
            'lineStyle': "dotted|solid",
            'width': w
        };
    }
    if ( data.datas ) {
        let keys = data.datakeys || Object.keys(data.datas);
        addData(datasets, data.datas, keys, dopros);
    }

    if ( data.options ) { mergeDeep(P.chart.options, data.options); }
    if ( data.dataopt ) { mergeDeep(datasets[0], data.dataopt); }
    if ( data.dataopt2 ) { mergeDeep(datasets[1], data.dataopt2); }

    P.chart.update();
  } catch(err) {
     let cont = document.getElementById('container');
     let p = document.createElement("p");
     let textnode = document.createTextNode(err.message);
     p.appendChild(textnode);
     cont.insertBefore(p, cont.firstChild);
  }
};

function setData(data) {
    TIMJS.setData(TIMJS, data);
}

function getData() {
    return TIMJS.globaldata;
}
