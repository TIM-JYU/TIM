/* globals angular, $ */

var timApp = angular.module('timApp');

timApp.defineRefPopup = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.onMouseOver('.parlink', function ($this, e) {
        sc.over_reflink = true;

        var $par = $this.parents('.par').find('.parContent');
        var coords = {left: e.pageX - $par.offset().left + 10, top: e.pageY - $par.offset().top + 10};
        var params;

        try {
            params = {
                docid: $this[0].attributes['data-docid'].value,
                parid: $this[0].attributes['data-parid'].value
            };
        } catch (TypeError) {
            // The element was modified
            return;
        }

        sc.showRefPopup(e, $this, coords, params);
    });

    sc.onMouseOver('.ref-popup', function ($this, e) {
        sc.over_popup = true;
    });

    sc.onMouseOut('.ref-popup', function ($this, e) {
        sc.over_popup = false;
        sc.hideRefPopup();
    });

    sc.onMouseOut('.parlink', function ($this, e) {
        sc.over_reflink = false;
        sc.hideRefPopup();
    });

    sc.showRefPopup = function (e, $ref, coords, attrs) {
        var $popup = $('<ref-popup>');
        $popup.offset(coords);

        for (var attr in attrs) {
            if (attrs.hasOwnProperty(attr)) {
                $popup.attr(attr, attrs[attr]);
            }
        }

        $ref.parent().prepend($popup); // need to prepend to DOM before compiling
        $compile($popup[0])(sc);
        return $popup;
    };

    sc.hideRefPopup = function () {
        if (sc.over_reflink || sc.over_popup)
            return;

        $(".refPopup").remove();
    };
};
