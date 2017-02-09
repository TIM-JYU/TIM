/* globals angular, $ */

var timApp = angular.module('timApp');

timApp.defineAreas = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.onMouseOverOut(".areaeditline1", function ($this, e, select) {
        var areaName = $this.attr('data-area');
        sc.selectArea(areaName, ".areaeditline1", select);
    });

    sc.onMouseOverOut(".areaeditline2", function ($this, e, select) {
        var areaName = $this.attr('data-area');
        sc.selectArea(areaName, ".areaeditline2", select);
    });

    sc.onMouseOverOut(".areaeditline3", function ($this, e, select) {
        var areaName = $this.attr('data-area');
        sc.selectArea(areaName, ".areaeditline3", select);
    });

    sc.onClick(".areaeditline1", function ($this, e) {
        return sc.onAreaEditClicked($this, e, ".areaeditline1");
    });

    sc.onClick(".areaeditline2", function ($this, e) {
        return sc.onAreaEditClicked($this, e, ".areaeditline2");
    });

    sc.onClick(".areaeditline3", function ($this, e) {
        return sc.onAreaEditClicked($this, e, ".areaeditline3");
    });

    sc.selectArea = function (areaName, className, selected) {
        var $selection = $('.area.area_' + areaName).children(className);
        if (selected)
            $selection.addClass('manualhover');
        else
            $selection.removeClass('manualhover');
    };

    sc.onAreaEditClicked = function ($this, e, className) {
        sc.closeOptionsWindow();
        var areaName = $this.attr('data-area');
        var $pars = sc.getArea(areaName).find('.par');
        var $area_part = $this.parent().filter('.area');
        var coords = {left: e.pageX - $area_part.offset().left, top: e.pageY - $area_part.offset().top};

        sc.selectedAreaName = areaName;
        $('.area.area_' + areaName).children(className).addClass('menuopen');

        // We need the timeout so we don't trigger the ng-clicks on the buttons
        $timeout(function () {
            sc.showAreaOptionsWindow(e, $area_part, $pars, coords);
        }, 80);
        return false;
    };

    sc.onClick(".areaexpand, .areacollapse", function ($this, e) {
        if ($(e.target).hasClass('areareadline') ||
            $(e.target).hasClass('readline') ||
            $(e.target).hasClass('editline'))
            return;
        var expanding = true;
        var newClass = 'areacollapse';
        if ($this.hasClass('areacollapse')) {
            expanding = false;
            newClass = 'areaexpand';
        }
        $this.removeClass("areaexpand areacollapse");
        var area_name = $this.attr('data-area');
        var toggle = $this.children('.areatoggle');
        toggle.attr('class', '');
        if (expanding) {
            sc.getArea(area_name).removeClass("collapsed");
            $('.areawidget_' + area_name).removeClass("collapsed");
            toggle.attr('class', 'areatoggle glyphicon glyphicon-minus');
        } else {
            sc.getArea(area_name).addClass("collapsed");
            $('.areawidget_' + area_name).addClass("collapsed");
            toggle.attr('class', 'areatoggle glyphicon glyphicon-plus');
        }

        $this.addClass(newClass);
    });

    sc.showAreaOptionsWindow = function (e, $area, $pars, coords) {
        sc.updateClipboardStatus();
        sc.showPopupMenu(e, $pars, coords, sc.popupMenuAttrs, $area, "area");
    };


    sc.startArea = function (e, $par) {
        sc.extendSelection($par);
    };

    sc.nameArea = function (e, $pars) {
        var $newArea = $('<div class="area" id="newarea" />');
        $newArea.attr('data-doc-id', sc.docId);
        sc.selection.pars.wrapAll($newArea);

        $newArea = $('#newarea');
        var $popup = $('<name-area>');
        $popup.attr('tim-draggable-fixed', '');
        $popup.attr('onok', 'nameAreaOk');
        $popup.attr('oncancel', 'nameAreaCancel');
        $newArea.prepend($popup);

        $compile($popup[0])(sc);
    };

    sc.nameAreaOk = function ($area, areaName, options) {
        $area.attr("data-name", areaName);

        http.post('/name_area/' + sc.docId + '/' + areaName, {
            "area_start": sc.getFirstParId($area.first()),
            "area_end": sc.getLastParId($area.last()),
            "options": options
        }).success(function (data, status, headers, config) {
            //$area.children().wrapAll('<div class="areaContent">');
            //$area.append('<div class="areaeditline1">');

            //if (options.collapsible)
            sc.reload();

        }).error(function (data, status, headers, config) {
            $window.alert(data.error);
            sc.nameAreaCancel($area);
        });
    };

    sc.nameAreaCancel = function ($area) {
        $area.children().unwrap();
    };

    sc.cancelArea = function (e, $par) {
        sc.selection.start = null;
        sc.selection.end = null;
    };

    sc.removeAreaMarking = function (e, $pars) {
        var area_name = sc.selectedAreaName;
        if (!area_name) {
            $window.alert("Could not get area name");
        }

        http.post('/unwrap_area/' + sc.docId + '/' + area_name, {}).success(function (data, status, headers, config) {
            sc.reload();
        }).error(function (data, status, headers, config) {
            $window.alert(data.error);
        });
    };
};
