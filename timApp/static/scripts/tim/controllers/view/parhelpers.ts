import angular from "angular";
import $ from "jquery";

export function defineParHelpers(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.getParId = function($par) {
        if ($par === null || $par.length === 0 || !$par.hasClass("par")) {
            return null;
        }
        return $par.attr("id");
    };

    sc.getParAttributes = function($par) {
        return JSON.parse($par.attr("attrs"));
    };

    sc.dereferencePar = function($par) {
        if ($par.length === 0 || !$par.hasClass("par")) {
            return null;
        }
        if ($par.attr("ref-id") && $par.attr("ref-doc-id")) {
            return [$par.attr("ref-doc-id"), $par.attr("ref-id")];
        }

        return [sc.docId, $par.attr("id")];
    };

    sc.getAreaDocId = function($area) {
        if (!$area.hasClass("area")) {
            return null;
        }

        return $area.attr("data-doc-id");
    };

    sc.getFirstPar = function($par_or_area) {
        if ($par_or_area.length > 1) {
            return sc.getFirstPar($par_or_area.first());
        }
        if ($par_or_area.hasClass("area")) {
            return $par_or_area.find(".par").first();
        }
        if ($par_or_area.hasClass("par")) {
            return $par_or_area;
        }

        return null;
    };

    sc.getLastPar = function($par_or_area) {
        if ($par_or_area.length > 1) {
            return sc.getLastPar($par_or_area.last());
        }
        if ($par_or_area.hasClass("area")) {
            return $par_or_area.find(".par").last();
        }
        if ($par_or_area.hasClass("par")) {
            return $par_or_area;
        }

        return null;
    };

    sc.getFirstParId = function($par_or_area) {
        return sc.getParId(sc.getFirstPar($par_or_area));
    };

    sc.getLastParId = function($par_or_area) {
        return sc.getParId(sc.getLastPar($par_or_area));
    };

    sc.getNextPar = function($par) {
        let $next = $par.next();
        if ($next.hasClass("par")) {
            return $next;
        }
        if ($next.hasClass("area")) {
            let $content = $next.children(".areaContent");
            if ($content.length > 0) {
                let $firstpar = $content.children(".par:first");
                if ($firstpar.length > 0) {
                    return $firstpar;
                }
            }
        }
        return null;
    };

    sc.getElementByParId = function(id) {
        return $("#" + id);
    };

    sc.getElementByParHash = function(t) {
        return $("[t='" + t + "']");
    };

    sc.getRefAttrs = function($par) {
        return {
            "ref-id": $par.attr("ref-id"),
            "ref-t": $par.attr("ref-t"),
            "ref-doc-id": $par.attr("ref-doc-id"),
            "ref-attrs": JSON.parse($par.attr("ref-attrs") || "{}"),
        };
    };

    sc.forEachParagraph = function(func) {
        $(".paragraphs .par").each(func);
    };

    sc.getElementByRefId = function(ref) {
        return $(".par[ref-id='" + ref + "']");
    };

    sc.isReference = function($par) {
        return angular.isDefined($par.attr("ref-id"));
    };

    sc.isParWithinArea = function($par) {
        return sc.selection.pars.filter($par).length > 0;
    };

    sc.getParIndex = function($par) {
        let par_id = sc.getParId($par);
        let $pars = $(".par");
        let realIndex = 0;

        if ($par.find(".parContent").not(":empty").length === 0)
            return null;

        for (let i = 0; i < $pars.length; i++) {
            let $node = $pars.eq(i);
            if ($node.find(".parContent").not(":empty").length === 0)
                continue;

            if (sc.getParId($node) === par_id) {
                //$log.info('sc.getParIndex(' + par_id + ') = ' + realIndex);
                return realIndex;
            }

            realIndex++;
        }
    };

    sc.getArea = function(area) {
        return $(".area_" + area);
    };

    sc.getPars = function($par_first, $par_last) {
        let pars = [$par_first];
        let $par = $par_first;
        let $next = $par.next();
        let i = 1000000;

        while (i > 0) {
            if ($next.length === 0) {
                $par = $par.parent();
                $next = $par.next();
                if ($par.prop("tagName").toLowerCase() === "html") {
                    break;
                }

                continue;
            }

            if ($next.hasClass("area")) {
                $next = $next.children(".areaContent").children().first();
                continue;
            }

            if ($next.hasClass("par") && sc.getParIndex($next) !== null) {
                pars.push($next);
                if ($next.is($par_last))
                    break;
            }

            $par = $next;
            $next = $par.next();
            i -= 1;
        }

        return $(pars).map(function() {
            return this.toArray();
        });
    };
}
