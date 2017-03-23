
import $ = require("jquery");
export function defineReadings(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.readingTypes = {
        onScreen: 1,
        hoverPar: 2,
        clickPar: 3,
        clickRed: 4
    };
    sc.readClasses = {
        1: 'screen',
        2: 'hover',
        3: 'click',
        4: 'read'
    };

    sc.markParsRead = function ($pars) {
        var parIds = $pars.map(function (i, e) {
            return sc.getParId($(e));
        }).get();
        $pars.find('.readline').addClass(sc.readClasses[sc.readingTypes.clickRed]);
        http.put('/read/' + sc.docId + '/' + 'null' + '/' + sc.readingTypes.clickRed, {pars: parIds})
            .then(function (response) {
                sc.markPageDirty();
            }, function (response) {
                $log.error('Could not save the read markings');
            });
    };

    sc.markParRead = function ($par, readingType) {
        var $readline = $par.find('.readline');
        var readClassName = sc.readClasses[readingType];
        if ($readline.hasClass(readClassName)) {
            return q.resolve(null);
        }

        // If the paragraph is only a preview, ignore it.
        if ($par.parents('.previewcontent').length > 0 || $par.parents('.csrunPreview').length > 0) {
            return q.resolve(null);
        }
        var par_id = sc.getParId($par);
        if (par_id === 'NEW_PAR' || par_id === null || par_id === 'HELP_PAR') {
            return q.resolve(null);
        }
        $readline.addClass(readClassName);
        var data = {};
        if (sc.isReference($par)) {
            data = sc.getRefAttrs($par);
        }
        if (!Users.isLoggedIn()) return q.resolve(null);
        return http.put('/read/' + sc.docId + '/' + par_id + '/' + readingType, data)
            .then(function (response) {
                $readline.removeClass(readClassName + '-modified');
                if (readingType === sc.readingTypes.clickRed) {
                    sc.markPageDirty();
                    sc.refreshSectionReadMarks();
                }
            }, function (response) {
                $log.error('Could not save the read marking for paragraph ' + par_id);
                $readline.removeClass(readClassName);
            });
    };

    sc.onClick(".readline", function ($this, e) {
        sc.markParRead($this.parents('.par'), sc.readingTypes.clickRed);
        return true;
    });

    sc.onClick(".areareadline", function ($this, e) {
        var oldClass = $this.attr("class");
        $this.attr("class", "readline read");

        if (!Users.isLoggedIn()) return true;

        // Collapsible area
        var area_id = $this.parent().attr('data-area');
        $log.info($this);

        http.put('/read/' + sc.docId + '/' + area_id)
            .success(function (data, status, headers, config) {
                sc.getArea(area_id).find('.readline').attr('class', 'areareadline read');
                sc.markPageDirty();
            }).error(function () {
            $window.alert('Could not save the read marking.');
            $this.attr("class", oldClass);
        });

        return false;
    });

    $.expr[":"].onScreen = function (el) {
        var rect = el.getBoundingClientRect();

        return (
            rect.top >= 0 &&
            rect.left >= 0 &&
            rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
            rect.right <= (window.innerWidth || document.documentElement.clientWidth)
        );
    };

    sc.jQuery = $;
    sc.readPromise = null;
    sc.readingParId = null;

    sc.queueParagraphForReading = function () {
        //noinspection CssInvalidPseudoSelector
        var visiblePars = sc.jQuery('.par:onScreen').find('.readline').not('.' + sc.readClasses[sc.readingTypes.onScreen]);
        var parToRead = visiblePars.first().parents('.par');
        var parId = sc.getParId(parToRead);

        if (sc.readPromise !== null && sc.readingParId !== parId) {
            $timeout.cancel(sc.readPromise);
        } else if (sc.readingParId === parId) {
            return;
        }

        if (parToRead.length === 0) {
            return;
        }
        sc.readingParId = parId;
        var numWords = parToRead.find('.parContent').text().trim().split(/[\s\n]+/).length;
        sc.readPromise = $timeout(function () {
            sc.markParRead(parToRead, sc.readingTypes.onScreen).finally(sc.queueParagraphForReading);
        }, 300 * numWords);
    };

    $($window).scroll(sc.queueParagraphForReading);

    sc.queueParagraphForReading();

    sc.markAllAsRead = function () {
        http.put('/read/' + sc.docId)
            .success(function (data, status, headers, config) {
                $('.readline').attr("class", "readline read");
            }).error(function (data, status, headers, config) {
            $window.alert('Could not mark the document as read.');
        });
    };

    /**
     * Refreshes the section read marks.
     */
    sc.refreshSectionReadMarks = function () {
        $(".readsection").remove();
        for (var key in sc.sections) {
            if (sc.sections.hasOwnProperty(key)) {
                var sectionPars = sc.sections[key];
                var readlines = sectionPars.children('.readline');
                var modifiedCount = readlines.filter('.read-modified').not('.read').length;
                var unreadCount = readlines.not('.read-modified').not('.read').length;
                if (modifiedCount + unreadCount > 0) {
                    sectionPars.last().append($('<div>', {
                        class: 'readsection',
                        title: 'Mark preceding section as read (' +
                        sectionPars.length + ' paragraphs - ' + unreadCount +
                        ' unread, ' + modifiedCount + ' modified)'
                    }).html('<i class="glyphicon glyphicon-align-left"></i><i class="glyphicon glyphicon-ok"></i>'));
                }
            }
        }
    };

    sc.onClick('.readsection', function ($readsection, e) {
        var $par = $readsection.parents('.par');
        var $pars = sc.sections[sc.getParId($par)];
        if ($par.length === 0) {
            $window.alert('Unable to mark this section as read');
            return;
        }
        sc.markParsRead($pars);
        $readsection.remove();
    });

    sc.onMouseOverOut(".par", function ($this, e, select) {
        if (select) {
            sc.markParRead($this, sc.readingTypes.hoverPar);
        }
    });

    if (Users.isLoggedIn()) {
        $timeout(function () {
            http.post('/bookmarks/markLastRead/' + sc.docId, {}).then(function () {
                // all ok
            }, function () {
                $log.error('Failed to mark document as last read');
            });
        }, 10000);
    }
}
