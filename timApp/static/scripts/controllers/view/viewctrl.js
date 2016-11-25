/* globals angular, timLogTime, $ */

var timApp = angular.module('timApp');

timApp.controller("ViewCtrl", [
    '$scope',
    '$http',
    '$q',
    '$injector',
    '$compile',
    '$window',
    '$document',
    '$rootScope',
    '$localStorage',
    '$filter',
    '$timeout',
    '$log',
    'Users',
    function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
        "use strict";
        timLogTime("ViewCtrl start", "view");
        sc.noBrowser = $window.noBrowser;
        sc.docId = $window.item.id;
        sc.docName = $window.item.path;
        sc.docVersion = $window.docVersion;
        sc.crumbs = $window.crumbs;
        sc.item = $window.item;
        sc.startIndex = $window.startIndex;
        sc.users = $window.users;
        sc.group = $window.group;
        sc.teacherMode = $window.teacherMode;
        sc.velpMode = $window.velpMode;
        sc.lectureMode = $window.lectureMode;
        if (sc.users.length > 0) {
            sc.selectedUser = sc.users[0];
        } else {
            sc.selectedUser = Users.getCurrent();
        }
        sc.hidePending = false;
        sc.pendingUpdates = {};

        // from https://stackoverflow.com/a/7317311
        $window.onload = function () {
            sc.getAndEditQuestions();
            $window.addEventListener("beforeunload", function (e) {
                if (!sc.editing) {
                    return undefined;
                }

                var msg = 'You are currently editing something. Are you sure you want to leave the page?';

                (e || $window.event).returnValue = msg; //Gecko + IE
                return msg; //Gecko + Webkit, Safari, Chrome etc.
            });
        };

        sc.reload = function () {
            sc.markPageNotDirty();
            $window.location.reload();
        };

        sc.closeRefreshDlg = function () {
            sc.showRefresh = false;
        };

        sc.markPageDirty = function () {
            var e = angular.element('#page_is_dirty');
            e.val('1');
        };

        sc.markPageNotDirty = function () {
            var e = angular.element('#page_is_dirty');
            e.val('0');
        };

        sc.isPageDirty = function () {
            var e = angular.element('#page_is_dirty');
            return e.val() === '1';
        };

        sc.showDialog = function (message) {
            $('<div id="dialog"><p>' + message + '</div>').dialog({
                dialogClass: "no-close", modal: true,
                close: function (event, ui) {
                    $(this).dialog("close");
                    $(this).remove();
                },
                buttons: [
                    {
                        text: "OK",
                        click: function () {
                            $(this).dialog("close");
                        }
                    }
                ]
            });
        };

        sc.$on('showDialog', function (event, message) {
            sc.showDialog(message);
        });

        sc.changeUser = function (user, updateAll) {
            sc.selectedUser = user;
            sc.$broadcast('userChanged', {user: user, updateAll: updateAll});
        };

        $($window).resize(function (e) {
            if (e.target === $window) {
                var newWidth = $($window).width();
                if (newWidth !== sc.oldWidth) {
                    sc.oldWidth = newWidth;
                    var selected = $('.par.lightselect, .par.selected');
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

        sc.scrollToElement = function (element) {
            var viewport = {};
            viewport.top = $(window).scrollTop();
            viewport.bottom = viewport.top + $(window).height();
            var bounds = {};
            bounds.top = element.offset().top;
            bounds.bottom = bounds.top + element.outerHeight();
            var y = $(window).scrollTop();
            if (bounds.bottom > viewport.bottom) {
                y += (bounds.bottom - viewport.bottom);
            }
            else if (bounds.top < viewport.top) {
                y += (bounds.top - viewport.top);
            }
            $('html, body').animate({
                scrollTop: y
            }, 500);
        };

        sc.beginUpdate = function () {
            http.get('/getUpdatedPars/' + sc.docId)
                .success(function (data, status, headers, config) {
                    sc.updatePendingPars(data.changed_pars);
                })
                .error(function () {
                    $window.alert('Error occurred when getting updated paragraphs.');
                });
        };

        sc.pendingUpdatesCount = function () {
            return Object.keys(sc.pendingUpdates).length;
        };

        sc.showUpdateDialog = function () {
            return !sc.hidePending && sc.pendingUpdatesCount() > 0;
        };

        sc.updatePendingPars = function (pars) {
            angular.extend(sc.pendingUpdates, pars);
            sc.hidePending = false;
            if (sc.pendingUpdatesCount() < 10) {
                sc.updatePending();
            }
        };

        sc.updatePending = function () {
            for (var key in sc.pendingUpdates) {
                if (sc.pendingUpdates.hasOwnProperty(key)) {
                    var $par = sc.getElementByParId(key);
                    var $newPar = $($compile(sc.pendingUpdates[key])(sc));
                    $par.replaceWith($newPar);
                    sc.applyDynamicStyles($newPar);
                    sc.processAllMathDelayed($newPar);
                }
            }
            sc.rebuildSections();
            sc.pendingUpdates = {};
            if (sc.lectureMode) {
                sc.getAndEditQuestions();
            }
        };

        sc.applyDynamicStyles = function ($par) {
            if ($window.editMode) {
                $par.addClass('editmode');

                // Show hidden paragraphs if in edit mode
                $par.find('.mdcontent').css('display', 'initial');
            }
        };

        sc.dist = function (coords1, coords2) {
            return Math.sqrt(Math.pow(coords2.left - coords1.left, 2) + Math.pow(coords2.top - coords1.top, 2));
        };

        sc.setHeaderLinks = function () {
            var pars = $(".parContent");
            pars.each(function () {
                var $p = $(this);
                $p.find('h1, h2, h3, h4, h5, h6').each(function () {
                    var $h = $(this);
                    var id = $h.attr('id');
                    if (angular.isDefined(id)) {
                        $h.append($("<a>", {
                            text: '#',
                            href: '#' + id,
                            class: 'headerlink',
                            title: 'Permanent link'
                        }));
                    }
                });
            });
        };

        /**
         * Rebuilds the sections and refreshes the section read marks.
         */
        sc.rebuildSections = function () {
            $(".readsection").remove();
            sc.sections = {};
            sc.buildSections(sc.sections, $(), $('#pars'));
            sc.refreshSectionReadMarks();
        };

        /**
         * Builds a dictionary of sections that maps the last paragraph id of each section to the section paragraphs.
         *
         * @param {object} sections The dictionary to which to build the sections.
         * @param {jQuery} $currentSectionPars The collection of paragraphs in the current section being processed.
         * @param {jQuery} $container The container element where the paragraphs are located.
         * @returns {jQuery} The collection of paragraphs in the current section being processed.
         */
        sc.buildSections = function (sections, $currentSectionPars, $container) {
            var $child = $container.children(".par:first");
            while ($child.length > 0) {
                if ($child.hasClass('area')) {
                    $currentSectionPars = sc.buildSections(sections, $currentSectionPars, $child.find('.areaContent'));
                }
                else if ($child.hasClass('par')) {
                    var attrs = sc.getParAttributes($child);
                    var refAttrs = sc.getRefAttrs($child)['ref-attrs'];
                    var content = $child.children('.parContent');
                    if (content.is(':visible')) {
                        if (content.children('h1, h2, h3').length > 0) {
                            if ($currentSectionPars.length > 0) {
                                var parId = sc.getParId($currentSectionPars.last());
                                sections[parId] = $currentSectionPars;
                            }
                            $currentSectionPars = $child;
                        } else if (!attrs.hasOwnProperty('settings') && !attrs.hasOwnProperty('area') && !attrs.hasOwnProperty('area_end') && !refAttrs.hasOwnProperty('area') && !refAttrs.hasOwnProperty('area_end')) {
                            $currentSectionPars = $currentSectionPars.add($child);
                        }
                    }
                }
                else if ($child.hasClass('addBottomContainer')) {
                    sections[sc.getParId($currentSectionPars.last())] = $currentSectionPars;
                }
                $child = $child.next();
            }
            return $currentSectionPars;
        };

        sc.getEditMode = function () {
            return $window.editMode;
        };
        sc.getAllowMove = function () {
            return $window.allowMove;
        };

        /**
         * Gets the parent element of the given element.
         * @method getElementParent
         * @param element - Element whose parent is queried for
         * @returns {Element} Element parent
         */
        sc.getElementParent = function (element) {
            /*
             if (typeof element.parentElement !== "undefined")
             return element.parentElement;
             */
            if (!element) return null;
            var parent = element.parentNode;
            if (!parent) return null;
            if (typeof parent.tagName !== "undefined") {
                return parent;
            }

            sc.getElementParent(parent);
        };

        sc.nothing = function () {
        };

        timApp.defineEventHandlers(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineAreas(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineClipboard(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineEditing(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineIndex(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineMath(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineNotes(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineParHelpers(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineParMenu(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineQuestions(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineReadings(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);
        timApp.defineRefPopup(sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users);

        // Call necessary initialization functions below this line. Define any scope functions above this line.

        sc.onClick("html.ng-scope", function ($this, e) {
            // Clicking anywhere
            var tagName = e.target.tagName.toLowerCase();
            var jqTarget = $(e.target);
            var ignoreTags = ['button', 'input', 'label', 'i'];
            var ignoreClasses = ['menu-icon', 'editline', 'areaeditline', 'draghandle', 'actionButtons'];

            var curElement = jqTarget;
            var limit = 10;
            while (curElement !== null) {
                //$log.info(curElement);

                if (sc.editing || $.inArray(tagName, ignoreTags) >= 0 || curElement.attr('position') === 'absolute')
                    return false;

                for (var i = 0; i < ignoreClasses.length; i++) {
                    if (curElement.hasClass(ignoreClasses[i]))
                        return false;
                }

                curElement = curElement.parent();
                if (--limit < 0) {
                    //$log.info('Limit reached');
                    break;
                }
            }

            sc.closeOptionsWindow();

            if (tagName !== "p") {
                $(".selected").removeClass("selected");
                $(".lightselect").removeClass("lightselect");
            }

            //$log.info(e.target);
            return false;

        }, true);

        sc.setHeaderLinks();
        sc.rebuildSections();

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $('body,html').bind('scroll wheel DOMMouseScroll mousewheel', function (e) {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        sc.$watchGroup(['lectureMode', 'selection.start', 'selection.end', 'editing', 'getEditMode()',
            'allowPasteContent', 'allowPasteRef', 'getAllowMove()'], function (newValues, oldValues, scope) {
            sc.updatePopupMenu();
            if (sc.editing) {
                sc.notification = "Editor is already open.";
            } else {
                sc.notification = "";
            }
        });

        sc.$storage = $localStorage.$default({
            defaultAction: "Show options window",
            noteAccess: 'everyone'
        });

        $window.allowMove = false;
        sc.oldWidth = $($window).width();
        sc.showRefresh = sc.isPageDirty();

        try {
            var found = $filter('filter')(sc.editorFunctions,
                {desc: sc.$storage.defaultAction}, true);
            if (found.length) {
                sc.defaultAction = found[0];
            }
        } catch (e) {
        }
        timLogTime("ViewCtrl end", "view");
    }
]);
