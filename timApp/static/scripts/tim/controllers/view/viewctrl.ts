
import angular from "angular";
import $ from "jquery";
import ngStorage from "ngstorage";
import {timApp} from "tim/app";
import {defineAreas} from "tim/controllers/view/areas";
import {defineClipboard} from "tim/controllers/view/clipboard";
import {defineEditing} from "tim/controllers/view/editing";
//noinspection TypeScriptPreferShortImport
import {initIndex} from "tim/controllers/view/index";
import * as interceptor from "tim/controllers/view/interceptor";
import {defineNotes} from "tim/controllers/view/notes";
import {getElementByParId} from "tim/controllers/view/parhelpers";
import {closeOptionsWindow, defineParMenu} from "tim/controllers/view/parmenu";
import {defineQuestions} from "tim/controllers/view/questions";
import {initReadings} from "tim/controllers/view/readings";
import {defineRefPopup} from "tim/controllers/view/refpopup";
import * as popupMenu from "tim/directives/popupMenu";
import {timLogTime} from "tim/timTiming";
import {isPageDirty, markAsUsed, markPageNotDirty} from "tim/utils";
import {ParCompiler} from "../../services/parCompiler";
import {$compile, $http, $window, $localStorage, $interval, $timeout, $filter} from "../../ngimport";
import {Users} from "../../services/userService";
import {Document, setActiveDocument} from "./document";
import {onClick} from "./eventhandlers";
import {initCssPrint} from "../../cssPrint";
import * as printctrl from "../printCtrl";

markAsUsed(ngStorage, popupMenu, interceptor, printctrl);

timApp.controller("ViewCtrl", [
    "$scope",
    function(sc) {
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
        sc.inLecture = $window.in_lecture;

        const document = new Document(sc.docId);
        setActiveDocument(document);

        if (sc.users.length > 0) {
            sc.selectedUser = sc.users[0];
        } else {
            sc.selectedUser = Users.getCurrent();
        }
        sc.hidePending = false;
        sc.pendingUpdates = {};

        sc.reload = function() {
            markPageNotDirty();
            $window.location.reload();
        };

        sc.closeRefreshDlg = function() {
            sc.showRefresh = false;
        };

        sc.changeUser = function(user, updateAll) {
            sc.selectedUser = user;
            sc.$broadcast("userChanged", {user, updateAll});
        };

        $($window).resize(function(e) {
            if (e.target === $window as any) {
                const newWidth = $($window).width();
                if (newWidth !== sc.oldWidth) {
                    sc.oldWidth = newWidth;
                    const selected = $(".par.lightselect, .par.selected");
                    if (selected.length > 0) {
                        selected[0].scrollIntoView();
                    }
                }
            }
        });

        sc.beginUpdate = async function() {
            const response = await $http.get<{changed_pars: any[]}>("/getUpdatedPars/" + sc.docId);
            sc.updatePendingPars(response.data.changed_pars);
        };

        sc.pendingUpdatesCount = function() {
            return Object.keys(sc.pendingUpdates).length;
        };

        sc.showUpdateDialog = function() {
            return !sc.hidePending && sc.pendingUpdatesCount() > 0;
        };

        sc.updatePendingPars = function(pars) {
            angular.extend(sc.pendingUpdates, pars);
            sc.hidePending = false;
            if (sc.pendingUpdatesCount() < 10) {
                sc.updatePending();
            }
        };

        sc.updatePending = function() {
            for (const key in sc.pendingUpdates) {
                if (sc.pendingUpdates.hasOwnProperty(key)) {
                    const $par = getElementByParId(key);
                    const $newPar = $($compile(sc.pendingUpdates[key])(sc));
                    $par.replaceWith($newPar);
                    sc.applyDynamicStyles($newPar);
                    ParCompiler.processAllMathDelayed($newPar);
                }
            }
            document.rebuildSections();
            sc.pendingUpdates = {};
            sc.processQuestions();
        };

        sc.showQuestions = function() {
            return (sc.item.rights.teacher && (sc.lectureMode || sc.inLecture)) ||
                ($window.editMode && sc.item.rights.editable);
        };

        sc.applyDynamicStyles = function($par) {
            if ($window.editMode) {
                $par.addClass("editmode");

                // Show hidden paragraphs if in edit mode
                $par.find(".mdcontent").css("display", "initial");
            }
        };

        sc.setHeaderLinks = function() {
            const pars = $(".parContent");
            pars.each(function() {
                const $p = $(this);
                $p.find("h1, h2, h3, h4, h5, h6").each(function() {
                    const $h = $(this);
                    const id = $h.attr("id");
                    if (angular.isDefined(id)) {
                        $h.append($("<a>", {
                            text: "#",
                            href: "#" + id,
                            class: "headerlink",
                            title: "Permanent link",
                        }));
                    }
                });
            });
        };

        sc.getEditMode = function() {
            return $window.editMode;
        };
        sc.getAllowMove = function() {
            return $window.allowMove;
        };

        sc.nothing = function() {
        };

        defineAreas(sc);
        defineClipboard(sc);
        defineEditing(sc);
        initIndex();
        defineNotes(sc);
        defineParMenu(sc);
        defineQuestions(sc);
        initReadings(sc);
        defineRefPopup(sc);
        initCssPrint();

        // Call necessary initialization functions below this line. Define any scope functions above this line.

        // from https://stackoverflow.com/a/7317311
        $(() => {
            sc.processQuestions();
            $window.addEventListener("beforeunload", function(e) {
                if (!sc.editing) {
                    return undefined;
                }

                const msg = "You are currently editing something. Are you sure you want to leave the page?";

                (e || $window.event).returnValue = msg; //Gecko + IE
                return msg; //Gecko + Webkit, Safari, Chrome etc.
            });
        });

        onClick("html.ng-scope", function($this, e) {
            // Clicking anywhere
            const tagName = e.target.tagName.toLowerCase();
            const jqTarget = $(e.target);
            const ignoreTags = ["button", "input", "label", "i"];
            const ignoreClasses = ["menu-icon", "editline", "areaeditline", "draghandle", "actionButtons"];

            let curElement = jqTarget;
            let limit = 10;
            while (curElement !== null) {
                if (sc.editing || $.inArray(tagName, ignoreTags) >= 0 || curElement.attr("position") === "absolute") {
                    return false;
                }

                for (let i = 0; i < ignoreClasses.length; i++) {
                    if (curElement.hasClass(ignoreClasses[i])) {
                        return false;
                    }
                }

                curElement = curElement.parent();
                if (--limit < 0) {
                    break;
                }
            }

            closeOptionsWindow();

            if (tagName !== "p") {
                $(".selected").removeClass("selected");
                $(".lightselect").removeClass("lightselect");
            }

            //$log.info(e.target);
            return false;

        }, true);

        sc.setHeaderLinks();
        document.rebuildSections();

        // If you add 'mousedown' to bind, scrolling upon opening the menu doesn't work on Android
        $("body,html").bind("scroll wheel DOMMouseScroll mousewheel", function(e) {
            if (e.which > 0 || e.type === "mousedown" || e.type === "mousewheel") {
                $("html,body").stop();
            }
        });

        sc.$watchGroup(["lectureMode", "selection.start", "selection.end", "editing", "getEditMode()",
            "allowPasteContent", "allowPasteRef", "getAllowMove()"], function(newValues, oldValues, scope) {
                sc.updatePopupMenu();
                if (sc.editing) {
                    sc.notification = "Editor is already open.";
                } else {
                    sc.notification = "";
                }
            });

        sc.$storage = $localStorage.$default({
            defaultAction: "Show options window",
            noteAccess: "everyone",
        });

        $window.allowMove = false;
        sc.oldWidth = $($window).width();
        sc.showRefresh = isPageDirty();
        sc.liveUpdates = $window.liveUpdates;

        if (Users.isLoggedIn() && sc.liveUpdates) {
            $interval(async () => {
                const response = await $http.get<{version: any, diff: any[]}>("/getParDiff/" + sc.docId + "/" + sc.docVersion[0] + "/" + sc.docVersion[1]);
                sc.docVersion = response.data.version;
                const replaceFn = async (d, parId) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    const e = getElementByParId(parId);
                    e.replaceWith(compiled);
                };
                const afterFn = async (d, parId) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    const e = getElementByParId(parId);
                    e.after(compiled);
                };
                const beforeFn = async (d, e) => {
                    const compiled = await ParCompiler.compile(d.content, sc);
                    e.before(compiled);
                };
                for (let i = 0; i < response.data.diff.length; ++i) {
                    const d = response.data.diff[i];
                    if (d.type === "delete") {
                        if (d.end_id !== null) {
                            getElementByParId(d.start_id).nextUntil(getElementByParId(d.end_id)).addBack().remove();
                        } else {
                            getElementByParId(d.start_id).nextAll(".par").addBack().remove();
                        }
                    } else if (d.type === "replace") {
                        const first = getElementByParId(d.start_id);
                        if (d.start_id !== d.end_id) {
                            if (d.end_id !== null) {
                                first.nextUntil(getElementByParId(d.end_id)).remove();
                            } else {
                                first.nextAll(".par").remove();
                            }
                        }
                        replaceFn(d, d.start_id);
                    } else if (d.type === "insert") {
                        if (d.after_id === null) {
                            beforeFn(d, $(".par:first"));
                        } else {
                            afterFn(d, d.after_id);
                        }
                    } else if (d.type === "change") {
                        replaceFn(d, d.id);
                    }
                }
                $timeout(function() {
                    document.rebuildSections();
                }, 1000);
            }, 1000 * sc.liveUpdates);
        }

        try {
            const found = $filter("filter")(sc.editorFunctions,
                {desc: sc.$storage.defaultAction}, true);
            if (found.length) {
                sc.defaultAction = found[0];
            }
        } catch (e) {
        }
        timLogTime("ViewCtrl end", "view");
    },
]);
