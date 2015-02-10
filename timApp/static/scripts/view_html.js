var timApp = angular.module('timApp', ['ngSanitize', 'angularFileUpload', 'ui.ace'].concat(modules), function($locationProvider){
    $locationProvider.html5Mode(true);
    $locationProvider.hashPrefix('!');
});

timApp.controller("ViewCtrl", ['$scope',
    '$http',
    '$q',
    '$upload',
    '$injector',
    '$compile',
    '$location',
    function (sc, http, q, $upload, $injector, $compile, $location) {
        http.defaults.headers.common.Version = version.hash;
        http.defaults.headers.common.RefererPath = refererPath;
        sc.docId = docId;
        sc.docName = docName;
        sc.canEdit = canEdit;
        sc.startIndex = startIndex;
        sc.noteClassAttributes = ["difficult", "unclear", "editable", "private"];
        sc.editing = false;
        var NOTE_EDITOR_CLASS = "editorArea";
        var NOTE_EDITOR_CLASS_DOT = "." + NOTE_EDITOR_CLASS;
        var NOTE_CANCEL_BUTTON = ".timButton.cancelNote";
        var NOTE_DELETE_BUTTON = ".timButton.deleteNote";
        var NOTE_SAVE_BUTTON = ".timButton.saveNote";
        var NOTE_ADD_BUTTON_CLASS = "timButton addNote";
        var NOTE_ADD_BUTTON = "." + NOTE_ADD_BUTTON_CLASS.replace(" ", ".");

        var EDITOR_CLASS = "editorArea";
        var EDITOR_CLASS_DOT = "." + EDITOR_CLASS;
        var PAR_CANCEL_BUTTON = ".timButton.cancelPar";
        var PAR_DELETE_BUTTON = ".timButton.deletePar";
        var PAR_SAVE_BUTTON = ".timButton.savePar";
        var PAR_ADD_BUTTON_CLASS = "timButton addPar";
        var PAR_ADD_BUTTON = "." + PAR_ADD_BUTTON_CLASS.replace(" ", ".");
        var PAR_EDIT_BUTTON_CLASS = "timButton editPar";
        var PAR_EDIT_BUTTON = "." + PAR_EDIT_BUTTON_CLASS.replace(" ", ".");

        sc.getParIndex = function ($par) {
            return $par.index() + sc.startIndex;
        };

        sc.getElementByParIndex = function (index) {
            return $("#pars").children().eq(index - sc.startIndex);
        };

        sc.getNoteEditorFields = function (parElement, editorArea) {
            var editorElem = editorArea.find('.editor')[0];
            return {
                par_id: sc.getParIndex(parElement),
                content: ace.edit(editorElem).getSession().getValue(),
                access: editorArea.find('input[name=access]:checked').val(),
                difficult: editorArea.find('input[name=difficult]').is(':checked'),
                unclear: editorArea.find('input[name=unclear]').is(':checked')
            };
        };

        sc.toggleParEditor = function ($par, options) {
            if ($par.children(EDITOR_CLASS_DOT).length) {
                $par.children().remove(EDITOR_CLASS_DOT);
                sc.editing = false;
            } else {
                $(EDITOR_CLASS_DOT).remove();
                var url;
                if ($par.hasClass("new")) {
                    url = '/newParagraph/';
                } else {
                    url = '/postParagraph/';
                }
                var par_id = sc.getParIndex($par);
                var attrs = {
                    "save-url": url,
                    "extra-data": JSON.stringify({
                        docId: sc.docId,
                        par: par_id
                    }),
                    "options": JSON.stringify({
                        showDelete: options.showDelete
                    }),
                    "after-save": 'addSavedParToDom(saveData, extraData)',
                    "after-cancel": 'handleCancel(extraData)',
                    "after-delete": 'handleDelete(saveData, extraData)',
                    "preview-url": '/preview/' + sc.docId,
                    "delete-url": '/deleteParagraph/' + sc.docId + "/" + par_id
                };

                var createEditor = function (attrs) {
                    var $div = $("<pareditor>", {class: EDITOR_CLASS}).attr(attrs);
                    $compile($div[0])(sc);
                    $par.append($div);
                    sc.editing = true;
                };

                if (options.showDelete){
                    $(".par.new").remove();
                    http.get('/getBlock/' + sc.docId + "/" + par_id).
                        success(function (data, status, headers, config) {
                            attrs["initial-text"] = data.md;
                            createEditor(attrs);
                        }).
                        error(function (data, status, headers, config) {
                            alert(data.error);
                        });
                } else {
                    createEditor(attrs);
                }
            }
        };

        sc.toggleNoteEditor = function ($par, options) {
            if ($par.children(NOTE_EDITOR_CLASS_DOT).length) {
                $par.children().remove(NOTE_EDITOR_CLASS_DOT);
                sc.editing = false;
            } else {
                $(".par.new").remove();
                $(EDITOR_CLASS_DOT).remove();
                var $div = $("<div>", {class: NOTE_EDITOR_CLASS});
                $div.loadTemplate("/static/templates/noteEditor.html?_=" + (new Date).getTime(), {}, {
                    success: function () {
                        if (options.isNew) {
                            $div.find(NOTE_DELETE_BUTTON).hide();
                            $div.data({});
                        }
                        else {
                            var data = options.noteData;
                            if (!data.editable) {
                                alert('You cannot edit this note.');
                                return;
                            }
                            $div.find('.editor').text(data.content);
                            $div.find('input:radio[name=access]').val([data.access]);
                            $div.find('input[name=difficult]').prop('checked', data.difficult);
                            $div.find('input[name=unclear]').prop('checked', data.unclear);
                        }
                        sc.applyAceEditor($div.find('.editor')[0]);
                        $div.data(data);
                        $par.append($div);
                        sc.editing = true;
                    }
                });
            }
        };

        sc.forEachParagraph = function (func) {
            $('.paragraphs .par').each(func);
        };

        sc.applyAceEditor = function (element) {
            var editor = new ace.edit(element);
            editor.setTheme("ace/theme/eclipse");
            editor.renderer.setPadding(10, 10, 10, 10);
            editor.getSession().setMode("ace/mode/markdown");
            editor.getSession().setUseWrapMode(false);
            editor.getSession().setWrapLimitRange(0, 79);
            editor.setOptions({maxLines: 40, minLines: 3});
            //$('.' + elem.par).get()[0].focus();
            var iOS = /(iPad|iPhone|iPod)/g.test(navigator.platform);
            
            // iPad does not open the keyboard if not manually focused to editable area
            if (!iOS)
            {
                editor.focus();
            }
            return editor;
        };

        // Event handlers

        var ua = navigator.userAgent,
            eventName = (ua.match(/iPad/i)) ? "touchstart" : "click";

        sc.addEvent = function (className, func) {
            $(document).on(eventName, className, func);
        };

        sc.addEvent(PAR_EDIT_BUTTON, function (e) {
            var $par = $(e.target).parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons($par, false, false, null);
            sc.toggleParEditor($par, {showDelete: true});
        });

        sc.addEvent(PAR_ADD_BUTTON, function (e) {
            var $par = $(e.target).parent().parent();
            $(".par.new").remove();
            sc.toggleActionButtons($par, false, false, null);
            var $newpar = $("<div>", {class: "par new"})
                .append($("<div>", {class: "parContent"}).html('New paragraph'));

            if ($(e.target).hasClass("above")) {
                $par.before($newpar);
            }
            else if ($(e.target).hasClass("below")) {
                $par.after($newpar);
            }

            sc.toggleParEditor($newpar, {showDelete: false});
        });

        sc.handleCancel = function (extraData) {
            var $par = sc.getElementByParIndex(extraData.par);
            if ($par.hasClass("new")) {
                $par.remove();
            }
            sc.editing = false;
        };

        sc.handleDelete = function (data, extraData) {
            var $par = sc.getElementByParIndex(extraData.par);
            http.defaults.headers.common.Version = data.version;
            $par.remove();
            sc.editing = false;
        };

        sc.addSavedParToDom = function (data, extraData) {
            var $par = sc.getElementByParIndex(extraData.par);
            http.defaults.headers.common.Version = data.version;
            var len = data.texts.length;
            for (var i = len - 1; i >= 0; i--) {
                var $newpar = $("<div>", {class: "par"});
                $par.after($newpar
                    .append($("<div>", {class: "parContent"}).html($compile(data.texts[i])(sc))));
                MathJax.Hub.Queue(["Typeset", MathJax.Hub, $newpar[0]]);
            }
            $par.remove();
            sc.editing = false;
        };

        sc.addEvent(".readline", function (e) {
            var par_id = sc.getParIndex($(this).parents('.par'));
            $(this).hide();
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + (new Date).getTime())
                .success(function (data, status, headers, config) {
                    // No need to do anything here
                }).error(function (data, status, headers, config) {
                    alert('Could not save the read marking.');
                });
        });

        sc.addEvent(NOTE_ADD_BUTTON, function (e) {
            var $par = $(e.target).parent().parent();
            sc.toggleActionButtons($par, false, false, null);
            sc.toggleNoteEditor($par, {isNew: true});
        });

        sc.addEvent(NOTE_CANCEL_BUTTON, function (e) {
            $(this).parent().parent().remove();
            sc.editing = false;
        });

        sc.addEvent(NOTE_DELETE_BUTTON, function (e) {
            var noteElement = $(this).parents('.note');
            var $par = $(this).parents('.par');
            var par_id = sc.getParIndex($par);
            var $editor = $par.find(NOTE_EDITOR_CLASS_DOT);
            var data = $editor.data();
            http.post('/deleteNote', {
                par_id: par_id,
                doc_id: sc.docId,
                note_index: data.note_index
            }).success(function (data, status, headers, config) {
                // TODO: Maybe fetch notes only for this paragraph and not the
                // whole document.
                sc.getNotes();
                $editor.remove();
                sc.editing = false;
            }).error(function (data, status, headers, config) {
                alert('Could not save the note.');
            });
        });

        sc.addEvent(NOTE_SAVE_BUTTON, function (e) {
            sc.saveNote($(this).parent().parent().parent(), $(this).parent().parent().data());
        });

        sc.addEvent('.paragraphs .parContent', function (e) {
            if (sc.editing)
                return;
            
            var tag = $(e.target).prop("tagName");

            // Don't show paragraph menu on these specific tags
            if (tag === 'BUTTON' || tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'A')
                return;

            var $par = $(this).parent();
            var coords = { left: e.pageX - $par.offset().left, top: e.pageY - $par.offset().top };
            var toggle1 = $par.find(".actionButtons").length === 0;
            var toggle2 = $par.hasClass("lightselect");

            $(".par.selected").removeClass("selected");
            $(".par.lightselect").removeClass("lightselect");
            $(".actionButtons").remove();
            sc.toggleActionButtons($par, toggle1, toggle2, coords);
        });

        sc.addEvent(".noteContent", function () {
            sc.toggleNoteEditor($(this).parent().parent().parent(), {isNew: false, noteData: $(this).parent().data()});
        });


        // Note-related functions

        sc.toggleActionButtons = function ($par, toggle1, toggle2, coords) {
            if (toggle2) {
                // Clicked twice successively
                $par.addClass("selected");
                var $actionDiv = $("<div>", {class: 'actionButtons'});
                var button_width = $par.outerWidth() / 4;
                $actionDiv.append($("<button>", {class: NOTE_ADD_BUTTON_CLASS, text: 'Comment/note', width: button_width}));
                if (sc.canEdit) {
                    $actionDiv.append($("<button>", {class: PAR_EDIT_BUTTON_CLASS, text: 'Edit', width: button_width}));
                    $actionDiv.append($("<button>", {
                        class: PAR_ADD_BUTTON_CLASS + ' above',
                        text: 'Add paragraph above',
                        width: button_width
                    }));
                    $actionDiv.append($("<button>", {
                        class: PAR_ADD_BUTTON_CLASS + ' below',
                        text: 'Add paragraph below',
                        width: button_width
                    }));
                }
                $actionDiv.offset(coords);
                $actionDiv.css('position', 'absolute'); // IE needs this
                $par.prepend($actionDiv);
            } else if (toggle1) {
                // Clicked once
                $par.addClass("lightselect");
            } else {
                $par.children().remove(".actionButtons");
                $par.removeClass("selected");
                $par.removeClass("lightselect");
            }
        };

        sc.getNoteHtml = function (notes) {
            var $noteDiv = $("<div>", {class: 'notes'});
            for (var i = 0; i < notes.length; i++) {
                var classes = ["note"];
                for (var j = 0; j < sc.noteClassAttributes.length; j++) {
                    if (notes[i][sc.noteClassAttributes[j]]) {
                        classes.push(sc.noteClassAttributes[j]);
                    }
                }
                $noteDiv.append($("<div>", {class: classes.join(" ")})
                    .data(notes[i])
                    .append($("<div>", {class: 'noteContent', html: notes[i].content})));
            }
            return $noteDiv;
        };

        sc.saveNote = function ($par, data) {
            var fields = sc.getNoteEditorFields($par, $par.find(NOTE_EDITOR_CLASS_DOT));
            if (!$.isEmptyObject(data)) {
                // save edits to existing note
                http.post('/editNote', {
                    par_id: fields.par_id,
                    doc_id: sc.docId,
                    text: fields.content,
                    access: fields.access,
                    difficult: fields.difficult,
                    unclear: fields.unclear,
                    note_index: data.note_index
                }).success(function (data, status, headers, config) {
                    // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            } else {
                http.post('/postNote', {
                    par_id: fields.par_id,
                    doc_id: sc.docId,
                    text: fields.content,
                    visibility: fields.access,
                    difficult: fields.difficult,
                    unclear: fields.unclear
                }).success(function (data, status, headers, config) {
                    // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    $(NOTE_EDITOR_CLASS_DOT).remove();
                    sc.editing = false;
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            }
            var $editor = $par.find(NOTE_EDITOR_CLASS_DOT);
            $editor.remove();
        };

        sc.getNotes = function () {
            var rn = "?_=" + (new Date).getTime();

            http.get('/notes/' + sc.docId + rn).success(function (data, status, headers, config) {
                $('.notes').remove();
                var pars = {};

                var noteCount = data.length;
                for (var i = 0; i < noteCount; i++) {
                    var pi = data[i].par_index;
                    if (!(pi in pars)) {
                        pars[pi] = {notes: []};

                    }
                    if (!('notes' in pars[pi])) {
                        pars[pi].notes = [];
                    }
                    pars[pi].notes.push(data[i]);
                }
                sc.forEachParagraph(function (index, elem) {
                    var parIndex = index + sc.startIndex;
                    if (parIndex in pars) {
                        var $notediv = sc.getNoteHtml(pars[parIndex].notes);
                        $(this).append($notediv);
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, "pars"]); // TODO queue only the paragraph
                    }
                });

            }).error(function (data, status, headers, config) {
                alert("Could not fetch notes.");
            });
        };

        sc.getReadPars = function () {
            var rn = "?_=" + (new Date).getTime();
            http.get('/read/' + sc.docId + rn).success(function (data, status, headers, config) {
                var readCount = data.length;
                $('.readline').remove();
                var pars = {};
                for (var i = 0; i < readCount; i++) {
                    var readPar = data[i];
                    var pi = data[i].par_index;
                    if (!(pi in pars)) {
                        pars[pi] = {};
                    }
                    pars[pi].readStatus = readPar.status;
                }
                sc.forEachParagraph(function (index, elem) {
                    var parIndex = index + sc.startIndex;
                    var classes = ["readline"];
                    if (parIndex in pars && 'readStatus' in pars[parIndex]) {
                        classes.push(pars[parIndex].readStatus);
                    } else {
                        classes.push("unread");
                    }
                    var $div = $("<div>", {class: classes.join(" "), title: "Click to mark this paragraph as read"});
                    $(this).append($div);
                })
            }).error(function (data, status, headers, config) {
                alert("Could not fetch reading info.");
            });
        };

        sc.setHeaderLinks = function () {
            $(".par h1, .par h2, .par h3, .par h4, .par h5, .par h6").each(function () {
                var $par = $(this).parent();
                $par.append($("<a>", {
                    text: '#',
                    href: '#' + $(this).attr('id'),
                    class: 'headerlink',
                    title: 'Permanent link'
                }));
            });
        };

        // Index-related functions

        sc.totext = function (str) {
            if (str.indexOf('{') > 0) {
                return str.substring(0, str.indexOf('{')).trim();
            }
            return str;
        };

        sc.tolink = function (str) {
            if (str.indexOf('{') >= 0 && str.indexOf('}') > 0) {
                var ob = str.indexOf('{');
                var cb = str.indexOf('}');
                return str.substring(ob + 1, cb);
            }
            return "#" + str.replace(/^(\d)+(\.\d+)*\.? /, "").replace(/[^\d\wåäö\.\- ]/g, "").trim().replace(/ +/g, '-').toLowerCase()
        };

        sc.findIndexLevel = function (str) {
            for (var i = 0; i < str.length; i++) {
                if (str.charAt(i) != '#') {
                    return i;
                }
            }

            return 0;
        };

        sc.getIndex = function () {
            http.get('/index/' + sc.docId).success(function (data, status, headers, config) {
                var parentEntry = null;
                sc.indexTable = [];

                for (var i = 0; i < data.length; i++) {
                    var lvl = sc.findIndexLevel(data[i]);
                    if (lvl < 1 || lvl > 3)
                        continue;

                    var astyle = "a" + lvl;
                    var txt = data[i].substr(lvl);
                    txt = txt.trim().replace(/\\#/g, "#");
                    var entry = {
                        text: sc.totext(txt),
                        target: encodeURIComponent($location.path().substring(1)) + sc.tolink(txt),
                        style: astyle,
                        level: lvl,
                        items: [],
                        state: ""
                    };

                    if (lvl == 1) {
                        if (parentEntry != null) {
                            if ("items" in parentEntry && parentEntry.items.length > 0)
                                parentEntry.state = 'col';
                            sc.indexTable.push(parentEntry);
                        }

                        parentEntry = entry;
                    }
                    else if (parentEntry != null) {
                        if (!("items" in parentEntry)) {
                            // For IE
                            parentEntry.items = []
                        }
                        parentEntry.items.push(entry)
                    }
                }

                if (parentEntry != null) {
                    if (parentEntry.items.length > 0)
                        parentEntry.state = 'col';
                    sc.indexTable.push(parentEntry);
                }

                //sc.$apply();
            }).error(function (data, status, headers, config) {
                alert("Could not fetch index entries.");
            });
        };

        sc.invertState = function (state) {
            if (state == 'exp')
                return 'col';
            if (state == 'col')
                return 'exp';
            return state;
        };

        sc.clearSelection = function () {
            if (document.selection)
                document.selection.empty();
            else if (window.getSelection)
                window.getSelection().removeAllRanges();
        };

        sc.invertStateClearSelection = function (event, state) {
            if (event.which != 1) {
                // Listen only to the left mouse button
                return state;
            }
            if (event.target.className == 'a2' || event.target.className == 'a3') {
                // Do not collapse/expand if a subentry is clicked
                return state;
            }

            var newState = sc.invertState(state);
            if (newState != state)
                sc.clearSelection();
            return newState;
        };

        // Load index, notes and read markings
        sc.setHeaderLinks();
        sc.indexTable = [];
        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();
    }]);
