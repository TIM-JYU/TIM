var timApp = angular.module('timApp', ['ngSanitize', 'angularFileUpload'].concat(modules));

timApp.controller("ViewCtrl", ['$scope',
    '$http',
    '$q',
    '$upload',
    '$injector',
    '$compile',
    function (sc, http, q, $upload, $injector, $compile) {
        http.defaults.headers.common.Version = version.hash;
        sc.selectedPar = "";
        sc.docId = docId;
        sc.docName = docName;
        sc.canEdit = canEdit;
        var EDITOR_CLASS = "noteEditorArea";
        var EDITOR_CLASS_DOT = "." + EDITOR_CLASS;

        sc.toggleNoteEditor = function ($par, options) {
            if ($par.children(EDITOR_CLASS_DOT).length) {
                $par.children().remove(EDITOR_CLASS_DOT);
            } else {
                var $div = $("<div>", {class: EDITOR_CLASS});
                $div.loadTemplate("/static/templates/noteEditor.html?_=" + (new Date).getTime(), {}, {
                    success: function () {
                        if (!options.showDelete) {
                            $div.find(".deleteButton").hide();
                        }
                        else {
                            var data = $par.data();
                            if (!data.editable) {
                                alert('You cannot edit this note.');
                                return;
                            }
                            $div.find('.noteeditor').text(data.content);
                            $div.find('input[name=access][value=' + data.access + ']').prop('checked', true);
                            $div.find('input[name=difficult]').prop('checked', data.difficult);
                            $div.find('input[name=unclear]').prop('checked', data.unclear);
                        }

                        var editor = new ace.edit($div.find('.noteeditor')[0]);
                        editor.setTheme("ace/theme/eclipse");
                        editor.renderer.setPadding(10, 10, 10, 10);
                        editor.getSession().setMode("ace/mode/markdown");
                        editor.getSession().setUseWrapMode(false);
                        editor.getSession().setWrapLimitRange(0, 79);
                        editor.setOptions({maxLines: 40, minLines: 3});
                        //$('.' + elem.par).get()[0].focus();
                        editor.focus();
                        $par.append($div);
                    }
                });
            }
        };

        var ua = navigator.userAgent,
            eventName = (ua.match(/iPad/i)) ? "touchstart" : "click";

        $(document).on(eventName, ".readline", function (e) {
            var par_id = $(this).parents('.par').attr('id');
            $(this).hide();
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + (new Date).getTime())
                .success(function (data, status, headers, config) {
                    // No need to do anything here
                }).error(function (data, status, headers, config) {
                    alert('Could not save the read marking.');
                });
        });

        $(document).on(eventName, ".addNoteButton", function (e) {
            var $par = $(document.getElementById(sc.selectedPar));
            sc.toggleNoteEditor($par, {showDelete: false});
        });

        $(document).on(eventName, ".editButtonArea .cancelButton", function (e) {
            $(this).parent().parent().remove();
        });

        $(document).on(eventName, ".editButtonArea .deleteButton", function (e) {
            var noteElement = $(this).parents('.note');

            var fields = sc.getEditorFields(noteElement.parents('.par'),
                noteElement.children(EDITOR_CLASS_DOT));
            http.post('/deleteNote', {
                par_id: fields.par_id,
                doc_id: sc.docId,
                note_index: noteElement.data().note_index
            }).success(function (data, status, headers, config) {
                // TODO: Maybe fetch notes only for this paragraph and not the
                // whole document.
                sc.getNotes();
            }).error(function (data, status, headers, config) {
                alert('Could not save the note.');
            });
        });

        sc.getEditorFields = function (parElement, noteEditorArea) {
            var editorElem = noteEditorArea.find('.noteeditor')[0];
            return {
                par_id: parElement.attr('id'),
                content: ace.edit(editorElem).getSession().getValue(),
                access: noteEditorArea.find('input[name=access]:checked').val(),
                difficult: noteEditorArea.find('input[name=difficult]').is(':checked'),
                unclear: noteEditorArea.find('input[name=unclear]').is(':checked')
            };
        };

        $(document).on(eventName, ".editButtonArea .parSaveButton", function (e) {
            if ($(this).parents('.note').length) {
                var noteElement = $(this).parents('.note');

                var fields = sc.getEditorFields(noteElement.parents('.par'),
                    noteElement.children(EDITOR_CLASS_DOT));
                // save edits to existing note
                http.post('/editNote', {
                    par_id: fields.par_id,
                    doc_id: sc.docId,
                    text: fields.content,
                    access: fields.access,
                    difficult: fields.difficult,
                    unclear: fields.unclear,
                    note_index: noteElement.data().note_index
                }).success(function (data, status, headers, config) {
                    // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            } else {
                var fields = sc.getEditorFields($(this).parents('.par'),
                    $(this).parents('.par').children(EDITOR_CLASS_DOT));
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
                    $(EDITOR_CLASS_DOT).remove();
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            }
        });

        $(document).on(eventName, '.paragraphs .parContent', function () {
            var id = $(this).parent().attr('id');
            if (sc.selectedPar !== "") {
                sc.toggleActionbuttons(sc.selectedPar, false);
            }
            if (sc.selectedPar !== id) {
                sc.selectedPar = id;
                sc.toggleActionbuttons(sc.selectedPar, true);
            } else {
                sc.selectedPar = "";
            }
        });

        $(document).on(eventName, ".noteContent", function () {
            sc.toggleNoteEditor($(this).parent(), {showDelete: true});
        });

        sc.toggleActionbuttons = function (par, toggle) {
            var $par = $(document.getElementById(par));
            if (toggle) {
                var $actionDiv = $("<div>", {class: 'actionButtons'});
                $actionDiv.append($("<button>", {class: 'addNoteButton', text: 'Comment/note'}));
                $par.append($actionDiv);
            } else {
                $par.children().remove(".actionButtons");
            }
        };

        sc.getNoteHtml = function (notes) {
            var $noteDiv = $("<div>", {class: 'notes'});
            for (var i = 0; i < notes.length; i++) {
                var classes = ["note"];
                if (notes[i].difficult) {
                    classes.push("difficult")
                }
                if (notes[i].unclear) {
                    classes.push("unclear")
                }
                if (notes[i].private) {
                    classes.push("private")
                }
                $noteDiv.append($("<div>", {class: classes.join(" ")})
                    .data(notes[i])
                    .append($("<div>", {class: 'noteContent', html: notes[i].content})));
            }
            return $noteDiv;
        };

        sc.forEachParagraph = function (func) {
            $('.paragraphs .par').each(func);
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
                sc.forEachParagraph(function () {
                    if (this.id in pars) {
                        var $notediv = sc.getNoteHtml(pars[this.id].notes);
                        $(this).append($notediv);
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, this.id]);
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
                sc.forEachParagraph(function () {
                    var classes = ["readline"];
                    if (this.id in pars && 'readStatus' in pars[this.id]) {
                        classes.push(pars[this.id].readStatus);
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

        sc.totext = function (str) {
            if (str.indexOf('{') > 0) {
                return str.substring(0, str.indexOf('{')).trim();
            }
            return str;
        };

        sc.tolink = function (str) {
            if (str.indexOf('{') >= 0 && str.indexOf('}') > 0) {
                ob = str.indexOf('{')
                cb = str.indexOf('}')
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
                    lvl = sc.findIndexLevel(data[i]);
                    if (lvl < 1 || lvl > 3)
                        continue;

                    astyle = "a" + lvl;
                    txt = data[i].substr(lvl);
                    txt = txt.trim().replace(/\\#/g, "#")
                    entry = {
                        text: sc.totext(txt),
                        target: sc.tolink(txt),
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

            newState = sc.invertState(state);
            if (newState != state)
                sc.clearSelection();
            return newState;
        };

        sc.indexTable = [];

        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();
    }]);