var timApp = angular.module('timApp', ['ngSanitize', 'angularFileUpload'].concat(modules));

timApp.controller("ViewCtrl", ['$scope',
    '$http',
    '$q',
    '$upload',
    '$injector',
    '$compile',
    function (sc, http, q, $upload, $injector, $compile) {
        http.defaults.headers.common.Version = version.hash;
        sc.docId = docId;
        sc.docName = docName;
        sc.canEdit = canEdit;
        sc.startIndex = startIndex;
        sc.noteClassAttributes = ["difficult", "unclear", "editable", "private"];
        var NOTE_EDITOR_CLASS = "noteEditorArea";
        var NOTE_EDITOR_CLASS_DOT = "." + NOTE_EDITOR_CLASS;
        var NOTE_CANCEL_BUTTON = ".timButton.cancelNote";
        var NOTE_DELETE_BUTTON = ".timButton.deleteNote";
        var NOTE_SAVE_BUTTON = ".timButton.saveNote";
        var NOTE_ADD_BUTTON_CLASS = "timButton addNote";
        var NOTE_ADD_BUTTON = "." + NOTE_ADD_BUTTON_CLASS.replace(" ", ".");

        var PAR_EDITOR_CLASS = "editorArea";
        var PAR_EDITOR_CLASS_DOT = "." + PAR_EDITOR_CLASS;
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

        sc.toggleParEditor = function ($par, options) {
            if ($par.children(PAR_EDITOR_CLASS_DOT).length) {
                $par.children().remove(PAR_EDITOR_CLASS_DOT);
            } else {
                var $div = $("<div>", {class: PAR_EDITOR_CLASS});
                $div.loadTemplate("/static/templates/parEditor.html?_=" + (new Date).getTime(), {}, {
                    success: function () {
                        var editorScope = sc.$new();
                        var aceEdit = sc.applyAceEditor($div.find('.pareditor')[0]);
                        editorScope.onFileSelect = function (url, $files) {
                            //$files: an array of files selected, each file has name, size, and type.
                            for (var i = 0; i < $files.length; i++) {
                                var file = $files[i];
                                editorScope.upload = $upload.upload({
                                    url: url,
                                    method: 'POST',
                                    file: file
                                }).progress(function (evt) {
                                    editorScope.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                                }).success(function (data, status, headers, config) {
                                    editorScope.uploadedFile = '/images/' + data.file;
                                    editorScope.progress = 'Uploading... Done!';
                                    aceEdit.insert("![Image](" + editorScope.uploadedFile + ")");
                                }).error(function (data, status, headers, config) {
                                    editorScope.progress = 'Error while uploading: ' + data.error;
                                });
                            }
                        };

                        if (!options.showDelete) {
                            $div.find(PAR_DELETE_BUTTON).hide();
                        }
                        else {
                            aceEdit.getSession().setValue("Loading paragraph text...");
                            http.get('/getBlock/' + sc.docId + "/" + sc.getParIndex($par)).
                                success(function (data, status, headers, config) {
                                    aceEdit.getSession().setValue(data.md);
                                }).
                                error(function (data, status, headers, config) {
                                    alert(data.error);
                                });
                        }
                        $compile($div[0])(editorScope);
                        $par.append($div);
                    }
                });
            }
        };

        sc.getNoteEditorFields = function (parElement, noteEditorArea) {
            var editorElem = noteEditorArea.find('.noteeditor')[0];
            return {
                par_id: sc.getParIndex(parElement),
                content: ace.edit(editorElem).getSession().getValue(),
                access: noteEditorArea.find('input[name=access]:checked').val(),
                difficult: noteEditorArea.find('input[name=difficult]').is(':checked'),
                unclear: noteEditorArea.find('input[name=unclear]').is(':checked')
            };
        };

        sc.toggleNoteEditor = function ($par, options) {
            if ($par.children(NOTE_EDITOR_CLASS_DOT).length) {
                $par.children().remove(NOTE_EDITOR_CLASS_DOT);
            } else {
                $(".par.new").remove();
                var $div = $("<div>", {class: NOTE_EDITOR_CLASS});
                $div.loadTemplate("/static/templates/noteEditor.html?_=" + (new Date).getTime(), {}, {
                    success: function () {
                        if (!options.showDelete) {
                            $div.find(NOTE_DELETE_BUTTON).hide();
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
                        sc.applyAceEditor($div.find('.noteeditor')[0]);
                        $par.append($div);
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
            editor.focus();
            return editor;
        };

        // Event handlers

        var ua = navigator.userAgent,
            eventName = (ua.match(/iPad/i)) ? "touchstart" : "click";

        sc.addEvent = function (className, func) {
            $(document).on(eventName, className, func);
        };

        sc.addEvent(PAR_EDIT_BUTTON, function (e) {
            $(".par.new").remove();
            sc.toggleParEditor($(e.target).parent().parent(), {showDelete: true});
        });

        sc.addEvent(PAR_ADD_BUTTON, function (e) {
            var $par = $(e.target).parent().parent();
            $(".par.new").remove();
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

        sc.addEvent(PAR_CANCEL_BUTTON, function (e) {
            var $par = $(this).parent().parent().parent();
            if ($par.hasClass("new")) {
                $par.remove();
            }
            else {
                $(this).parent().parent().remove(); // remove editor only
            }
        });

        sc.addEvent(PAR_DELETE_BUTTON, function (e) {
            var $par = $(this).parents('.par');
            var par_id = sc.getParIndex($par);

            http.get('/deleteParagraph/' + sc.docId + "/" + par_id).
                success(function (data, status, headers, config) {
                    http.defaults.headers.common.Version = data.version;
                    $par.remove();
                }).
                error(function (data, status, headers, config) {
                    alert("Failed to remove paragraph: " + data.error);
                });
        });

        sc.addEvent(PAR_SAVE_BUTTON, function (e) {
            var $par = $(this).parents('.par');
            var par_id = sc.getParIndex($par);
            var editorElem = $par.find('.pareditor')[0];
            var text = ace.edit(editorElem).getSession().getValue();
            if ($par.hasClass("new")) {
                var url = '/newParagraph/';
            } else {
                var url = '/postParagraph/';
            }
            http.post(url, {
                "docId": sc.docId,
                "par": par_id,
                "text": text
            }).success(function (data, status, headers, config) {
                http.defaults.headers.common.Version = data.version;
                var len = data.texts.length;
                for (var i = len - 1; i >= 0; i--) {
                    $par.after($("<div>", {class: "par"})
                        .append($("<div>", {class: "parContent"}).html($compile(data.texts[i])(sc))));
                }
                $par.remove();
            }).error(function (data, status, headers, config) {
                alert("Failed to save paragraph: " + data.error);
            });
        });

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
            sc.toggleNoteEditor($par, {showDelete: false});
        });

        sc.addEvent(NOTE_CANCEL_BUTTON, function (e) {
            $(this).parent().parent().remove();
        });

        sc.addEvent(NOTE_DELETE_BUTTON, function (e) {
            var noteElement = $(this).parents('.note');

            var fields = sc.getNoteEditorFields(noteElement.parents('.par'),
                noteElement.children(NOTE_EDITOR_CLASS_DOT));
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

        sc.addEvent(NOTE_SAVE_BUTTON, function (e) {
            sc.saveNote($(this));
        });

        sc.addEvent('.paragraphs .parContent', function () {
            var $par = $(this).parent();
            var toggle = $par.find(".actionButtons").length === 0;
            $(".par.selected").removeClass("selected");
            $(".actionButtons").remove();
            sc.toggleActionbuttons($par, toggle);
        });

        sc.addEvent(".noteContent", function () {
            sc.toggleNoteEditor($(this).parent(), {showDelete: true});
        });


        // Note-related functions

        sc.toggleActionbuttons = function ($par, toggle) {
            if (toggle) {
                $par.addClass("selected");
                var $actionDiv = $("<div>", {class: 'actionButtons'});
                $actionDiv.append($("<button>", {class: NOTE_ADD_BUTTON_CLASS, text: 'Comment/note'}));
                if (sc.canEdit) {
                    $actionDiv.append($("<button>", {class: PAR_EDIT_BUTTON_CLASS, text: 'Edit'}));
                    $actionDiv.append($("<button>", {
                        class: PAR_ADD_BUTTON_CLASS + ' above',
                        text: 'Add paragraph above'
                    }));
                    $actionDiv.append($("<button>", {
                        class: PAR_ADD_BUTTON_CLASS + ' below',
                        text: 'Add paragraph below'
                    }));
                }
                $par.prepend($actionDiv);
            } else {
                $par.children().remove(".actionButtons");
                $par.removeClass("selected");
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

        sc.saveNote = function ($par) {
            if ($par.parents('.note').length) {
                var noteElement = $par.parents('.note');
                var fields = sc.getNoteEditorFields(noteElement.parents('.par'),
                    noteElement.children(NOTE_EDITOR_CLASS_DOT));
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
                var fields = sc.getNoteEditorFields($par.parents('.par'),
                    $par.parents('.par').children(NOTE_EDITOR_CLASS_DOT));
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
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            }
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
                    class: 'headerlink'
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

        // Load index, notes and read markings
        sc.setHeaderLinks();
        sc.indexTable = [];
        sc.getIndex();
        sc.getNotes();
        sc.getReadPars();
    }]);