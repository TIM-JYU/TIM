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

        sc.toggleNoteEditor = function ($par, options) {
            if ($par.children('.noteEditorArea').length) {
                $par.children().remove(".noteEditorArea");
            } else {
                var $div = $("<div>", {class: 'noteEditorArea'});
                $div.loadTemplate("/static/templates/noteEditor.html?_=" + (new Date).getTime(), {}, {
                    success: function () {
                        if (!options.showDelete) {
                            $div.find(".deleteButton").hide();
                        }
                        else {
                            var data = $par.data();
                            $div.find('.noteeditor').text(data.content);
                            $div.find('input[name=access][value=' + data.access + ']').prop('checked', true);
                            $div.find('input[name=difficult]').prop('checked', data.difficult);
                            $div.find('input[name=unclear]').prop('checked', data.unclear);
                        }
                        $par.append($div);
                    }
                });
            }
        };

        $(document).on("click", ".readline", function (e) {
            var par_id = $(this).parents('.par').attr('id');
            http.put('/read/' + sc.docId + '/' + par_id + '?_=' + (new Date).getTime())
                .success(function (data, status, headers, config) {
                    // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    sc.getReadPars();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
        });

        $(document).on("click", ".addNoteButton", function (e) {
            var $par = $(document.getElementById(sc.selectedPar));
            sc.toggleNoteEditor($par, {showDelete: false});
        });

        $(document).on("click", ".editButtonArea .cancelButton", function (e) {
            $(this).parent().parent().remove();
        });

        $(document).on("click", ".editButtonArea .deleteButton", function (e) {
            var noteElement = $(this).parents('.note');

            var fields = sc.getEditorFields(noteElement.parents('.par'),
                noteElement.children('.noteEditorArea'));
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
            return {
                par_id: parElement.attr('id'),
                content: noteEditorArea.find('.noteeditor').val(),
                access: noteEditorArea.find('input[name=access]:checked').val(),
                difficult: noteEditorArea.find('input[name=difficult]').is(':checked'),
                unclear: noteEditorArea.find('input[name=unclear]').is(':checked')
            };
        };

        $(document).on("click", ".editButtonArea .parSaveButton", function (e) {
            if ($(this).parents('.note').length) {
                //alert('edited');
                //return;
                var noteElement = $(this).parents('.note');

                var fields = sc.getEditorFields(noteElement.parents('.par'),
                    noteElement.children('.noteEditorArea'));
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
                    $(this).parents('.par').children('.noteEditorArea'));
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
                    $('.noteEditorArea').remove();
                    sc.getNotes();
                }).error(function (data, status, headers, config) {
                    alert('Could not save the note.');
                });
            }
        });

        $(document).on('click', '.paragraphs .parContent', function () {
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

        $(document).on('click', ".noteContent", function () {
            sc.toggleNoteEditor($(this).parent(), {showDelete: true});
        });

        sc.toggleActionbuttons = function (par, toggle) {
            var $par = $(document.getElementById(par));
            if (toggle) {
                $par.append('<div class="actionButtons"><button class="addNoteButton">Comment/note</button></div>');
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
                    .append($("<div>", {class: 'noteContent'}).html(notes[i].content)));
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
                //for (var key in sc.pars) {
                //    if (sc.pars.hasOwnProperty(key)) {
                //        delete sc.pars[key];
                //    }
                //}
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

        sc.getNotes();
        sc.getReadPars();
    }]);