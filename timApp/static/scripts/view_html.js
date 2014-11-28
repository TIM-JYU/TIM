var timApp = angular.module('timApp', ['ngSanitize', 'angularFileUpload'].concat(modules));

timApp.controller("ViewCtrl", ['$scope',
    '$http',
    '$q',
    '$upload',
    '$injector',
    '$compile',
    function (sc, http, q, $upload, $injector, $compile) {
        sc.selectedPar = "";
        sc.pars = {};
        sc.docId = docId;

        sc.toggleNoteEditor = function ($par, options) {
            if ($par.children('.noteEditorArea').length) {
                $par.children().remove(".noteEditorArea");
            } else {
                var $div = $("<div>", {class: 'noteEditorArea'});
                $div.loadTemplate("/static/templates/noteEditor.html", {}, {
                    success: function () {
                        if (!options.showDelete) {
                            $div.find(".deleteButton").hide();
                        }
                        $par.append($div);
                    }
                });

            }
        };

        $(document).on("click", ".addNoteButton", function (e) {
            var $par = $(document.getElementById(sc.selectedPar));
            sc.toggleNoteEditor($par, {showDelete: false});
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

        sc.getNoteHtml = function (par) {
            var html = "";
            var notes = sc.pars[par].notes;
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
                html += $("<div>", {class: classes.join(" ")})
                    .attr('data-note_index', notes[i].note_index)
                    .html('<div class="noteContent">' + notes[i].content + '</div>')[0].outerHTML;
            }
            return html;
        };

        sc.forEachParagraph = function (func) {
            $('.paragraphs .par').each(func);
        };

        sc.getNotes = function () {
            var rn = "?rand=" + Math.floor(Math.random() * 100001);
            http.get('/notes/' + sc.docId + rn).success(function (data, status, headers, config) {
                var noteCount = data.length;

                for (var i = 0; i < noteCount; i++) {
                    var pi = data[i].par_index;
                    if (!(pi in sc.pars)) {
                        sc.pars[pi] = {notes: []};

                    }
                    if (!('notes' in sc.pars[pi])) {
                        sc.pars[pi].notes = [];
                    }
                    sc.pars[pi].notes.push(data[i]);
                }
                sc.forEachParagraph(function () {
                    if (this.id in sc.pars) {
                        var $notediv = $("<div>", {class: 'notes'}).html(sc.getNoteHtml(this.id));
                        $(this).append($notediv);
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, this.id]);
                    }
                });

            }).error(function (data, status, headers, config) {
                alert("Could not fetch notes.");
            });
        };

        sc.getReadPars = function () {
            var rn = "?rand=" + Math.floor(Math.random() * 100001);
            http.get('/read/' + sc.docId + rn).success(function (data, status, headers, config) {
                var readCount = data.length;

                for (var i = 0; i < readCount; i++) {
                    var readPar = data[i];
                    var pi = data[i].par_index;
                    if (!(pi in sc.pars)) {
                        sc.pars[pi] = {};
                    }
                    sc.pars[pi].readStatus = readPar.status;
                }
                sc.forEachParagraph(function () {
                    var classes = ["readline"];
                    if (this.id in sc.pars && 'readStatus' in sc.pars[this.id]) {
                        classes.push(sc.pars[this.id].readStatus);
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