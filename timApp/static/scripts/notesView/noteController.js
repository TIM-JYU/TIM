var controls = angular.module('controller');

controls.controller('NoteCtrl', function($scope, $controller, $http) {

    // $controller('ParCtrl', {
        // $scope : $scope
        // });

        $scope.m = {};

        $scope.addNote = function(parIndex, docID) {
            $http.post('/postNote', {
                "par_id" : parIndex,
                "doc_id" : $scope.docID,
                "text" : $scope.m.noteText,
                // test group
                "group_id" : 1

            }).success(function(data, status, headers, config) {
                // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    $scope.getNotes();
                }).error(function(data, status, headers, config) {
                alert('Could not save the note.');
            });
        };

        $scope.editNote = function() {
            $http.post('/editNote', {
                "note_id" : $scope.m.editingNote,
                "text" : $scope.m.noteText
            }).success(function(data, status, headers, config) {
                // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    $scope.getNotes();
                }).error(function(data, status, headers, config) {
                alert('Could not save the note.');
            });
        }

        $scope.editButtonToggled = function(note) {
            $scope.m.editingNote = note.id;
            $scope.m.noteText = note.content;
            $scope.m.showEditor = true;
        }
        
        $scope.saveButtonToggled = function() {
            if ($scope.m.editingNote !== -1) {
                $scope.editNote();
            } else {
                $scope.addNote($scope.parIndex);
            }
            $scope.m.editingNote = -1;
            $scope.m.showEditor = false;
        }

        $scope.addNoteButtonToggled = function() {
            $scope.m.showEditor = true;
            $scope.m.editingNote = -1;
        }

        $scope.deleteNote = function(noteID, docID) {
            $http.post('/deleteNote', {
                "note_id" : noteID

            }).success(function(data, status, headers, config) {
                // TODO: Maybe fetch notes only for this paragraph and not the
                    // whole document.
                    $scope.getNotes();
                }).error(function(data, status, headers, config) {
                alert('Could not delete the note.');
            });
        }
    });