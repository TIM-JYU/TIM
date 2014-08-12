var controls = angular.module('controller');

controls.directive('focusMe', function($timeout) {
    return {
        link : function(scope, element, attrs) {
            scope.$watch(attrs.focusMe, function(value) {
                if (value === true) {
                    console.log('value=', value);
                    // $timeout(function() {
                    element[0].focus();
                    element[0].select();
                    scope[attrs.focusMe] = false;
                    // });
                }
            });
        }
    };
});

controls.controller('NoteCtrl', function($scope, $controller, $http) {

    $scope.m = {};
    $scope.visibility = 'everyone';

    $scope.addNote = function(parIndex) {
        $http.post('/postNote', {
            "par_id" : parIndex,
            "doc_id" : $scope.docId,
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
        $scope.focusArea = true;
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
        $scope.focusArea = true;
        $scope.m.editingNote = -1;
    }

    $scope.deleteNote = function(docID) {
        console.log(docID);
        $http.post('/deleteNote', {
            "note_id" : $scope.m.editingNote
        }).success(function(data, status, headers, config) {
            // TODO: Maybe fetch notes only for this paragraph and not the
            // whole document.
            $scope.getNotes();
            $scope.m.showEditor = false;
        }).error(function(data, status, headers, config) {
            alert('Could not delete the note.');
        });
    }
});