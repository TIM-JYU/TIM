var timApp = angular.module('timApp');

timApp.directive("pareditor", ['$upload', '$http', '$sce', '$compile', function ($upload, $http, $sce, $compile) {
    return {
        templateUrl: "/static/templates/parEditor.html",
        restrict: 'E',
        scope: {
            saveUrl: '@',
            deleteUrl: '@',
            previewUrl: '@',
            extraData: '=',
            afterSave: '&',
            afterCancel: '&',
            afterDelete: '&',
            options: '=',
            initialText: '@'
        },
        controller: function($scope) {
            $scope.aceChanged = function () {
                $scope.outofdate = true;
                if ($scope.timer) {
                    clearTimeout($scope.timer);
                }
                $scope.timer = setTimeout(function () {
                    var text = $scope.editor.getSession().getValue();
                    $http.post($scope.previewUrl, {
                        "text": text
                    }).success(function (data, status, headers, config) {
                        var len = data.texts.length;

                        // TODO: More Angular, less jQuery
                        var $previewDiv = $(".previewcontent");
                        $previewDiv.html("");
                        for (var i = 0; i < len; i++) {
                            $previewDiv
                                .append($("<div>", {class: "par"})
                                    .append($("<div>", {class: "parContent"})
                                        .html($compile(data.texts[i])($scope))));
                        }
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, $previewDiv[0]]);
                        $scope.outofdate = false;
                        $scope.parCount = len;
                    }).error(function (data, status, headers, config) {
                        alert("Failed to show preview: " + data.error);
                    });
                }, 500);
            };

            $scope.aceLoaded = function (editor) {
                $scope.editor = editor;
                editor.renderer.setPadding(10, 10, 10, 10);
                editor.getSession().setMode("ace/mode/markdown");
                editor.getSession().setUseWrapMode(false);
                editor.getSession().setWrapLimitRange(0, 79);
                editor.setOptions({maxLines: 40, minLines: 3});
                editor.getSession().setValue($scope.initialText);
                var iOS = /(iPad|iPhone|iPod)/g.test(navigator.platform);

                // iPad does not open the keyboard if not manually focused to editable area
                if (!iOS) {
                    editor.focus();
                }
            };
        },
        link: function ($scope, $element, $attrs) {
            $scope.timer = null;
            $scope.outofdate = false;

            $scope.saveClicked = function () {
                var text = $scope.editor.getSession().getValue();
                $http.post($scope.saveUrl, angular.extend({
                    text: text
                }, $scope.extraData)).success(function (data, status, headers, config) {
                    $scope.afterSave({
                        extraData: $scope.extraData,
                        saveData: data
                    });
                }).error(function (data, status, headers, config) {
                    alert("Failed to save paragraph: " + data.error);
                });
            };

            $scope.deleteClicked = function () {
                $http.get($scope.deleteUrl).
                    success(function (data, status, headers, config) {
                        $scope.afterDelete({
                            extraData: $scope.extraData,
                            saveData: data
                        });
                    }).
                    error(function (data, status, headers, config) {
                        alert("Failed to remove paragraph: " + data.error);
                    });
            };

            $scope.cancelClicked = function () {
                $element.remove();
                $scope.afterCancel({
                    extraData: $scope.extraData
                });
            };

            $scope.onFileSelect = function (url, $files) {
                //$files: an array of files selected, each file has name, size, and type.
                for (var i = 0; i < $files.length; i++) {
                    var file = $files[i];
                    $scope.upload = $upload.upload({
                        url: url,
                        method: 'POST',
                        file: file
                    }).progress(function (evt) {
                        $scope.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                    }).success(function (data, status, headers, config) {
                        $scope.uploadedFile = '/images/' + data.file;
                        $scope.progress = 'Uploading... Done!';
                        $scope.editor.insert("![Image](" + $scope.uploadedFile + ")");
                    }).error(function (data, status, headers, config) {
                        $scope.progress = 'Error while uploading: ' + data.error;
                    });
                }
            };
        }
    };
}]);
