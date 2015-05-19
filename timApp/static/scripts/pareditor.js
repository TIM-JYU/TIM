var angular;
var timApp = angular.module('timApp');

timApp.directive("pareditor", ['$upload', '$http', '$sce', '$compile', '$window',
    function ($upload, $http, $sce, $compile, $window) {
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
                editorText: '@',
                initialTextUrl: '@'
            },
            controller: function ($scope) {
                $scope.aceChanged = function () {
                    $scope.outofdate = true;
                    if ($scope.timer) {
                        $window.clearTimeout($scope.timer);
                    }
                    $scope.timer = $window.setTimeout(function () {
                        var text = $scope.editor.getSession().getValue();
                        $http.post($scope.previewUrl, {
                            "text": text
                        }).success(function (data, status, headers, config) {
                            var len = data.texts.length;

                            var $previewDiv = angular.element(".previewcontent");
                            $previewDiv.html("");

                            for (var i = 0; i < len; i++) {
                                $previewDiv.append(angular.element("<div>", {class: "par"})
                                    .append(angular.element("<div>", {class: "parContent"})
                                        .html($compile(data.texts[i].html)($scope))));
                            }
                            $scope.$parent.processAllMath($previewDiv);
                            $scope.outofdate = false;
                            $scope.parCount = len;
                        }).error(function (data, status, headers, config) {
                            $window.alert("Failed to show preview: " + data.error);
                        });
                    }, 500);
                };

                $scope.aceLoaded = function (editor) {
                    $scope.editor = editor;
                    editor.renderer.setPadding(10, 10, 10, 10);
                    editor.getSession().setMode("markdown");
                    editor.getSession().setUseWrapMode(false);
                    editor.getSession().setWrapLimitRange(0, 79);
                    editor.setOptions({maxLines: 40, minLines: 3});
                    if ($scope.initialTextUrl) {
                        editor.getSession().setValue('Loading text...');
                        $http.get($scope.initialTextUrl).
                            success(function (data, status, headers, config) {
                                $scope.editorText = data.text;
                            }).
                            error(function (data, status, headers, config) {
                                $window.alert('Failed to get text: ' + data.error);
                            });
                    }

                    var iOS = /(iPad|iPhone|iPod)/g.test($window.navigator.platform);

                    // iPad does not open the keyboard if not manually focused to editable area
                    if (!iOS) {
                        editor.focus();
                    }
                };
            },
            link: function ($scope, $element, $attrs) {
                $scope.timer = null;
                $scope.outofdate = false;
                $scope.parCount = 0;

                $scope.saveClicked = function () {
                    var text = $scope.editor.getSession().getValue();
                    $http.post($scope.saveUrl, angular.extend({
                        text: text
                    }, $scope.extraData)).success(function (data, status, headers, config) {
                        $scope.afterSave({
                            extraData: $scope.extraData,
                            saveData: data
                        });
                        if ($scope.options.destroyAfterSave) {
                            $element.remove();
                        }
                    }).error(function (data, status, headers, config) {
                        $window.alert("Failed to save: " + data.error);
                    });
                };

                $scope.deleteClicked = function () {
                    if (!$window.confirm("Delete - are you sure?")) {
                        return;
                    }
                    $http.post($scope.deleteUrl, $scope.extraData).
                        success(function (data, status, headers, config) {
                            $scope.afterDelete({
                                extraData: $scope.extraData,
                                saveData: data
                            });
                            if ($scope.options.destroyAfterSave) {
                                $element.remove();
                            }
                        }).
                        error(function (data, status, headers, config) {
                            $window.alert("Failed to delete: " + data.error);
                        });
                };

                $scope.cancelClicked = function () {
                    $element.remove(); 
                    $scope.afterCancel({
                        extraData: $scope.extraData
                    });
                };

                $scope.releaseClicked = function () {
                    var div = $("#previewDiv");
                    
                    if (div.css("position") == "absolute") {
                        div.css("position", "static");
                        div.find(".draghandle").css("visibility", "hidden");
                        document.getElementById("releaseButton").innerHTML = "&#8594;";
                    }
                    else {
                        div.css("position", "absolute");
                        div.find(".draghandle").css("visibility", "visible");
                        document.getElementById("releaseButton").innerHTML = "&#8592;";
                    }
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
