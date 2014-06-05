
       
        timApp = angular.module('timApp', ['ngSanitize']);
        timApp.controller("TimCtrl", ['$scope', function(sc){
            sc.paragraphs = jsonData;
            sc.convertHtml = new Markdown.getSanitizingConverter();
            sc.editors = [];
            for(var i = 0; i < sc.paragraphs.length; i++){
                    sc.paragraphs[i].html = sc.convertHtml.makeHtml(sc.paragraphs[i].text);
                    sc.paragraphs[i].display = "false";
            }
            sc.editing = false;
            sc.activeEdit = {"editId": "", "text" : sc.paragraphs[0].text};
            sc.setEditable = function(par){
                var elem = sc.findPar(par);
                var elemId = sc.findParId(par);
                if(sc.editorExists(elem.par)){
                        mdtext = sc.getEditor(elem.par).editor.getSession().getValue();
                        sc.paragraphs[elemId].text = mdtext; 
                        $('.'+elem.par).html(sc.convertHtml.makeHtml(mdtext));
                        sc.paragraphs[elemId].display = false;
                        sc.activeEdit.editId = "";
                        sc.activeEdit.text = "";
                        sc.editing = false;
                }

                                
                else {

                    sc.paragraphs[elemId].display = true;
                    var editor = ace.edit(elem.par);
                    editor.getSession().setValue(sc.paragraphs[elemId].text);
                    editor.setTheme("ace/theme/monokai");
                    editor.getSession().setMode("ace/mode/markdown");  
                    sc.editors.push({"par": elem.par, "editor" : editor});
                    editor.getSession().on('change', function(e) {
                        $('.'+elem.par).html(sc.convertHtml.makeHtml(editor.getSession().getValue()));
                    }); 
                    sc.editing = true;
                    sc.activeEdit.text = sc.paragraphs[elemId].text;
                    sc.activeEdit.editId = elem.par;
       //             sc.setEditPosition(elem.par);
                }               
            };
            
            sc.editorExists = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par == par){
                                    return i;
                            }
                    }
            }

            sc.getEditor = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par == par){
                                    return sc.editors[i]
                            }
                    }
                    // Should never happen:
                    alert("An error has occurred: it appears that editor has broken due to modified paragraph-id.");
                   
            }
            sc.findParId = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par == par){
                        return i;
                    }
                }
            };
            sc.findPar = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par == par){
                        return sc.paragraphs[i];
                    }
                }
            };
            sc.setEditPosition = function(elem){ 
                    element = $('.' + elem.par).get(0);
                    x = element.offset().left;
                    y = element.offset().top;
                    $('#wmd-input').get(0).css({ top: y });
            }
            
        }]); 

            // Render markdown in the HTML page, maybe working?
            timApp.directive("markdown", function ($compile, $http) {
                var mdConvert = new Showdown.converter();
                    return {
                        restrict: 'E',
                        replace: true,
                        link: function (scope, element, attrs) {
                            if ("src" in attrs) {
                                $http.get(attrs.src).then(function(data) {
                                    element.html(mdConvert.makeHtml(data.data));
                                });
                            } else {
                                element.html(mdConvert.makeHtml(element.text()));
                            }
                        }
                    };
             });

            timApp.directive('bindOnce', function() {
                return {
                    scope: true,
                    link: function( $scope ) {
                        setTimeout(function() {
                            $scope.$destroy();
                        }, 0);
                    }
                }
            });
