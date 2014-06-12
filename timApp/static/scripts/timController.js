var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("ParCtrl", ['$scope', '$http', function(sc, http){
            sc.paragraphs = "";
            sc.documentEdit = false;
            sc.documentList = documents;
            sc.convertHtml = new Markdown.getSanitizingConverter();
            sc.editors = [];
            sc.displayIndex = true;
            // Get document
            sc.getDocument = function(documentName){
                sc.displayIndex = false;
                http({method: 'GET', url: '/getJSON/' + documentName}).
                //http({method: 'GET', url: '/document/' + "3/"}).
                    success(function(data, status, headers, config) {
                        sc.displayIndex = false;
                        sc.paragraphs = data.text;
                        sc.updateParagraphs();
                    }).
                    error(function(data, status, headers, config) {
                        sc.displayIndex = false;
                        alert("Document is not currently available");
                    });
            };

            sc.updateParagraphs = function(){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    sc.paragraphs[i].html = sc.convertHtml.makeHtml(sc.paragraphs[i].text);
                    sc.paragraphs[i].display = "false";
                }
            }
            

            sc.editing = false;
            sc.activeEdit = {"editId": "", "text" : ""};
            sc.setEditable = function(par){
                var elem = sc.findPar(par);
                var elemId = sc.findParId(par);
                if(sc.paragraphs[elemId].display == true && sc.editorExists(elem.par)){
                        mdtext = sc.getEditor(elem.par).editor.getSession().getValue();
                        sc.paragraphs[elemId].text = mdtext; 
                        $('.'+elem.par).html(sc.convertHtml.makeHtml(mdtext));
                        sc.paragraphs[elemId].display = false;
                        sc.activeEdit.editId = "";
                        sc.activeEdit.text = "";
                        http({method: 'POST',
                               url: '/postParagraph/',
                               data: JSON.stringify({"documentName":sc.documentName,
                                                     "par" : elem.par, 
                                                     "text": elem.text})})
                               
                         
                }

                                
                else {
                    sc.paragraphs[elemId].display = true;
                    sc.createEditor(elem, elemId);
                    sc.activeEdit.text = sc.paragraphs[elemId].text;
                    sc.activeEdit.editId = elem.par;
                }               
            };
            
            sc.editorExists = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par == par){
                                    return true;
                            }
                    }
                    return false;
            }

            sc.getEditor = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par == par){
                                    return sc.editors[i]
                            }
                    }
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
            
            sc.createEditor = function(elem, elemId){
                    var editor = ace.edit(elem.par);
                    editor.getSession().setValue(sc.paragraphs[elemId].text);
                    editor.setTheme("ace/theme/eclipse");
                    editor.renderer.setPadding(10, 10, 10,10);
                    editor.getSession().setMode("ace/mode/markdown"); 
                    editor.getSession().setUseWrapMode(true);
                    editor.getSession().setWrapLimitRange(0, 79);
                    sc.editors.push({"par": elem.par, "editor" : editor});
                    editor.getSession().on('change', function(e) {
                            $('.'+elem.par).html(sc.convertHtml.makeHtml(editor.getSession().getValue()));
                    });
            };
        }]);









