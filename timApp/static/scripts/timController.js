var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("ParCtrl", ['$scope', '$http', '$q', function(sc, http, q){
            sc.paragraphs = "";
            sc.pluginPat = /\{plugin}\[(.*)]/;
            sc.getDocIds = function(){
                http({method: 'GET', url: '/getDocuments/'}).
                    success(function(data, status, headers, config) {
                            sc.documentList = data;
                    }).
                    error(function(data, status, headers, config) {
                            sc.documentList = [];
                    });
            };
            sc.documentEdit = false;
            sc.documentList = [];
            sc.getDocIds();
            sc.convertHtml = new Markdown.getSanitizingConverter();
           
            sc.convertHtml.hooks.chain("preBlockGamut", function (text, runBlockGamut) {
                var pat = /\{plugin}\[(.*)]/;
                var match = pat.exec(text);
                if(match != null){
                    alert(match[0] + "   " + match[1]);   
                    teksti = text.replace(match[0], sc.pluginPromise(match[1]).then(function(data){return JSON.stringify(data)}));
                    return teksti;
                }
                else {
                    return text;
                }
                
            });


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
                    sc.paragraphs[i].display = false;
                }
            }

            sc.pluginPromise = function(plugin){
                var deferred = q.defer();
                http({method: 'GET', url: '/pluginCall/' + plugin}).
                    success(function(data, status, headers, config) {
                            alert(data);
                            deferred.resolve(data.toString('utf-8'))
                    }).
                    error(function(data, status, headers, config) {
                            deferred.reject( "[unspecified plugin]")
                    });
                return deferred.promise;
            }
          
            sc.editors = []; 
            sc.editing = false;
            sc.activeEdit = {"editId": "", "text" : "", "editor": ""};
            sc.setEditable = function(par){
                var elem = sc.findPar(par);
                var elemId = sc.findParId(par); 
                if(elem.par == sc.activeEdit['editId']){
                    sc.saveEdits(elem, elemId);
                    sc.activeEdit = {"editId": "", "text" : "", "editor": ""};
               }
                else {
                    if(sc.activeEdit.editId != ""){
                        sc.setEditable(sc.activeEdit.editId);
                    }

                    if(!sc.editorExists(elem.par)){
                            sc.createEditor(elem,elemId);
                            sc.activeEdit['editor'] = sc.getEditor(par);
                    }
                    else{
                            sc.activeEdit['editor'] = sc.getEditor(par);
                    }
                    sc.paragraphs[elemId].display = true;
                    
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
                    editor = new ace.edit(elem.par);
                    editor.getSession().setValue(sc.paragraphs[elemId].text);
                    editor.setTheme("ace/theme/eclipse");
                    editor.renderer.setPadding(10, 10, 10,10);
                    editor.getSession().setMode("ace/mode/markdown"); 
                    editor.getSession().setUseWrapMode(true);
                    editor.getSession().setWrapLimitRange(0, 79);
                    $('.'+elem.par).get()[0].focus();
                    editor.getSession().on('change', function(e) {
                            text = sc.activeEdit['editor'].editor.getSession().getValue();

                            $('.'+elem.par).html(sc.convertHtml.makeHtml(text));
                    });
                   /** editor.on('blur', function(e){
                            sc.setEditable(elem.par);
                    });**/
                    sc.activeEdit["editor"] = editor;
                    if(!sc.editorExists(elem)) {
                            sc.editors.push({"par" : elem.par, "editor": editor});
                    }
            };
            sc.saveEdits = function(elem, elemId){
                    mdtext = sc.activeEdit['editor'].editor.getSession().getValue();
                    sc.paragraphs[elemId].text = mdtext; 
                    $('.'+elem.par).html(sc.convertHtml.makeHtml(mdtext));
                    sc.paragraphs[elemId].display = false;
                    http({method: 'POST',
                          url: '/postParagraph/',
                          data: JSON.stringify({"documentName":sc.documentName,
                                                "par" : elem.par, 
                                                "text": elem.text})})
                                        
            }

        }]);









