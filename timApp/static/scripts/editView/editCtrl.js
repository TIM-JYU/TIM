//var controllerProvider = null; // For addittional controllers from plugins.
var EditApp = angular.module('controller', ['ngSanitize', 'angularFileUpload']);


EditApp.controller("ParCtrl", ['$scope', 
                               '$http', 
                               '$q', 
                               '$upload',
                               '$templateCache',
    function(sc, http, q, $upload, $templateCache){
            // Set all requests to also sen the version number of current document
            http.defaults.headers.common.Version = version.hash; 


        // Various regex patterns for editor
            sc.pluginPat = /\{(.*)}\[(.*)]/;


            // Initialize paragraphs, jsonDoc is a global variable made by jinja2-template, it resides in viewTemplate (editing.html) 
            sc.paragraphs = [];
            sc.updateParagraphs = function(){
                    for(var i = 0; i < jsonDoc.length; i++){
                            (function(i){
                                    var text = jsonDoc[i];
                                    sc.paragraphs.push({"par": i, "html": text, "display":false});
                            })(i);
                    }
            };
            sc.updateParagraphs();

            // Meta-data for document, docId and docName are global variables from jinja2-template
            sc.docId = docId;
            sc.docName = docName;
            
            // Auxiliary variables
            sc.uploadFile;
            sc.documentEdit = false;
            sc.sendingNew = false;
            sc.tempVariable = ""; // TODO: only serves to act as temporary storage for data fetched, fix           
            
            // markdown-html converter, currently unused but for future preview purposes might be useful.
            /*sc.convertHtml = new Markdown.getSanitizingConverter();
            sc.convertHtml.hooks.chain("preBlockGamut", function (text, runBlockGamut) {
                var match = sc.pluginPat.exec(text);
                if(match !== null){
                    teksti = sc.fetchAndReplace(text, match[0], match[1], match[2]);
                    return teksti;
                }
                else {
                    return text;
                }
                
            });*/


            
////////////////////////// EDITING ////////////////////////////////////////////
            sc.editors = []; 
            sc.editing = false;
            sc.oldParagraph = "";
            sc.activeEdit = {"editId": "", "editor": ""};
            sc.setEditable = function(par){
                var elem = sc.findPar(par); 
                if(elem.par === sc.activeEdit['editId']){
                    sc.saveEdits(elem, elem.par, sc.sendingNew);
                    sc.activeEdit = {"editId": "", "editor": ""};
                    sc.sendingNew = false;
                    sc.oldParagraph = "";
                }
                else {
                    if(sc.activeEdit.editId !== ""){
                        sc.setEditable(sc.activeEdit.editId);
                    }

                    if(!sc.editorExists(elem.par)){
                            editor = sc.createEditor(elem, elem.par);
                    }
                    else{
                            sc.activeEdit.editId = par;
                            sc.updateEditor(elem, elem.par);
                            sc.activeEdit['editor'] = sc.getEditor(par).editor;
                            sc.oldParagraph = sc.activeEdit['editor'].getSession().getValue();
                    }
                    sc.paragraphs[elem.par].display = true;
                    
                    sc.activeEdit.editId = elem.par;
					var iOS = /(iPad|iPhone|iPod)/g.test( navigator.platform );
					// if ( !iOS ) alert("Not IOS"); else alert("ios");
					// alert(navigator.platform);
					// iPad does not open the keyboard if not manually focused to editable area
                    //if ( !iOS ) sc.activeEdit['editor'].editor.focus();
                }               
            };

            sc.deleteEditor = function(id){
                for(var i = 0;i < sc.editors.length; i++){
                    if(sc.editors[i].par === id){
                        sc.editors.splice(i, 1);
                    }
                }
            }
            sc.parAdd = function(indx){
                    if(sc.sendingNew){
/**                            var element = $("#" + sc.activeEdit.editId.toSring()).toggleClass("flashEdit");
                            element.toggleClass("flashEdit");
                            var backgroundInterval = setTimeout(function(){
                                element.toggleClass("flashEdit");
                            },100)**/
                    }else{
                        sc.addParagraph(indx);
                        sc.setEditable(indx);
                    }
            }
            
            sc.cancelEdit = function(par){
                    if(sc.sendingNew){
                        sc.deleteEditor(par);
                        sc.delParagraph(par);
                        sc.oldParagraph = "";
                        sc.sendingNew = false;
                    }else{
                        sc.activeEdit['editor'].getSession().setValue(sc.oldParagraph);
                        sc.paragraphs[par].display = false;
                        sc.deleteEditor(par);
                        sc.activeEdit = {"editId": "", "editor": ""};
                        sc.sendingNew = false;
                        sc.oldParagraph = "";
                    }
                    
            }            

            sc.editorExists = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par === par){
                                    return true;
                    }}return false;}

            sc.getEditor = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par === par){
                                    return sc.editors[i]
            }}}

            sc.findParId = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par === par){
                        return i;
            }}};

            sc.findPar = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par === par){
                        return sc.paragraphs[i];
            }}};

            sc.getValue = function(i){return i};


            sc.getBlockMd = function(blockId){
                var deferred = q.defer(); 
                getBlockMd = function(blockId){
                    http.get('/getBlock/' +sc.docId + "/" + blockId).
                    success(function(data, status, headers, config) {

                                deferred.resolve(data.md);
                        
                    }).
                    error(function(data, status, headers, config) {
                                deferred.reject("Failed to fetch markdown");
                    });
                }
                if(sc.sendingNew){
                    deferred.resolve("");
                }else{
                    getBlockMd(blockId);
                }
                return deferred.promise;
            }

            sc.createEditor = function(elem, elemId){
                    var promise = sc.getBlockMd(elem.par);
                    promise.then(function(data) {                            
                            editor = new ace.edit(elem.par.toString());                    
                            editor.getSession().setValue(data);
                            editor.setTheme("ace/theme/eclipse");
                            editor.renderer.setPadding(10, 10, 10,10);
                            editor.getSession().setMode("ace/mode/markdown"); 
                            editor.getSession().setUseWrapMode(true);
                            editor.getSession().setWrapLimitRange(0, 79);
                            editor.setOptions({maxLines:40, minLines:3});
                            $('.'+elem.par).get()[0].focus();
                            editor.getSession().on('change', function(e) {

                            });
                            sc.activeEdit["editor"] = editor;
                                    
                            sc.oldParagraph = sc.activeEdit['editor'].getSession().getValue();
                            if(!sc.editorExists(elem.par)) {
                                    sc.editors.push({"par" : elem.par, "editor": editor});
                            }
                    }, function(reason) {
                            alert('Failed: ' + reason);
                    }, function(data) {
                            alert('Request progressing');

                    });

            };

            sc.updateEditor = function(elem, elemId){
                    var promise = sc.getBlockMd(elem.par);
                    promise.then(function(data) {                                                    
                            editor = sc.getEditor(elem.par).editor;                    
                            editor.getSession().setValue(data);
                            }, function(reason) {
                            alert('Failed: ' + reason);
                    }, function(data) {
                            alert('Request progressing');

                    });

            };
/*
            sc.getPlugin = function(text){ 
                var match = sc.pluginPat.exec(text);
                if(match !== null){
                    teksti = sc.fetchAndReplace(text, match[0], match[1], match[2]);
                    return teksti;
                }
                else {
                    return text;
                }
            }
*/
            sc.tempVar = "";
            sc.saveEdits = function(elem, elemId,postingNew){
                    if(sc.oldParagraph === sc.activeEdit.editor.getSession().getValue()){
                            sc.cancelEdit(elemId);
                            return
                    }
                    if(sc.activeEdit.editor.getSession().getValue().length <= 0){
                            sc.delParagraph(elemId);
                    }else{
                            text = sc.activeEdit['editor'].getSession().getValue();
                            sc.paragraphs[elem.par].display = false;
                            promise = q.defer();
                            if(postingNew){
                                    var urli = '/newParagraph/';
                            }else{
                                    var urli = '/postParagraph/';
                            }
                            var savedPars = function(){
                                http({method: 'POST',
                                      url: urli,
                                      data: JSON.stringify({
                                            "docId" : sc.docId,
                                            "par" : elem.par, 
                                            "text": text
                                    })}).success(function(data){
                                            promise.resolve(data);
                                    }).error(function(){
                                            promise.reject("Failed to post data");
                                    });
                            };
                            savedPars();
                            promise.promise.then(
                                function(data){
                                    var newParId = sc.getValue(elem.par);
                                    for(var i = 0; i < data.length; i++){                           
                                        if(i > 0){
                                            sc.addParagraph(newParId);
                                            sc.sendingNew = false; 
//                                            sc.paragraphs[newParId].html = data[sc.getValue(i)];
                                            sc.paragraphs[newParId].html = data[sc.getValue(i)];
                                            //$("." + (newParId).toString()).get()[0].innerHTML = sc.paragraphs[sc.getValue(newParId)].html;
                                        
                                        }else{                                            
                                            $templateCache.put("plugin", data[sc.getValue(i)]);
                                            sc.paragraphs[newParId].html = data[sc.getValue(i)];
                                            sc.updateEditor(elem, elemId);
                                            //$("." + newParId).get()[0].innerHTML = sc.paragraphs[sc.getValue(newParId)].html;  
                                            
                                        }
                                        sc.$apply();
                                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, "par-" + newParId]);
                                        newParId = newParId + 1;
                                    }
                                    sc.sendingNew = false; 
                                    sc.paragraphs[elemId].loading = "";
                                    },
                                    function(reason){
                                        alert(reason);
                                    sc.paragraphs[elemId].loading = "";
                                    },
                                    function(){
                                    });
                    }
            }            

            sc.callDelete = function(blockId){
                var deferred = q.defer();
                removePar = function(blockId){ 
                    http.get('/deleteParagraph/' + sc.docId + "/" + blockId).
                        success(function(data, status, headers, config) {
                            deferred.resolve(data);
                        }).
                        error(function(data, status, headers, config) {
                            deferred.reject("Failed to fetch html")
                        });
                }
                removePar(blockId);
                return deferred.promise;
            }
            sc.delParagraph = function(indx){
                    if(!sc.sendingNew && !(confirm("Delete paragraph?"))){
                            return;
                    }
                    sc.editors = [];
                    sc.activeEdit = {"editId": "", "editor": ""};
                    if(sc.sendingNew){
                        sc.paragraphs.splice(indx, 1);
                    }else{
                        var promise = sc.callDelete(indx);
                        promise.then(function(data){
                            sc.paragraphs.splice(indx, 1);
                            sc.oldParagraph = "";
                            sc.$apply();
                        }, function(reason) {
                                alert('Failed: ' + reason);
                        }, function(data) {
                                alert('Request progressing');

                        });
                    }
                    sc.sendingNew = false;
                    sc.decreaseIds(indx);

            };

            sc.increaseIds = function(i){
                    for(var j = i+1; j < sc.paragraphs.length;j++){
                        sc.paragraphs[j].par = sc.paragraphs[j].par + 1;
                    }
                    for(var z = 0; z < sc.editors.length; z++){
                            if(sc.editors[z].par >= i){
                                    sc.editors[z].par = sc.editors[z].par + 1;
                            }
                    }

            }

            sc.decreaseIds = function(i){
                            for(var j = i; j < sc.paragraphs.length;j++){
                                sc.paragraphs[j].par = sc.paragraphs[j].par - 1;
                            }
                            for(var z = 0; z < sc.editors.length; z++){
                                if(sc.editors[z].par >= i){
                                       sc.editors[z].par = sc.editors[z].par - 1;
                                }
                            }
            }
            
            sc.addParagraph = function(indx){
                    if(sc.activeEdit.editId !== ""){
                        sc.setEditable(sc.activeEdit.editId);
                    }
                    if(sc.sendingNew){
                            sc.cancelEdit(sc.activeEdit.editId);
                    }
                    sc.paragraphs.splice(indx, 0, {"par": indx, "html" : "<empty>", "display" : false});   
                    var i = (function(i){return i})(indx);
                    sc.increaseIds(indx);
                    sc.sendingNew = true;
                    sc.$apply();
            };
            
            sc.uploadedFile;
            sc.progress;
            sc.selectedFile = "";
            sc.onFileSelect = function(url, $files) {
                //$files: an array of files selected, each file has name, size, and type.
                for (var i = 0; i < $files.length; i++) {
                  var file = $files[i];
                  sc.upload = $upload.upload({
                    url: url,
                    method: 'POST',
                    file: file,
                  }).progress(function(evt) {
                    sc.progress = 'Uploading... ' + parseInt(100.0 * evt.loaded / evt.total) + '%';
                  }).success(function(data, status, headers, config) {
                    sc.uploadedFile = '/images/' + data.file;
                    sc.progress = 'Uploading... Done!'
                  });
                  //.error(...)
                }
              };
            
        }]);

EditApp.filter('to_trusted', ['$sce', function($sce){
        return function(text) {
                return $sce.trustAsHtml(text);
        };
}]);




