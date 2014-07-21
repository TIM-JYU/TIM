var EditCtrl = angular.module('controller', []);

EditCtrl.controller("ParCtrl", ['$scope', '$http', '$q', 'fileUpload', function(sc, http, q, fileUpload){
            http.defaults.headers.common.Version = version.hash;
            sc.callPlugin = function(plugin, params){
                var promise = q.defer();
                var callPlugin = function(plugin, params){
                    http.get('/pluginCall/' + plugin + '/?param=' + encodeURIComponent(params)).
                        success(function(data, status, headers, config) {
                                 promise.resolve(data);
                        }).
                        error(function(data, status, headers, config) {
                                 promise.reject("unspecified plugin");
                        });
               }
               callPlugin(plugin, params);
               return promise.promise;

            };

            sc.fetchAndReplace = function(text, wholeMatch, plugin, params){ 
                    pluginPromise = sc.callPlugin(plugin, params);
                    pluginPromise.then(function(data){
                                            sc.tempVariable = data; 
                                       },
                                       function(){},
                                       function(){});
                    receivedText = sc.tempVariable;
                    return text.replace(wholeMatch, receivedText);
            }


        // Various regex patterns for editor
            sc.pluginPat = /\{(.*)}\[(.*)]/;
            // Initialize paragraphs, jsonDoc is a global variable made by jinja2-template, it resides in viewTemplate (editing.html) 
            sc.paragraphs = [];
            sc.updateParagraphs = function(){
                    for(var i = 0; i < jsonDoc.length; i++){
                            (function(i){
                                    var text = jsonDoc[i];
                                    sc.paragraphs.push({"par": i, "html": text});
                                    var match = sc.pluginPat.exec(text);
                                    if(match !== null){
                                            var promise = sc.callPlugin(match[1], match[2]);
                                            promise.then(
                                                    function(data){ 
                                                            text = text.replace(match[0], data);
                                                            $("." + i.toString()).get()[0].innerHTML = text;
                                                    },
                                                    function(){},
                                                    function(){});
                                    }
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
            sc.convertHtml = new Markdown.getSanitizingConverter();
            sc.convertHtml.hooks.chain("preBlockGamut", function (text, runBlockGamut) {
                var match = sc.pluginPat.exec(text);
                if(match !== null){
                    teksti = sc.fetchAndReplace(text, match[0], match[1], match[2]);
                    return teksti;
                }
                else {
                    return text;
                }
                
            });


            
////////////////////////// EDITING ////////////////////////////////////////////
            sc.editors = []; 
            sc.editing = false;
            sc.oldParagraph = "";
            sc.activeEdit = {"editId": "", "editor": ""};
            sc.setEditable = function(par){
                var elem = sc.findPar(par);
                var elemId = sc.findParId(par); 
                if(elem.par === sc.activeEdit['editId']){
                    sc.saveEdits(elem, elemId,sc.sendingNew);
                    sc.activeEdit = {"editId": "", "editor": ""};
                    sc.sendingNew = false;
                    sc.oldParagraph = "";

               }
                else {
                    if(sc.activeEdit.editId !== ""){
                        sc.setEditable(sc.activeEdit.editId);
                    }

                    if(!sc.editorExists(elem.par)){
                            sc.createEditor(elem,elemId);
                    }
                    else{
                            sc.activeEdit.editId = par;
                            sc.updateEditor(elem, elemId);
                            sc.activeEdit['editor'] = sc.getEditor(par).editor;
                            sc.oldParagraph = sc.activeEdit['editor'].getSession().getValue();
                    }
                    sc.paragraphs[elemId].display = true;
                    
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
                            }
                    }
                    return false;
            }

            sc.getEditor = function(par){
                    for(var i = 0; i < sc.editors.length; i++){
                            if(sc.editors[i].par === par){
                                    return sc.editors[i]
                            }
                    }
            }
            sc.findParId = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par === par){
                        return i;
                    }
                }
            };
            sc.findPar = function(par){
                for(var i = 0; i < sc.paragraphs.length; i++){
                    if (sc.paragraphs[i].par === par){
                        return sc.paragraphs[i];
                    }
                }
            };

            sc.getValue = function(i){return i};


            sc.getBlockMd = function(blockId){
                var deferred = q.defer(); 
                getBlockMd = function(blockId){
                    http.get('/getBlock/' +sc.docId + "/" + blockId).
                    success(function(data, status, headers, config) {
                                deferred.resolve(data);
                        
                    }).
                    error(function(data, status, headers, config) {
                        deferred.reject("Failed to fetch markdown")
                    });
                }
                if(sc.sendingNew){
                   deferred.resolve("<empty>");
                }else{
                    getBlockMd(blockId);
                }
                return deferred.promise;
            }
/**
            sc.getBlockHtml = function(blockId){
                var deferred = q.defer();
                getBlockHtml = function(blockId){ 
                    http.get('/getBlockHtml/' + sc.docId + "/" + blockId).
                        success(function(data, status, headers, config) {
                            deferred.resolve(data);
                        }).
                        error(function(data, status, headers, config) {
                            deferred.reject("Failed to fetch html")
                        });
                }
                getBlockHtml(blockId);
                return deferred.promise;
            }

            sc.promiseOfHtml = function(elemId){
                    var promise = sc.getBlockHtml(elemId);
                    promise.then(function(data){
                            var match = sc.pluginPat.exec(data);
                            if(match !== null){
                                data = sc.fetchAndReplace(data, match[0], match[1], match[2]);
                            }
                            $("." + elemId).get()[0].innerHTML = data;  
                            sc.paragraphs[elemId].html = data;
                    
                    }, function(reason) {
                            alert('Failed: ' + reason);
                    }, function(data) {
                            alert('Request progressing');

                    });
            }
**/            
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
                            if(!sc.editorExists(elem.par)) {
                                    sc.editors.push({"par" : elem.par, "editor": editor});
                            }
                            sc.oldParagraph = sc.activeEdit['editor'].getSession().getValue();
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

            sc.saveEdits = function(elem, elemId,postingNew){
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
                                            "docName" : sc.docId,
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
                                            sc.paragraphs[newParId].html = data[sc.getValue(i)];
                                            $("." + (newParId).toString()).get()[0].innerHTML = sc.paragraphs[sc.getValue(newParId)].html;   
                                        }else{
                                            sc.paragraphs[newParId].html = data[sc.getValue(i)];
                                            sc.updateEditor(elem, elemId);
                                            $("." + newParId).get()[0].innerHTML = sc.paragraphs[sc.getValue(newParId)].html;  
                                        }
                                        newParId = newParId + 1;
                                    }
                                    sc.sendingNew = false; 
                                    },
                                    function(reason){
                                        alert(reason);
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
                    sc.editors = [];
                    sc.activeEdit = {"editId": "", "editor": ""};
                    if(sc.sendingNew){
                        sc.paragraphs.splice(indx, 1);
                    }else{
                        var promise = sc.callDelete(indx);
                    
                        promise.then(function(data){
                            sc.paragraphs.splice(indx, 1);
                            var i = (function(i){return i})(indx);
                            for(var j = i; j < sc.paragraphs.length;j++){
                                sc.paragraphs[j].par = sc.paragraphs[j].par - 1;
                            }
                            for(var z = 0; z < sc.editors.length; z++){
                                if(sc.editors[z].par >= indx){
                                       sc.editors[z].par = sc.editors[z].par - 1;
                                }
                            }
                            sc.oldParagraph = "";
                            sc.$apply();
                        }, function(reason) {
                                alert('Failed: ' + reason);
                        }, function(data) {
                                alert('Request progressing');

                        });
                    }
                    sc.sendingNew = false;


            };
            
            sc.addParagraph = function(indx){
                    sc.paragraphs.splice(indx, 0, {"par": indx, "html" : "<empty>", "display" : false});   
                    var i = (function(i){return i})(indx);
                    for(var j = i+1; j < sc.paragraphs.length;j++){
                        sc.paragraphs[j].par = sc.paragraphs[j].par + 1;
                    }
                    for(var z = 0; z < sc.editors.length; z++){
                            if(sc.editors[z].par >= indx){
                                    sc.editors[z].par = sc.editors[z].par + 1;
                            }
                    }
                    sc.sendingNew = true;
                    sc.$apply();
            };

            sc.myFile;
            sc.uploadFile = function(){
                var file = sc.myFile;
                var uploadUrl = '/upload/';
                fileUpload.uploadFileToUrl(file, uploadUrl);
            };

        }]);



EditCtrl.directive('fileModel', ['$parse', function ($parse) {
                return {
                       restrict: 'A',
                       link: function(scope, element, attrs) {
                                 var model = $parse(attrs.fileModel);
                                 var modelSetter = model.assign;
                
                                 element.bind('change', function(){
                                    scope.$apply(function(){
                                        modelSetter(scope, element[0].files[0]);
                                   });
                                 });
        }
        };
}]);

EditCtrl.service('fileUpload', ['$http', function ($http) {
       this.uploadFileToUrl = function(file, uploadUrl){
                var fd = new FormData();
              fd.append('file', file);
                $http.post(uploadUrl, fd, {
                    transformRequest: angular.identity,
                     headers: {'Content-Type': undefined}
             })
             .success(function(){
            })
           .error(function(){
            });
     }
}]); 
