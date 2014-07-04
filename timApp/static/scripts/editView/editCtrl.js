var EditCtrl = angular.module('controller', []);

EditCtrl.controller("ParCtrl", ['$scope', '$http', '$q', function(sc, http, q){
            sc.paragraphs = [];
            for(var i = 0; i <= jsonDoc.length; i++){
                    sc.paragraphs.push({"par": i, "html": jsonDoc[i]});
            }
            sc.pluginPat = /\{plugin}\[(.*)][(.*)]/;
            sc.documentEdit = false;
            sc.docId = docId;
            sc.docName = docName;
            sc.uploadFile;
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

            // Get document
            sc.getDocument = function(documentName){
                sc.displayIndex = false;
                http({method: 'GET', url: '/getJSON/' + documentName}).
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
                   // var text = sc.paragraphs[i].text;
                   // var match = sc.pluginPat.exec(sc.paragraphs[i].text);
                   // if(match != null){
                   //     text = sc.fetchAndReplace(text, match[0], match[1]);
                   // }
                    sc.paragraphs[i].html = sc.convertHtml.makeHtml(text);
                    sc.paragraphs[i].display = false;
                }
            }
            sc.fetchAndReplace = function(text, wholeMatch, match, params){ 
                    pluginText = sc.callPlugin(match, params);
                    receivedText = sc.tempVariable;
                    return text.replace(wholeMatch, receivedText);
            }

            sc.tempVariable = ""; // TODO: only serves to act as temporary storage for data fetched, fix           
            sc.callPlugin = function(plugin, params){
                http.get('/pluginCall/' + plugin + '/' + params).
                    success(function(data, status, headers, config) {
                             sc.tempVariable = data;
                    }).
                    error(function(data, status, headers, config) {
                             return "[unspecified plugin]"
                    });
            }



            sc.editors = []; 
            sc.editing = false;
            sc.activeEdit = {"editId": "", "text" : "", "editor": ""};
            sc.setEditable = function(par){
                var elem = sc.findPar(par);
                var elemId = sc.findParId(par); 
                if(elem.par === sc.activeEdit['editId']){
                    sc.saveEdits(elem, elemId);
                    sc.activeEdit = {"editId": "", "text" : "", "editor": ""};
                    sc.promiseOfHtml(par);
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
                            sc.activeEdit['editor'] = sc.getEditor(par).editor;
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
                getBlockMd(blockId);
                return deferred.promise;
            }

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
                getBlockHtml();
                return deferred.promise;
            }

            sc.promiseOfHtml = function(elemId){
                    var promise = sc.getBlockHtml(elemId);
                    alert(elemId);
                    alert($("." + elemId).get()[0].innerHTML);
                    promise.then(function(data){
                            $window.alert("LOL");
                            $("." + elemId).get()[0].innerHTML = data;   
                    
                    }, function(reason) {
                            alert('Failed: ' + reason);
                    }, function(data) {
                            alert('Request progressing');

                    });
            }
            
            sc.createEditor = function(elem, elemId){
                    var promise = sc.getBlockMd(elem.par);
                    promise.then(function(data) {
                            sc.activeEdit['text'] = data;
                            editor = new ace.edit(elem.par.toString());                    
                            editor.getSession().setValue(sc.activeEdit.text);
                            editor.setTheme("ace/theme/eclipse");
                            editor.renderer.setPadding(10, 10, 10,10);
                            editor.getSession().setMode("ace/mode/markdown"); 
                            editor.getSession().setUseWrapMode(true);
                            editor.getSession().setWrapLimitRange(0, 79);
                            $('.'+elem.par).get()[0].focus();
                            editor.getSession().on('change', function(e) {
                                    //text = sc.activeEdit['editor'].editor.getSession().getValue();
                            });
/**                            editor.on('blur', function(e){
                                    sc.setEditable(elem.par);
                            });**/
                            sc.activeEdit["editor"] = editor;
                            if(!sc.editorExists(elem.par)) {
                                    sc.editors.push({"par" : elem.par, "editor": editor});
                            }
                            sc.activeEdit['text'] = sc.tempVariable;
                            // sc.setEditable(elem.par);
                    }, function(reason) {
                            alert('Failed: ' + reason);
                    }, function(data) {
                            alert('Request progressing');

                    });

            };
            sc.saveEdits = function(elem, elemId){
                    text = sc.activeEdit['editor'].getSession().getValue();
                    //sc.paragraphs[elemId].text = mdtext; 
                    //$('.'+elem.par).html(sc.convertHtml.makeHtml(mdtext));
                    sc.paragraphs[elemId].display = false;
                    http({method: 'POST',
                          url: '/postParagraph/',
                          data: JSON.stringify({
                                                "docName" : sc.docId,
                                                "par" : elem.par, 
                                                "text": text
                          })});
            }


            sc.uploadFile = function(){
                var file = $scope.myFile;
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
