var TimCtrl = angular.module('controllers', []);

TimCtrl.controller("ParCtrl", ['$scope', '$http', '$q', function(sc, http, q){
            sc.paragraphs = "";
            sc.pluginPat = /\{plugin}\[(.*)][(.*)]/;
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
            sc.uploadFile;
            sc.getDocIds();
            sc.convertHtml = new Markdown.getSanitizingConverter();
            sc.convertHtml.hooks.chain("preBlockGamut", function (text, runBlockGamut) {
                var match = sc.pluginPat.exec(text);
                if(match != null){
                    teksti = sc.fetchAndReplace(text, match[0], match[1], match[2]);
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
                    var text = sc.paragraphs[i].text;
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
                    sc.activeEdit['editor'].editor.focus();
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
/**                    editor.on('blur', function(e){
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


            sc.uploadFile = function(){
                var file = $scope.myFile;
                var uploadUrl = '/upload/';
                fileUpload.uploadFileToUrl(file, uploadUrl);
            };

        }]);



TimCtrl.directive('fileModel', ['$parse', function ($parse) {
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

TimCtrl.service('fileUpload', ['$http', function ($http) {
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
