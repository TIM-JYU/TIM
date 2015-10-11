"use strict";
var csPluginStartTime = new Date();
/*
Sagea varten ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst#id3
*/
var csApp = angular.module('csApp', ['ngSanitize']);
csApp.directive('csRunner',['$sanitize','$compile', function ($sanitize,$compile1) {	"use strict"; csApp.sanitize = $sanitize; csApp.compile = $compile1; return csApp.directiveFunction('console',false); }]);
csApp.directive('csJypeliRunner', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('jypeli',false); }]);
csApp.directive('csComtestRunner', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('comtest',false); }]);
csApp.directive('csRunnerInput',['$sanitize','$compile', function ($sanitize,$compile1) { "use strict";	csApp.sanitize = $sanitize; csApp.compile = $compile1; return csApp.directiveFunction('console',true); }]);
csApp.directive('csJypeliRunnerInput', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('jypeli',true); }]);
csApp.directive('csComtestRunnerInput', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('comtest',true); }]);
csApp.directive('csTaunoRunner', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('tauno',false); }]);
csApp.directive('csTaunoRunnerInput', ['$sanitize','$compile', function ($sanitize,$compile1) {"use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('tauno',true); }]);
csApp.directive('csParsonsRunner', ['$sanitize','$compile', function ($sanitize,$compile1) { "use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('parsons',false); }]);
csApp.directive('csSageRunner', ['$sanitize','$compile', function ($sanitize,$compile1) {"use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('sage',true); }]);
csApp.directive('csSimcirRunner', ['$sanitize','$compile', function ($sanitize,$compile1) {"use strict"; csApp.sanitize = $sanitize;  csApp.compile = $compile1; return csApp.directiveFunction('simcir',false); }]);
// csApp.directive('csRunner',function() {	csApp.sanitize = $sanitize; return csApp.directiveFunction('console'); }); // jos ei tarviiis sanitize


function csLogTime(msg) {
   var d = new Date();       
   var diff = d - csPluginStartTime;
   console.log("cs: " + d.toLocaleTimeString()+ " " + diff.valueOf() + " - " + msg);          
}   

csLogTime("directives done");


var TESTWITHOUTPLUGINS = true && false;
csApp.taunoNr = 0;

//==============================================================
// Global object to store every plugin that wants to
// know when pwd changes.  plugin must implement (or scope)
// setPWD method.  Also it should have property path = "user"
// to be able to register.
var ConsolePWD = {};

ConsolePWD.pwdHolders = [];

ConsolePWD.currentPWD = "/home/agent";

ConsolePWD.register = function(scope) {
    if ( !ConsolePWD.isUser(scope) ) return;
    ConsolePWD.pwdHolders.push(scope);
}    

ConsolePWD.isUser = function(scope) {
    return ( scope.path === "user" );
 
}    

ConsolePWD.setPWD = function(pwd,scope) {
    if ( !ConsolePWD.isUser(scope) ) {
        scope.setPWD("/home/agent");
        return;
    }
    
    ConsolePWD.currentPWD = pwd;
    for (var i = 0; i <ConsolePWD.pwdHolders.length; i++) {
        var pwdHolder = ConsolePWD.pwdHolders[i];
        pwdHolder.setPWD(pwd);
    }
}

ConsolePWD.getPWD = function() {
    return ConsolePWD.currentPWD;
}
//==============================================================



// =================================================================================================================
// Things for known languages

var languageTypes = {};
// What are known language types (be carefull not to include partial word):
languageTypes.runTypes     = ["jypeli","java","graphics","cc","c++","shell","py","fs","clisp","jjs","psql","sql","alloy","text","cs","run","md","js","sage","simcir","r"];
languageTypes.aceModes     = ["csharp","java","java"    ,"c_cpp","c_cpp","sh","python","fsharp","lisp","javascript","sql","sql","alloy","text","csharp","run","markdown","javascript","python","json","r"];
// For editor modes see: http://ace.c9.io/build/kitchen-sink.html ja sieltä http://ace.c9.io/build/demo/kitchen-sink/demo.js

// What are known test types (be carefull not to include partial word):
languageTypes.testTypes = ["ccomtest","jcomtest","comtest","junit"];

// If test type is comtest, how to change it for specific languages
languageTypes.impTestTypes = {cs:"comtest", console:"comtest", cc:"ccomtest", java:"jcomtest"};
languageTypes.impTestTypes["c++"] = "ccomtest";

languageTypes.whatIsIn = function (types, type, def) {
"use strict";
    if (!type) return def;
    type = type.toLowerCase();
    for (var i=0; i< types.length; i++)
        if ( type.indexOf(types[i]) >= 0 ) return types[i];
    return def;
};

languageTypes.whatIsInAce = function (types, type, def) {
"use strict";
    if (!type) return def;
    type = type.toLowerCase();
    for (var i=0; i< types.length; i++)
        if ( type.indexOf(types[i]) >= 0 ) return languageTypes.aceModes[i];
    return def;
};

languageTypes.getRunType = function(type,def) {
"use strict";
    return this.whatIsIn(this.runTypes,type,def);
};

languageTypes.getAceModeType = function(type,def) {
"use strict";
    return this.whatIsInAce(this.runTypes,type,def);
};

languageTypes.getTestType = function(type,def) {
"use strict";
    var t = this.whatIsIn(this.testTypes,type,def);
    if ( t !== "comtest" ) return t;
    var lt = this.whatIsIn(this.runTypes,type,"console");
    var impt = this.impTestTypes[lt];
    if ( impt ) return impt;
    return t;     
};
// =================================================================================================================

var removeXML = function(s) {
"use strict";
    s = s.replace(/^<\?xml [^>]*\?>/,"");
    s = s.replace(/(<svg [^>]*height="[0-9]+)pt/,"$1");
    s = s.replace(/(<svg [^>]*width="[0-9]+)pt/,"$1");
    return s;
};

var iotaPermutation = function(n) {
    var permutation = [];
    for (var i = 0; i < n; i++) permutation.push(i);
    return permutation;
};    
    


csApp.directive('contenteditable', ['$sce', function($sce) {
"use strict";
    return {
      restrict: 'A', // only activate on element attribute
      require: '?ngModel', // get a hold of NgModelController
      link: function(scope, element, attrs, ngModel) {
        if(!ngModel) return; // do nothing if no ng-model

        // Specify how UI should be updated
        ngModel.$render = function() {
          // element.html($sce.getTrustedHtml(ngModel.$viewValue || ''));
          element.text(ngModel.$viewValue || '');
        };

        // Listen for change events to enable binding
        element.on('blur keyup change', function() {
          scope.$apply(read);
        });
        read(); // initialize

        // Write data to the model
        function read() {
          // var html = element.html();
          var text = element.html();
		  // if ( text.indexOf("<div>") >= 0 )  { text = text.replace("<div>","").replace("</div>","\n"); element.html(text); }
          text = element.text();
          // When we clear the content editable the browser leaves a <br> behind
          // If strip-br attribute is provided then we strip this out
          // if( attrs.stripBr && html == '<br>' ) {
          //   html = '';
          // }
          ngModel.$setViewValue(text);
        }
      }
    };
  }]);


csApp.commentTrim = function(s) {
"use strict";
	if ( !s ) return "";
	var n = s.indexOf("//\n");
    if (n !== 0) return s;
	return s.substr(3); 
};

csApp.getHeading = function(a,key,$scope,defElem) {
"use strict";
	var h = csApp.set($scope,a,key,"");
	if ( !h ) return "";
	// if ( h.toLowerCase().indexOf("script") >= 0 ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = defElem;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	var ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	// var ea = elem.split(" ",2);
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	// if ( elem.toLowerCase().indexOf("script") >= 0 ) return "";
	// attributes = "";  // ei laiteta näitä, niin on vähän turvallisempi
	try {
	  // val = decodeURIComponent(escape(val));
	  val = decodeURIComponent(encodeURI(val));
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = csApp.sanitize(html);
	return html;
};


csApp.directiveTemplateCS = function(t,isInput) {
"use strict";
	csApp.taunoPHIndex = 3;
    csLogTime("dir templ " + t);
    if ( TESTWITHOUTPLUGINS ) return '';
    var taunoText = "Tauno";
    if ( t === "simcir" ) taunoText = "SimCir";
	return  '<div class="csRunDiv no-popup-menu" ng-cloak>' + 
    
				  '<p>Here comes header</p>' +
				//  '<p ng-bind-html="getHeader()"></p>
				  '<p ng-if="stem" class="stem" >{{stem}}</p>' +
  				  (t === "tauno" || t === "simcir" ?
				    '<p ng-if="taunoOn" class="pluginHide""><a ng-click="hideTauno()">hide ' + taunoText + '</a></p>' +
				    '<div ><p></p></div>' + // Tauno code place holder nr 3!!
				    '<p ng-if="!taunoOn" class="pluginShow" ><a ng-click="showTauno()">Click here to show ' + taunoText + '</a></p>' +
				    (t === "tauno" ? '<p ng-if="taunoOn" class="pluginHide"" ><a ng-click="copyTauno()">copy from Tauno</a> | <a ng-click="hideTauno()">hide Tauno</a></p>' +
				    '<p ng-if="taunoOn" class="taunoOhje">Kopioi Taunolla tekemäsi koodi "copy from Tauno"-linkkiä painamalla. Sitten paina Aja-painiketta. Huomaa että ajossa voi olla eri taulukko kuin Taunossa!</a></p>' 
                    : '<p ng-if="taunoOn && !noeditor" class="pluginHide"" ><a ng-click="copyFromSimcir()">copy from SimCir</a> | <a ng-click="copyToSimcir()">copy to SimCir</a> | <a ng-click="hideTauno()">hide SimCir</a></p>') +
					"" : "") +   
				  '<pre ng-if="viewCode && codeover">{{code}}</pre>'+
				  '<div class="csRunCode">'+
                  // '<p></p>'+
				  '<pre class="csRunPre" ng-if="viewCode &&!codeunder &&!codeover">{{precode}}</pre>'+
                  '<div class="csEditorAreaDiv">'+
                  '<div  class="csrunEditorDiv">'+
				  '<textarea class="csRunArea csEditArea no-popup-menu" ng-hide="noeditor && !viewCode" rows="{{rows}}" ng-model="usercode" ng-trim="false" ng-attr-placeholder="{{placeholder}}"></textarea>'+
                  '</div>'+
                  // (t=="sage" ? '</div>' : '') +

				  '<div class="csRunChanged" ng-if="usercode!=byCode"></div>'+
				  //'<div class="csRunChanged" ng-if="muokattu"></div>'+
                  '</div>'+
				  //'<div class="csRunArea" contentEditable ng-model="usercode" ng-trim="false" "></div>'+
				  '<pre class="csRunPost" ng-if="viewCode &&!codeunder &&!codeover">{{postcode}}</pre>'+
				  '</div>'+
				  //'<br />'+ 
                  (t=="sage" ? '<div class="computeSage no-popup-menu"></div>' : '')+
                  
                  (isInput  ?
                  '<div class="csInputDiv" ng-hide="!showInput">' +
                  '<p ng-show="inputstem" class="stem" >{{inputstem}}</p>' +
                  '<div class="csRunCode" >' +
				  '<textarea class="csRunArea csInputArea"  rows={{inputrows}} ng-model="userinput" ng-trim="false" placeholder="{{inputplaceholder}}"></textarea>'+
                  '</div>' + 
                  '</div>' + 
                  '<div class="csArgsDiv" ng-hide="!showArgs">' +
				  '<label>{{argsstem}} </label><span><input type ="text" class="csArgsArea" ng-model="userargs" ng-trim="false" placeholder="{{argsplaceholder}}"></span>'+
                  '</div>' +
                  ''
                  : "") + // end of isInput
                  
                  
				  '<p class="csRunSnippets" ng-if="buttons">' + // && viewCode">' +
				  '<button ng-repeat="item in buttons" ng-click="addText(item);">{{addTextHtml(item)}}</button> &nbsp;&nbsp;' +
                  '</p>' +
                  '<div class="csRunMenuArea">'+
				  '<p class="csRunMenu" >' +
				  '<button ng-if="isRun"  ng-disabled="isRunning" ng-click="runCode();">{{buttonText}}</button>&nbsp&nbsp'+
				  '<button ng-if="isTest" ng-disabled="isRunning" ng-click="runTest();">Test</button>&nbsp&nbsp'+
				  '<span ng-if="isDocument"><a href="" ng-disabled="isRunning" ng-click="runDocument();">Document</a>&nbsp&nbsp</span>'+
				  '<a href="" ng-if="!attrs.nocode && (file || attrs.program)" ng-click="showCode();">{{showCodeLink}}</a>&nbsp&nbsp'+
				  '<a href="" ng-if="muokattu" ng-click="initCode();">{{resetText}}</a>' +
				  ' <a href="" ng-if="toggleEditor" ng-click="hideShowEditor();">{{toggleEditorText[noeditor?0:1]}}</a>' +
				  ' <a href="" ng-if="!noeditor" ng-click="showOtherEditor();">{{editorText[editorModeIndecies[editorMode+1]]}}</a>' +
                  '</p>'+
                  '</div>'+
                  (t=="sage" ? '<div class="outputSage no-popup-menu"></div>' :"")+ 

				  '<pre ng-if="viewCode && codeunder">{{code}}</pre>'+
				  (t === "comtest" || t === "tauno" || t === "parsons" ? '<p class="unitTestGreen"  ng-if="runTestGreen" >&nbsp;ok</p>' : "") +
				  (t === "comtest" || t === "tauno" || t === "parsons" ? '<pre class="unitTestRed"    ng-if="runTestRed">{{comtestError}}</pre>' : "") +
				  // '<p>{{resImage}}</p>'+
				  // '<p>Testi valituksesta</p>' +
				  '<pre class="csRunError" ng-if="runError">{{error}}</pre>'+
				  '<pre  class="console ng-hide" ng-show="result" ng-cloak>{{result}}</pre>'+
				  '<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="svgImageSnippet()"></span></div>'+
				  //'<div  class="userlist" tim-draggable-fixed style="top: 39px; right: 408px;"><span>Raahattava</span>'+
				  '<span  class="csrunPreview"></span>'+
                  //'</div>'+
                  // '<div class="previewcontent"></div>' +
                  
                  //'<div  class="htmlresult" ng-if="htmlresult" ><span ng-bind-html="htmlresult"></span></div>'+
				  //'<div  class="htmlresult" ng-if="htmlresult" >{{htmlresult}}</span></div>'+
                  
				  (t === "jypeli" || true ? '<img ng-if="imgURL" class="grconsole" ng-src="{{imgURL}}" alt=""  />' : "") +
				  // '<a ng-if="docURL" class="docurl" href="{{docURL}}" target="csdocument" >Go to document</a>' +
				  '<div ng-if="docURL" class="docurl">'+
				  '<p align="right" style="position: absolute; margin-left: 790px;"><a ng-click="closeDocument()" >X</a></p>' +
				  '<iframe width="800" height="600"  ng-src="{{docURL}}" target="csdocument" />' +
				  '</div>' +
				  //(t == "jypeli" ? '<img  class="grconsole" ng-src="{{imgURL}}" alt=""  ng-if="runSuccess"  />' : "") +
                  //  '<div class="userlist" tim-draggable-fixed="" style="top: 39px; right: 408px;">' +
                  //  'Raahattava' +
                  //  '</div>' +  
				  '<p class="plgfooter">Here comes footer</p>'+
                  //'<p ng-show="logTime()"></p>'+
				  '</div>';
};


csApp.set = function(scope,attrs,name,def) {
"use strict";
    scope[name] = def;
    if ( attrs && attrs[name] ) scope[name] = attrs[name];
    if ( scope.attrs && scope.attrs[name] ) scope[name] = scope.attrs[name];
    if ( scope[name] == "None" ) scope[name] = "";
    return scope[name];
};


csApp.getParam = function(scope,name,def) {
"use strict";
    var result = def;
    if ( scope.attrs && scope.attrs[name] ) result = scope.attrs[name];
    if ( scope[name] ) result = scope[name];
    if ( result == "None" ) scope[name] = "";
    return result;
}

csApp.directiveFunction = function(t,isInput) {
"use strict";
	return {
		link: function (scope, element, attrs) {
            if ( TESTWITHOUTPLUGINS ) return;

            scope.cursor = "\u0383"; //"\u0347"; // "\u02FD";
			scope.taunoHtml = element[0].childNodes[csApp.taunoPHIndex]; // Check this carefully, where is Tauno placeholder
            scope.plugin = element.parent().attr("data-plugin");
            scope.taskId  = element.parent().attr("id");
            scope.isFirst = true;
            if ( scope.$parent.$$prevSibling ) scope.isFirst = false;

			csApp.set(scope,attrs,"type","cs");
            scope.isText = languageTypes.getRunType(scope.type,false) == "text";
            scope.isSage = languageTypes.getRunType(scope.type,false) == "sage";
            scope.isSimcir = t === "simcir";
            
			csApp.set(scope,attrs,"file");
			csApp.set(scope,attrs,"filename");
			csApp.set(scope,attrs,"lang");
			csApp.set(scope,attrs,"width");
			csApp.set(scope,attrs,"height");
			csApp.set(scope,attrs,"table");
			csApp.set(scope,attrs,"variables");
			csApp.set(scope,attrs,"indices");
			csApp.set(scope,attrs,"replace");
			csApp.set(scope,attrs,"taunotype");
			csApp.set(scope,attrs,"stem");
			csApp.set(scope,attrs,"iframe",false);
			// csApp.set(scope,attrs,"usercode","");
			csApp.set(scope,attrs,"codeunder",false);
			csApp.set(scope,attrs,"codeover",false);
			csApp.set(scope,attrs,"open",!scope.isFirst);
			csApp.set(scope,attrs,"rows",1);
			csApp.set(scope,attrs,"maxrows",100);
			csApp.set(scope,attrs,"attrs.bycode");
			csApp.set(scope,attrs,"placeholder","Write your code here");
			csApp.set(scope,attrs,"inputplaceholder","Write your input here");
			csApp.set(scope,attrs,"argsplaceholder",scope.isText ? "Write file name here" : "Write your program args here");
			csApp.set(scope,attrs,"argsstem",scope.isText ? "File name:" : "Args:");
			csApp.set(scope,attrs,"userinput","");
			csApp.set(scope,attrs,"userargs",scope.isText ? scope.filename : "");
			csApp.set(scope,attrs,"inputstem","");
			csApp.set(scope,attrs,"inputrows",1);
			csApp.set(scope,attrs,"toggleEditor",scope.isSimcir ? "True" : false);
            csApp.set(scope,attrs,"indent",-1);
            csApp.set(scope,attrs,"user_id");
            csApp.set(scope,attrs,"isHtml",false);
            csApp.set(scope,attrs,"autoupdate",false);
            csApp.set(scope,attrs,"canvasWidth",700);
            csApp.set(scope,attrs,"canvasHeight",300);
            csApp.set(scope,attrs,"button","");
            csApp.set(scope,attrs,"noeditor",scope.isSimcir ? "True" : false);
            csApp.set(scope,attrs,"norun",false);
            csApp.set(scope,attrs,"normal","Normal");
            csApp.set(scope,attrs,"highlight","Highlight");
            csApp.set(scope,attrs,"parsons","Parsons");
            csApp.set(scope,attrs,"jsparsons","JS-Parsons");
            csApp.set(scope,attrs,"editorMode",0);
            csApp.set(scope,attrs,"showCodeOn","Näytä koko koodi");
            csApp.set(scope,attrs,"showCodeOff","Piilota muu koodi");
            csApp.set(scope,attrs,"resetText","Alusta");
            csApp.set(scope,attrs,"blind",false);
            csApp.set(scope,attrs,"words",false);
            csApp.set(scope,attrs,"editorModes","01");
            // csApp.set(scope,attrs,"program");

            scope.editorMode = parseInt(scope.editorMode);
            scope.editorText = [scope.normal,scope.highlight,scope.parsons,scope.jsparsons];
            scope.editorModeIndecies =[];
            for (var i=0; i<scope.editorModes.length; i++) scope.editorModeIndecies.push(parseInt(scope.editorModes[i]));
            scope.editorModeIndecies.push(parseInt(scope.editorModes[0]));
            if ( scope.editorModes.length <= 1 ) scope.editorText = ["","","","","","",""];
            
            scope.showCodeLink = scope.showCodeOn;
			scope.minRows = csApp.getInt(scope.rows);
			scope.maxRows = csApp.getInt(scope.maxrows);
            
            scope.toggleEditorText = ["Muokkaa","Piilota"];

            if ( scope.toggleEditor && scope.toggleEditor != "True" ) scope.toggleEditorText =  scope.toggleEditor.split("|");
            
			csApp.set(scope,attrs,"usercode","");
			if ( scope.usercode === "" && scope.byCode )  {
                scope.usercode = scope.byCode;
                scope.initUserCode = true;
            }    
			scope.usercode = csApp.commentTrim(scope.usercode);
			if (scope.blind) scope.usercode = scope.usercode.replace(/@author.*/,"@author XXXX"); // blind
			scope.byCode = csApp.commentTrim(scope.byCode);
			// scope.usercode = csApp.commentTrim(decodeURIComponent(escape(scope.usercode)));
			// scope.byCode = csApp.commentTrim(decodeURIComponent(escape(scope.byCode)));

            
            if ( scope.usercode ) {
                var rowCount = csApp.countChars(scope.usercode,'\n') + 1;
                if ( scope.maxRows < 0 && scope.maxRows < rowCount ) scope.maxRows = rowCount; 
            } else if ( scope.maxRows < 0 ) scope.maxRows = 10;
            
            scope.isRun = languageTypes.getRunType(scope.type,false) !== false && scope.norun == false;
            scope.isTest = languageTypes.getTestType(scope.type,false) !== false;
            scope.isDocument = (scope.type.indexOf("doc") >= 0);

            scope.showInput = (scope.type.indexOf("input") >= 0);
            scope.showArgs = (scope.type.indexOf("args") >= 0);
            scope.buttonText = "Aja";
            if ( scope.type.indexOf("text") >= 0 || scope.isSimcir) { // || scope.isSage ) {
                scope.isRun = true;
                scope.buttonText = "Tallenna";
            }            
            if ( scope.button ) {
                scope.isRun = true;
                scope.buttonText = scope.button;
            }
            
			scope.indent = csApp.getInt(scope.indent);
            if ( scope.indent < 0 )
                if ( scope.file ) scope.indent = 8; else scope.indent = 0;

            
			scope.edit = element.find("textarea")[0]; // angular.element(e); // $("#"+scope.editid);
			scope.preview = element.find(".csrunPreview")[0]; // angular.element(e); // $("#"+scope.editid);
            scope.element0 = element[0];
			element[0].childNodes[0].outerHTML = csApp.getHeading(attrs,"header",scope,"h4");
			var n = element[0].childNodes.length;
			if ( n > 1 ) element[0].childNodes[n-1].outerHTML = csApp.getHeading(attrs,"footer",scope,'p class="footer"');
            scope.muokattu = false; // scope.usercode != scope.byCode;
        //    scope.header = head;
		//	scope.getHeader = function() { return head; };
		//	csApp.updateEditSize(scope);
            if (scope.open && (t == "tauno" || t === "simcir") ) scope.showTauno();

            //attrs.buttons = "$hellobuttons$\nMunOhjelma\n$typebuttons$\n$charbuttons$";
            var b = attrs.buttons || scope.attrs.buttons;
            if ( b ) {
                var helloButtons = 'public \nclass \nHello \n\\n\n{\n}\n'+
                                    'static \nvoid \n Main\n(\n)\n' + 
                                    '        Console.WriteLine(\n"\nworld!\n;\n ';
                var typeButtons =  'bool \nchar\n int \ndouble \nstring \nStringBuilder \nPhyscisObject \n[] \nreturn \n, ';
                var charButtons = 'a\nb\nc\nd\ne\ni\nj\n.\n0\n1\n2\n3\n4\n5\nfalse\ntrue\nnull\n=';
                // var b = attrs.buttons;
                b = b.replace('$hellobuttons$', helloButtons);
                b = b.replace('$typebuttons$' , typeButtons);
                b = b.replace('$charbuttons$' , charButtons);
                b = b.trim();
                b = b.replace('$space$' , " ");
                scope.buttons = b.split("\n");
            }
            /*
            var editArea = element[0].getElementsByClassName('csEditArea');
            var editor = ace.edit(editArea);
            editor.setTheme("ace/theme/monokai");
            editor.getSession().setMode("ace/mode/javascript");
            */
            //scope.out = element[0].getElementsByClassName('console');
            if ( scope.attrs.autorun ) scope.runCodeLink();
            if ( scope.editorMode != 0 || scope.editorModes !== "01" ) scope.showOtherEditor(scope.editorMode);
            scope.mode = languageTypes.getAceModeType(scope.type,"");
            
            var styleArgs = csApp.getParam(scope,"style-args","");
            if ( styleArgs ) {
                var argsEdit = element[0].getElementsByClassName("csArgsArea"); // element.find("csArgsArea")[0]
                if ( argsEdit.length > 0  )  argsEdit[0].setAttribute('style',styleArgs);
            }
            
            
            csLogTime(scope.taskId);
            
            // if ( scope.isSage ) alustaSage(scope);
            
		},		
		scope: {},				 
		controller: csApp.Controller,
		restrict: 'AE',
		/*
		compile: function(tElement, attrs) {
			var content = tElement.children();
		},
		*/
		transclude: true,
		replace: 'true',
		template: csApp.directiveTemplateCS(t,isInput)
		// templateUrl: 'csTempl.html'
	}; 
};

function alustaSage(scope,firstTime) {
// TODO: lisää kentätkin vasta kun 1. kerran alustetaan.
// TODO: kielien valinnan tallentaminen
// TODO: kielien valinta kunnolla float.  
// ks: https://github.com/sagemath/sagecell/blob/master/doc/embedding.rst  
    var types = scope.type.split("/");
    var languages = sagecell.allLanguages;
    if ( types.length > 1 ) languages = types.slice(1); 
    if ( scope.sagecellInfo ) {
        var outputLocation = $(scope.sageOutput);
        outputLocation.find(".sagecell_output_elements").hide();
        scope.sagecellInfo.code = scope.usercode;
        return;
    }    
    scope.sageArea = scope.element0.getElementsByClassName('computeSage')[0];                    
    scope.editArea = scope.element0.getElementsByClassName('csEditArea')[0];                    
    scope.sageOutput = scope.element0.getElementsByClassName('outputSage')[0];                    
    
    scope.sagecellInfo = sagecell.makeSagecell({
        inputLocation: scope.sageArea,
        // inputLocation: scope.editArea,
        editor: "textarea",
        hide: ["editor","evalButton"],
        outputLocation: scope.sageOutput,
        requires_tos: false,
        code: scope.usercode,
        getCode: function() { return scope.getCode(); },
        autoeval: scope.attrs.autorun || firstTime,
        callback: function() {
            scope.sageButton = scope.sageArea.getElementsByClassName("sagecell_evalButton")[0]; // .button();     
            
            scope.sageButton.onclick = function() {
                // scope.checkSageSave();
            };
            var sagecellOptions = scope.element0.getElementsByClassName('sagecell_options')[0];  
            var csRunMenuArea = scope.element0.getElementsByClassName('csRunMenuArea')[0];
            if ( csRunMenuArea && sagecellOptions ) csRunMenuArea.appendChild(sagecellOptions);
            sagecellOptions.style.marginTop = "-2em";
        },
        languages: languages // sagecell.allLanguages
    });
}    

csApp.getInt = function(s) {
"use strict";
   var n = parseInt(s);
   if ( isNaN(n) ) return 0;
   return n;
};


csApp.countChars = function (s,c) {
"use strict";
	var n = 0;
	for (var i=0; i<s.length; n += +(c===s[i++]));
	return n;
};


csApp.updateEditSize = function(scope) {
"use strict";
//return;
    if ( !scope ) return;
	if ( !scope.usercode ) return;
    var n = csApp.countChars(scope.usercode,'\n') + 1;
	if ( n < scope.minRows ) n = scope.minRows;
	if ( n > scope.maxRows ) n = scope.maxRows;
	if ( n < 1 ) n = 1;
	scope.rows = n;
};

csApp.ifIs = function(value,name,def) {
"use strict";
	if ( !value && !def ) return "";
	if ( !value  ) return name+'="'+def+'" ';
	return name+'="'+value+'" ';
};
		
csApp.doVariables = function(v,name) {
"use strict";
	if ( !v ) return "";
	var r = "";
	var va = v.split(";");  
	for ( var n in va ) {
        if ( va.hasOwnProperty(n)) {
            var nv = va[n].trim();
            if (nv) {
                r += name + nv + "&";
            }
        }
	}
	return r.replace(/ /g,"");
};


csApp.Hex2Str = function (s) {
"use strict";
  var result = '';
  for (var i=0; i<s.length; i+=2) {
    var c = String.fromCharCode(parseInt(s[i]+s[i+1],16));
    result += c;
  }
  return result;
};


csApp.Controller = function($scope,$http,$transclude,$sce) {
"use strict";
    csLogTime("controller");
	$scope.byCode ="";
	$scope.attrs = {};
    $scope.svgImageSnippet = function() {
        var s = $sce.trustAsHtml($scope.htmlresult); 
        return s;
        // return $scope.htmlresult;
    };

	$transclude(function(clone, scope) {
        if ( TESTWITHOUTPLUGINS ) return;
		if ( !clone[0] ) return;
		var markJSON = "xxxJSONxxx";
		var markHex = "xxxHEXJSONxxx";
		var s = clone[0].textContent;
		var chex = s.indexOf(markHex) === 0;
		var cjson = s.indexOf(markJSON) === 0;
		if ( !chex && !cjson ) {
		    $scope.byCode = s;
		    return;
        }
        if ( cjson ) s = s.substring(markJSON.length);
        if ( chex ) s = csApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
	    $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
	});
	$scope.errors = [];
	$scope.taunoOn = false;
	// $scope.replace = "INSERT YOUR CODE HERE";
	// $scope.file = "https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs";
	$scope.result = "";
	$scope.htmlresult = "";
	$scope.resImage = "";
	$scope.imgURL = "";
	$scope.viewCode = false;
	$scope.runSuccess = false;
	$scope.copyingFromTauno = false;

	$scope.$watch('usercode', function() {
		if ( !$scope.copyingFromTauno && $scope.usercode !== $scope.byCode ) $scope.muokattu = true;
		$scope.copyingFromTauno = false;
		if ( $scope.minRows < $scope.maxRows ) 
			csApp.updateEditSize($scope);
		if ( $scope.viewCode ) $scope.pushShowCodeNow();
        window.clearInterval($scope.runTimer);
        if ( $scope.autoupdate ) $scope.runTimer = setInterval($scope.runCodeAuto,$scope.autoupdate);

		/* // tällä koodilla on se vika, että ei voi pyyhkiä alusta välilyönteä ja yhdistää rivejä
		if ( $scope.carretPos && $scope.carretPos >= 0 ) {
		    $scope.edit.selectionStart = $scope.carretPos;
		    $scope.edit.selectionEnd = $scope.carretPos;
		    $scope.carretPos = -1;
		}
		else $scope.checkIndent(); // ongelmia saada kursori paikalleen
		*/
	}, true);
    
    
	$scope.$watch('userargs', function() {
        window.clearInterval($scope.runTimer);
        if ( $scope.autoupdate ) $scope.runTimer = setInterval($scope.runCodeAuto,$scope.autoupdate);
	}, true);
    
	$scope.$watch('userinput', function() {
        window.clearInterval($scope.runTimer);
        if ( $scope.autoupdate ) $scope.runTimer = setInterval($scope.runCodeAuto,$scope.autoupdate);
	}, true);
    
    $scope.logTime = function(msg) {
        csLogTime(msg + " " + $scope.taskId);
        return true;
    }
    $scope.runCodeAuto = function() {
        window.clearInterval($scope.runTimer);
		var t = languageTypes.getRunType($scope.type,"cs");  
        if ( t == "md" ) { $scope.showMD(); return; }
        if ( t == "js" ) { $scope.showJS(); return; }
		$scope.doRunCode(t,true);
	};
	
	$scope.runCodeLink = function() {
		var t = languageTypes.getRunType($scope.type,"cs"); 
        if ( t == "md" ) { $scope.showMD() ; return; }
        if ( t == "js" ) { $scope.showJS(); return; }
		$scope.doRunCode(t,false);
	};
	
	$scope.runCode = function() {
		var t = languageTypes.getRunType($scope.type,"cs"); 
        if ( t == "md" ) { $scope.showMD(); } // return; }
        if ( t == "js" ) { $scope.showJS(); } // return; }
        // if ( $scope.sageButton ) $scope.sageButton.click();
		$scope.doRunCode(t,false);
	};
	
	$scope.runTest = function() {
		var t = languageTypes.getTestType($scope.type,"comtest"); 
		$scope.doRunCode(t,false);
	};
	
    
	$scope.runDocument = function() {
	    if ( $scope.docURL ) {
	        $scope.closeDocument();
	        return;
	    }
		var t = languageTypes.getRunType($scope.type,"cs");
		$scope.doRunCode(t, false, {"document": true});
	};


	$scope.closeDocument = function() {
		$scope.docURL = "";
	};


    $scope.hideShowEditor = function() {
        $scope.noeditor = !$scope.noeditor;
    };
    
    /*
    $scope.sageCode = function() {
        if ( !$scope.sagecellInfo ) return false;
        if ( !$scope.sagecellInfo.inputLocation ) return false;
        if ( !$scope.sagecellInfo.inputLocation.sagecell_session ) return false;
        if ( !$scope.sagecellInfo.inputLocation.sagecell_session.code ) return false;     
        return $scope.sagecellInfo.inputLocation.sagecell_session.code;
    }
    
	$scope.checkSageSave = function() {
        window.clearInterval($scope.sageTimer);
        // if ( $scope.autoupdate ) 
        $scope.sageTimer = setInterval(
           function() {
                var sg = $scope.sageCode();
                if ( !sg ) return;
                window.clearInterval($scope.sageTimer);
                if ( sg === $scope.usercode ) return; // Automatic does not save if not changed
                $scope.doRunCode("sage",false);
           },500);    
    }
    */
    

    
	$scope.doRunCode = function(runType, nosave, extraMarkUp) {
		// $scope.viewCode = false;
        window.clearInterval($scope.runTimer);
        $scope.closeDocument();
        // alert("moi");
        
        if ( $scope.sageButton ) $scope.sageButton.click();
        if ( $scope.isSage && !$scope.sagecellInfo ) alustaSage($scope,true);
        
        if ( $scope.simcir ) {
            $scope.usercode = $scope.getCircuitData();
        }
		else if ( $scope.taunoOn && ( !$scope.muokattu || !$scope.usercode ) ) $scope.copyTauno();
        
        if ( $scope.parson ) {
            var fb = $scope.parson.getFeedback();
            $scope.usercode = $scope.getJsParsonsCode();
        }
        
        if ( $scope.csparson ) {
            // var fb = $scope.parson.getFeedback();
            $scope.usercode = $scope.csparson.join("\n");
            $scope.csparson.check($scope.usercode);
        }
        
        // if ( runType == "md" ) { $scope.showMD(); return; }
		$scope.checkIndent();
		if ( !$scope.autoupdate ) {
            $scope.error = "... running ...";
            $scope.runError = true;
            $scope.isRunning = true;
        }
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		if ( runType != "js" ) $scope.result = "";
		$scope.runTestGreen = false;
		$scope.runTestRed = false;
        var isInput = false;
        if ( $scope.type.indexOf("input") >= 0 ) isInput = true;

        var ucode = "";
        var uinput = "";
        var uargs = "";
        if ( $scope.usercode ) ucode = $scope.usercode.replace($scope.cursor,"");
        if ( $scope.userinput ) uinput = $scope.userinput;
        if ( $scope.userargs ) uargs = $scope.userargs;
		var t = runType;
		// if ( t == "tauno" ) t = "comtest";

		// params = 'type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&replace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		var params = {
            //   'input': 1
			'input': {
			    'usercode':ucode,
			    'userinput':uinput,
			    'isInput' : isInput,
			    'userargs': uargs,
                // 'markup': {'type':t, 'file': $scope.file, 'replace': $scope.replace, 'lang': $scope.lang, 'taskId': $scope.taskId, 'user_id': $scope.user_id},
				'markup': { 'type': t, 'taskId': $scope.taskId, 'user_id': $scope.user_id }
			}
        };
		//		  alert($scope.usercode);
        if ( nosave ) params.input.nosave = true;
        if ( extraMarkUp ) jQuery.extend(params.input.markup, extraMarkUp);
        var url = "/cs/answer";
        // url = "http://tim-beta.it.jyu.fi/cs/answer";
        if ( $scope.plugin ) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if ( i > 0 ) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
		$http({method: 'PUT', url: url, data:params, headers: {'Content-Type': 'application/json'}, timeout: 20000 }
		).success(function(data, status, headers, config) {
			if ( data.web.error && false ) {
				$scope.error = data.web.error;
				//return;
			}
			$scope.error = data.web.error;
			var imgURL = "";
			$scope.runSuccess = true;
            $scope.isRunning = false;

			$scope.runError = $scope.error; // !$scope.runSuccess;

			imgURL = data.web.image;
			if ( data.web.testGreen ) $scope.runTestGreen = true;
			if ( data.web.testRed ) $scope.runTestRed = true;
			$scope.comtestError = data.web.comtestError;

			var docURL = data.web.docurl;

			if ( docURL ) {
				$scope.docURL = docURL;
				$scope.result = data.web.console.trim();
			}

			if ( imgURL ) {
				// $scope.resImage = '<img src="' + imgURL + ' " alt="Result image" />';
				$scope.imgURL = imgURL;
				$scope.result = data.web.console.trim();
			} else {
				if ( $scope.runSuccess )  
                    if ( $scope.isHtml )
                        $scope.htmlresult = removeXML(data.web.console);
                    else
                        if ( runType != "js" ) $scope.result = data.web.console;
				else   
				   $scope.error = data.web.error;
			}

		}).error(function(data, status) {
            $scope.isRunning = false;
			$scope.errors.push(status);
            $scope.error = "Ikuinen silmukka tai jokin muu vika?";
            // $scope.error = "TIMIssä ongelmia, odota vikaa tutkitaan...";
			// $scope.error = data;
		});
	};
	
	$scope.hideTauno = function() {
        if ( $scope.simcir ) {
           $scope.simcir.children().remove();
           $scope.simcir = null;
        }
		$scope.taunoOn = false;
		$scope.taunoHtml.innerHTML = "<p></p>";
	};

	
	$scope.copyTauno = function() {
		var f = document.getElementById($scope.taunoId);
		// var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
		var s = f.contentWindow.getUserCodeFromTauno();
		$scope.copyingFromTauno = true;
        var treplace = $scope.attrs.treplace || "";
        if ( treplace ) {
            var treps = treplace.split("&");
            for (var i = 0; i < treps.length; i++) {
                var reps = (treps[i]+"|").split("|");
                s = s.replace(new RegExp(reps[0],'g'),reps[1]);
                s = s.replace(new RegExp("\n\n",'g'),"\n");
            }
        }
		$scope.usercode = s;
        $scope.checkIndent();
		$scope.muokattu = false;
	};
	
	
	$scope.addText = function (s) {
	    // $scope.usercode += s;
        if ( $scope.noeditor ) {
            $scope.userargs += s + " ";
            return;
        }
	    var tbox = $scope.edit;
	    var i = tbox.selectionStart || 0;
	    var uc = $scope.usercode || "";
	    // $scope.usercode = uc.substring(0, i) + s.replace(/\\n/g,"\n") + uc.substring(i);
	    $scope.usercode = (uc + s.replace(/\\n/g,"\n")).replace($scope.cursor,"")+$scope.cursor;
	    // $scope.insertAtCursor(tbox, s);
	    //tbox.selectionStart += s.length;
	    //tbox.selectionEnd += s.length;
	};

	$scope.addTextHtml = function (s) {
	    var ret = s.trim();
	    if (ret.length === 0) ret = "\u00A0";
	    return ret;
	};


	$scope.insertAtCursor = function(myField, myValue) {
	    //IE support
	    if (document.selection) {
	        myField.focus();
	        var sel = document.selection.createRange();
	        sel.text = myValue;
	    }
	        //MOZILLA and others
	    else if (myField.selectionStart || myField.selectionStart === 0) {
	        var startPos = myField.selectionStart;
	        var endPos = myField.selectionEnd;
	        myField.value = myField.value.substring(0, startPos) +
                myValue +
                myField.value.substring(endPos, myField.value.length);
	    } else {
	        myField.value += myValue;
	    }
	};

    // Returns the visible index for next item and the desired size
    $scope.getVid = function() {
		csApp.taunoNr++;
		var vid = 'tauno'+csApp.taunoNr;
		$scope.taunoId = vid;
		var w = csApp.ifIs($scope.width,"width","100% ");
		var h = csApp.ifIs($scope.height,"height",500);
        return {vid:vid,w:w,h:h};
    }

    
    $scope.setCircuitData = function(data) {
        var data = {};
        $scope.runError = false;
        try {
            if ( $scope.usercode ) data = JSON.parse($scope.usercode);
        } catch (err ) {
            $scope.error = err.message;
            $scope.runError = true;
        }
        try {
            var initstr = csApp.getParam($scope,"initSimcir","");
            if ( initstr ) {
                var initdata = JSON.parse(initstr);
                $.extend(data,initdata);
            }    
        } catch (err ) {
            $scope.error = err.message;
            $scope.runError = true;
        }
        
        
        data.width = csApp.getParam($scope,"width",800);
        data.height = csApp.getParam($scope,"height",400);
        $scope.simcir.children().remove();
        simcir.setupSimcir($scope.simcir, data );
    }
    
    
    $scope.getCircuitData = function() {
        var data = simcir.controller($scope.simcir.find('.simcir-workspace'));
        data = data.data();
        
        var buf = '';
        var print = function(s) {
            buf += s;
        };
        var println = function(s) {
            print(s);
            buf += '\r\n';
        };
        var printArray = function(array) {
            $.each(array, function(i, item) {
              println('    ' + JSON.stringify(item) +
                  (i + 1 < array.length? ',' : '') );
            });
        };
        println('{');
        println('  "devices":[');
        printArray(data.devices);
        println('  ],');
        println('  "connectors":[');
        printArray(data.connectors);
        println('  ]');
        print('}');
        return buf;
        // return JSON.stringify(result);
    }
    
    
    $scope.copyToSimcir = function() {
        $scope.setCircuitData();
    }
    
    
    $scope.copyFromSimcir = function() {
        $scope.usercode = $scope.getCircuitData();
    }
    
    
    $scope.showSimcir = function() {
        var v = this.getVid();
        $scope.taunoOn = true;	
        $scope.taunoHtml.innerHTML = "<div id="+v.vid+"></div>";
        var jqTauno = $($scope.taunoHtml);
        $scope.simcir2 = $($scope.taunoHtml.innerHTML);
        $scope.simcir = $("#"+v.vid);
        $scope.simcir = jqTauno.find("#"+v.vid);
        $scope.setCircuitData();
        return true;
    }
    
    
	$scope.showTauno = function () {
        if ( $scope.isSimcir ) return $scope.showSimcir();
	    /* 
		csApp.taunoNr++;
		var vid = 'tauno'+csApp.taunoNr;
		$scope.taunoId = vid;
		var w = csApp.ifIs($scope.width,"width",800);
		var h = csApp.ifIs($scope.height,"height",500);
        */
        var v = this.getVid();
		var p = "";
		// var tt = "http://users.jyu.fi/~ji/js/tdbg/";
		// var tt = "http://tim-beta.it.jyu.fi/cs/ptauno/indexi.html";
		var tt = "/cs/tauno/index.html?";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://users.jyu.fi/~vesal/js/ptauno/index.html";
		// if ( $scope.taunotype && $scope.taunotype == "ptauno" ) tt = "http://tim-beta.it.jyu.fi/cs/ptauno/index.html";
		if ( $scope.taunotype && $scope.taunotype === "ptauno" ) tt = "/cs/tauno/index.html?s&";
		var taunoUrl = tt; // +"?"; // t=1,2,3,4,5,6&ma=4&mb=5&ialku=0&iloppu=5";
		var s = $scope.table;
		if ( s && s.length > 0) {
			if ( s[0] === 's' ) p = "ts="+s.substring(1) + "&"; // table it's size param "table: s10"
			else p = "t="+s.trim() + "&";                      // table by it's items
		}
		 
		p += csApp.doVariables($scope.variables,"m");
		p += csApp.doVariables($scope.indices,"i");
		
		taunoUrl = taunoUrl + p;
		$scope.iframe = true;
		if ( $scope.iframe )
			$scope.taunoHtml.innerHTML = 
			// '<p class="pluginHide"" ><a ng-click="hideTauno()">hide Tauno</a></p>' + // ng-click ei toimi..
			'<iframe id="'+v.vid+'" class="showTauno" src="' + taunoUrl + '" ' + v.w + v.h + ' ></iframe>';
			// youtube: <iframe width="480" height="385" src="//www.youtube.com/embed/RwmU0O7hXts" frameborder="0" allowfullscreen></iframe>
		else   
			$scope.taunoHtml.innerHTML = '<div class="taunoNaytto" id="'+v.vid+'" />';
		$scope.taunoOn = true;	
	};
	
	
	$scope.initCode = function() {
		$scope.muokattu = false;
		$scope.usercode = $scope.byCode;
		$scope.resImage = "";
		$scope.imgURL = "";
		$scope.runSuccess = false;
		$scope.runError = false;
		$scope.result = "";
		$scope.viewCode = false;
        if ( $scope.parson || $scope.editorModeIndecies[$scope.editorMode] > 1 ) {
            $scope.initUserCode = true;
            $scope.showOtherEditor($scope.editorMode);
        }
        if ( $scope.isSage ) alustaSage($scope);
        if ( $scope.simcir ) $scope.setCircuitData();
	};

	
	$scope.stopShow = function() {
		if (angular.isDefined($scope.stop)) {
			$interval.cancel($scope.stop);
			$scope.stop = undefined;
		}
	};
	
	$scope.pushShowCodeNow = function() {
		if ( !$scope.viewCode ) return;
		$scope.showCodeNow();
        return;
        /*
		$scope.stopShow();
		$scope.stop = $interval(function() {
			$scope.showCodeNow();
		}, 1000,1);
		*/
	};
	
	
	$scope.showCode = function() {
		$scope.viewCode = !$scope.viewCode;
        
        if ( $scope.viewCode ) 
            $scope.showCodeLink = $scope.showCodeOff;
        else
            $scope.showCodeLink = $scope.showCodeOn;

		$scope.localcode = undefined;
		$scope.showCodeNow();
	};
		
        
    $scope.checkIndent = function() {
        if ( !$scope.indent || !$scope.usercode ) return;
        var start = $scope.edit.selectionStart;
        var spaces = "";
        for (var j1=0; j1<$scope.indent; j1++) spaces += " ";
        var n = 0;
        var len = 0;
		var st = $scope.usercode.split("\n");
        for (var i in st) if ( st.hasOwnProperty(i) ) {
			var s = st[i];
			var l = s.length;
			len += l;
			var j = 0;
			for ( ; j<s.length; j++) if ( s[j] !== " " ) break;
			// if ( s.lastIndexOf(spaces,0) == 0 ) continue;
			if ( j >= spaces.length ) continue;
            if ( s.trim() === "" ) continue; // do not indent empty lines
			s = spaces + s.substring(j);
			var dl = s.length - l;
			if ( len - l < start ) start += dl;
			len += dl;
			st[i] = s;
			n++;
        }
        if ( !n ) return;
        $scope.usercode = st.join("\n");
        // $scope.edit[0].selectionStart = start; // aiheuttaa uuden tapahtuman
        $scope.carretPos = start; // seuraava tapahtuma nappaa tämän ja siirtää vain kursorin.
    };
		
	$scope.showCodeLocal = function() {
		// $scope.code = $scope.localcode;
		if ( $scope.localcode === "" ) { $scope.code = $scope.usercode; return; }
		var st = $scope.localcode.split("\n");
		var r = "";
		var rp = ["",""]; // alkuosa, loppuosa
		var step = 0;
		var nl = "";
		var nls = "";
		for (var i in st) if ( st.hasOwnProperty(i) ) {
			var s = st[i];
			if ( s.indexOf($scope.replace) >= 0 ) {
				r += nl + $scope.usercode;
				if ( step === 0 ) { step++; nls = ""; continue; }
			} else { 
				r += nl + s;
				rp[step] += nls + s;
			}
			nl = nls = "\n";
		}
		$scope.code = r;
		$scope.precode = rp[0];
		$scope.postcode = rp[1];
	};
		
		
	$scope.showCodeNow = function() {
	    // var copyEvent = new ClipboardEvent('copy', { dataType: 'text/plain', data: 'kissa' } );
        // document.dispatchEvent(copyEvent);
		if ( !$scope.viewCode ) return;
		if ( angular.isDefined($scope.localcode) ) { $scope.showCodeLocal(); return; } 
		if ( !$scope.file &&!$scope.attrs.program ) { $scope.localcode = ""; $scope.showCodeLocal(); return; }
		
		// params = 'print=1&type='+encodeURIComponent($scope.type)+'&file='+encodeURIComponent($scope.file)+ '&keplace='+ encodeURIComponent($scope.replace)+ '&by=' + encodeURIComponent($scope.usercode);
		var params = $scope.attrs;
		// $http({method: 'POST', url:"http://tim-beta.it.jyu.fi/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		$http({method: 'POST', url: '/cs/?print=1&replace=', data:params, headers: {'Content-Type': 'application/json'}}
		// $http({method: 'POST', url:"/cs/", data:params, headers: {'Content-Type': 'application/x-www-form-urlencoded'}}
		).success(function(data, status, headers, config) {
			if (data.msg !== '')
			{
				$scope.localcode = data;
				$scope.showCodeLocal();
			}
			else
			{
				$scope.errors.push(data.error);
			}
		}).error(function(data, status) {
			$scope.errors.push(status);
		});
	};
    
    $scope.lastMD = "";
    
	$scope.showMD = function() {
        if ( !$scope.usercode ) return;
        var text = $scope.usercode.replace($scope.cursor,"");
        if ( text == $scope.lastMD ) return;
        $scope.lastMD = text;
        $scope.previewUrl="/preview/" + $scope.taskId.split(".")[0]; // 12971"
        $http.post($scope.previewUrl, {
            "text": text
        }).success(function (data, status, headers, config) {
            var s  = "";
            var $previewDiv = angular.element($scope.preview); 
            
            if( typeof data.texts === 'string' ) {
                s = data.texts;
            } else {    
                var len = data.texts.length;
                for (var i = 0; i < len; i++) s += data.texts[i].html;
            }    
            $previewDiv.html(csApp.compile(s)($scope));

            //MathJax.Hub.Queue(["Typeset", MathJax.Hub, $previewDiv[0]]);
            // $scope.$parent.processAllMath($previewDiv[0]);
            if  ( $scope.$parent.processAllMath )            
                $scope.$parent.processAllMath($previewDiv); 
            //$scope.outofdate = false;
            //$scope.parCount = len;
        }).error(function (data, status, headers, config) {
            $window.alert("Failed to show preview: " + data.error);
        });
    };
    
    
    $scope.getJsParsonsCode = function() {
        /*
        var parson = $scope.parson;
        var elemId = parson.options.sortableId;
        var student_code = parson.normalizeIndents(parson.getModifiedCode("#ul-" + elemId));
        var result = "";
        
        // Find the line objects for the student's code
        for (i = 0; i < student_code.length; i++) {
            var line = parson.getLineById(student_code[i].id);
            result += line.code + "\n";
        }
        return result;
        */
        var gr = new ParsonsWidget._graders.VariableCheckGrader($scope.parson);
        var result = gr._codelinesAsString();
        var len = result.length;
        if ( result[len-1] === "\n" ) result = result.slice(0,-1);
        return result;
    }
    
    $scope.showJsParsons = function(parsonsEditDiv) {
        var v = $scope.parsonsId;
        if ( !v ) { 
            v = this.getVid();;
            $scope.parsonsId = v;
        }    
        parsonsEditDiv.setAttribute('id',v.vid);
        if ( $("#"+v.vid).length == 0 ) {         
           setTimeout(function() { 
             $scope.showJsParsons(parsonsEditDiv); } 
            , 300);
            console.log("wait 300 ms " + v.vid);
            return;
        }   
        var classes = "csrunEditorDiv sortable-code";
        var can_indent = true;
        if ( $scope.words ) {
            classes += " jssortable-words";
            can_indent = false;
            var a = $("#"+v.vid);
        }
        parsonsEditDiv.setAttribute('class',classes);
        // parsonsEditDiv.setAttribute('style',"float: none;");

        
        // parsonsEditDiv.innerHTML = "";
        $scope.parson = new ParsonsWidget({
                'sortableId': v.vid,
                'max_wrong_lines': 1,
                //'prettyPrint': false,
                //'x_indent': 0,
                'can_indent' : can_indent,
                //'vartests': [{initcode: "output = ''", code: "", message: "Testing...", variables: {output: "I am a Java program I am a Java program I am a Java program "}},
                //    ],
                //'grader': ParsonsWidget._graders.LanguageTranslationGrader,
            });
        // $scope.parson.init($scope.byCode,$scope.usercode);
        $scope.parson.init($scope.usercode);
        if ( !$scope.initUserCode )
            $scope.parson.options.permutation = iotaPermutation; 
        $scope.parson.shuffleLines();
    }
    
    $scope.showCsParsons = function(sortable) {
        var parson = new CsParsonsWidget({
            'sortable': sortable,
            'trashId': 'sortableTrash',
            'words': $scope.words,
            'minWidth': "40px",
            'shuffle' : $scope.initUserCode,
            'styleWords' : csApp.getParam($scope,"style-words",""),
            'onChange': function(p) {
                var s = p.join("\n");
                $scope.usercode = s; 
                }
            });
        parson.init($scope.byCode,$scope.usercode);
        parson.show();
        $scope.csparson = parson;
    }


    
    $scope.showOtherEditor = function(editorMode) {
        if ( $scope.parson ) {
            $scope.usercode = $scope.getJsParsonsCode();
        }
        $scope.parson = null;
        $scope.csparson = null;
        
        var editorHtml = '<textarea class="csRunArea csrunEditorDiv" ng-hide="noeditor" rows={{rows}} ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></textarea>';

        var aceHtml = '<div class="no-popup-menu"><div ng-show="mode" ui-ace="{onLoad:aceLoaded,  mode: \'{{mode}}\', require: [\'ace/ext/language_tools\'],  advanced: {enableSnippets: true,enableBasicAutocompletion: true,enableLiveAutocompletion: true}}"'+
        // var aceHtml = '<div ng-show="mode" ui-ace="{  mode: \'{{mode}}\',    require: [\'/static/scripts/bower_components/ace-builds/src-min-noconflict/ext-language_tools.js\'],  advanced: {enableSnippets: true,enableBasicAutocompletion: true,enableLiveAutocompletion: true}}"'+
                   // ' style="left:-6em; height:{{rows*1.17}}em;" class="csRunArea csEditArea" ng-hide="noeditor"  ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></div>'+
                   ' style="left:-1em; width: 105% !important;" class="csRunArea csEditArea" ng-hide="noeditor"  ng-model="usercode" ng-trim="false" placeholder="{{placeholder}}"></div>'+
                   /*
                   '<div style="right:0px;">'+
                   '<button ng-click="moveCursor(-1, 0);">&#x21d0;</button>'+
                   '<button ng-click="moveCursor( 0,-1);">&#x21d1;</button>'+
                   '<button ng-click="moveCursor( 1, 0);">&#x21d2;</button>'+
                   '<button ng-click="moveCursor( 0, 1);">&#x21d3;</button>'+
                   '</div>'+
                   */
                   '</div>';
        var parsonsHtml = '<div class="no-popup-menu"></div>';
        var html = [editorHtml,aceHtml,parsonsHtml,parsonsHtml];                    
        $scope.mode = languageTypes.getAceModeType($scope.type,"");
        if (typeof editorMode !== 'undefined') $scope.editorMode = editorMode;
        else $scope.editorMode++; 
        if ( $scope.editorMode >= $scope.editorModeIndecies.length-1 ) $scope.editorMode = 0; 
        var eindex = $scope.editorModeIndecies[$scope.editorMode];
        
        var otherEditDiv = $scope.element0.getElementsByClassName('csrunEditorDiv')[0];                    
        var editorDiv = angular.element(otherEditDiv); 
        editorDiv.html(csApp.compile(html[eindex])($scope));
        if ( eindex == 1 ) {
            $scope.aceEditor.setFontSize(15);
            $scope.aceEditor.setOptions({
                maxLines: $scope.maxRows
            });
            $scope.aceEditor.renderer.setScrollMargin(12, 12, 0, 0);
        }
        if ( eindex == 2 ) $scope.showCsParsons(otherEditDiv.children[0]);
        if ( eindex == 3 ) $scope.showJsParsons(otherEditDiv.children[0]);
    };
    
    $scope.moveCursor = function(dx,dy) {
        var p = $scope.aceEditor.getCursorPosition();
        p.row +=  dy;
        p.column += dx;
        $scope.aceEditor.moveCursorToPosition(p);
        
    }
    
     // Runs when editor loads
    $scope.aceLoaded = function(editor){
      $scope.aceEditor = editor;
      console.log('Ace editor loaded successfully');
      var session = editor.getSession();
      session.setUndoManager(new ace.UndoManager());
      // Editor Events
      // _session.on("change", function(){
      //   console.log('[EditorCtrl] Session changed:', _session);
      // });
      // editor.renderer.setShowGutter(false);
    };   
    
    $scope.write = function(s) {
       $scope.result += s;
    }    
      
    $scope.writeln = function(s) {
       $scope.write(s+"\n");
    }    
    
    $scope.canvasConsole = {};
    $scope.canvasConsole.log = function(s) {
        var res = "", sep = "";
        for (var i=0; i<arguments.length; i++) { res += sep + arguments[i]; sep = " "; }    
        $scope.writeln(res);
    };

    
    
    $scope.toggleFixed = function() {
        if ( $scope.canvas.style["position"] == "fixed" ) {
            $scope.canvas.style["position"] = ""
            $scope.irrotaKiinnita = "Irrota";
        } else {
            $scope.canvas.style["position"] = "fixed"        
            $scope.canvas.style["width"] = 900;
            $scope.irrotaKiinnita = "Kiinnitä";
        }
    }
    
    $scope.getCode = function() {
        if ( $scope.attrs.program && !$scope.codeInitialized ) {
            $scope.localcode = $scope.attrs.program;
            $scope.showCodeLocal();
        }
        $scope.codeInitialized = true;
        var text = $scope.usercode.replace($scope.cursor,"");
        if ( $scope.precode || $scope.postcode ) {
        	text = $scope.precode + "\n" + text + "\n" + $scope.postcode;
        }
        return text;
    }
    
    
   $scope.lastJS = "";
    $scope.iframeClientHeight = -1;
	$scope.showJS = function() {
        var wantsConsole = false;
        if ( $scope.type.indexOf("/c") >= 0 ) wantsConsole = true;
        if ( !$scope.usercode && !$scope.userargs && !$scope.userinput ) return;
        if ( !$scope.canvas ) { // cerate a canvas on first time
            if ( $scope.iframe ) {
                var v = $scope.getVid();
                $scope.irrotaKiinnita = "Irrota";
                $scope.canvas = angular.element('<div tim-draggable-fixed class="no-popup-menu" style="top: 91px; right: 0px;" >'+
                  '<span class="csRunMenu"><div><a href ng-click="toggleFixed()" >{{irrotaKiinnita}}</a></div></span>'+
                  '<iframe id="'+v.vid+'" class="jsCanvas" src="/cs/gethtml/canvas.html?scripts='+($scope.attrs.scripts||"") + '" ' + v.w + v.h + ' style="border:0" seamless="seamless" sandbox="allow-scripts allow-same-origin"></iframe>'+
                  '</div>');
                // $scope.canvas = angular.element('<iframe id="'+v.vid+'" class="jsCanvas" src="/cs/gethtml/canvas.html" ' + v.w + v.h + ' style="border:0" seamless="seamless" ></iframe>');
                $scope.iframeLoadTries = 10;
            } else {  
                    $scope.canvas = angular.element(//'<div class="userlist" tim-draggable-fixed="" style="top: 91px; right: -375px;">'+
              '<canvas id="csCanvas" width="'+$scope.canvasWidth+'" height="'+$scope.canvasHeight+'" class="jsCanvas"></canvas>' +
              // '<canvas id="csCanvas" width="'+$scope.canvasWidth+'" height="'+$scope.canvasHeight+'" class="jsCanvas userlist" tim-draggable-fixed="" style="top: 91px; right: -375px;"></canvas>' +
              ''); // '</div>'); 
            }
            var $previewDiv = angular.element($scope.preview);
            //$previewDiv.html($scope.canvas);
            $previewDiv.html($scope.previewIFrame = csApp.compile($scope.canvas)($scope));
            //$scope.canvas = $scope.preview.find(".csCanvas")[0];
            $scope.canvas = $scope.canvas[0];
        }
        var text = $scope.usercode.replace($scope.cursor,"");
        if ( text == $scope.lastJS && $scope.userargs == $scope.lastUserargs && $scope.userinput == $scope.lastUserinput ) return;
        $scope.lastJS = text;
        $scope.lastUserargs = $scope.userargs;
        $scope.lastUserinput = $scope.userinput;
        
        text = $scope.getCode();
        
        if ( $scope.iframe ) { // in case of iframe, the text is send to iframe
            var f =  document.getElementById($scope.taunoId); // but on first time it might be not loaded yet
            // var s = $scope.taunoHtml.contentWindow().getUserCodeFromTauno();
            if ( !f || !f.contentWindow || !f.contentWindow.runJavaScript ) {
               $scope.lastJS = ""; 
               $scope.lastUserargs = "";
               $scope.lastUserinput = "";
               $scope.iframeLoadTries--;
               if ( $scope.iframeLoadTries <= 0 ) return;
               setTimeout($scope.showJS, 300);
               console.log("Odotetaan 300 ms");
               return;
            }
            if ( $scope.iframeClientHeight < 0 ) $scope.iframeClientHeight = f.clientHeight;
            var s = f.contentWindow.runJavaScript(text,$scope.userargs,$scope.userinput, wantsConsole);
            var ch = f.contentWindow.getConsoleHeight();
            if ( ch < $scope.iframeClientHeight ) ch = $scope.iframeClientHeight;
            f.height = "";
            f.height = ""+ch+"px";

            return;
        }
        $scope.error = "";
        $scope.runError = false;
        try {
            var ctx = $scope.canvas.getContext("2d");
            ctx.save();
            $scope.result = "";
            var beforeCode = 'function paint(ctx,out, userargs, userinput, console) {"use strict"; ';
            var afterCode = '\n}\n';
            var a = "";
            var b = "";
            var cons = console;
            if ( wantsConsole ) {
               b = beforeCode;
               a = afterCode; 
               cons = $scope.canvasConsole;
            }

            var paint = new Function("return ("+b+text+a+")")(); 
            if ( !$scope.out ) {
               $scope.out = $scope.element0.getElementsByClassName('console')[0]; 
               $scope.out.write = $scope.write;
               $scope.out.writeln = $scope.writeln;
               $scope.out.canvas = $scope.canvas;
            }   
            paint(ctx,$scope.out,$scope.userargs,$scope.userinput,cons);
            ctx.restore();
       } catch (exc) {
          var rivi ='';
          var sarake = '';
          if (exc.column) sarake = ' Col '+exc.column.toString()+': ';
          if (exc.line) rivi = 'Row '+exc.line+': ';                  // FF has line
          else if (exc.lineNumber) rivi = 'Row '+exc.lineNumber+': '; // Safari has lineNUmber
          $scope.error = rivi+sarake+exc.toString();
          $scope.runError = $scope.error;
       }
       $scope.safeApply();
    };
    
    $scope.safeApply = function(fn) {
        var phase = this.$root.$$phase;
        if(phase == '$apply' || phase == '$digest') {
        if(fn && (typeof(fn) === 'function')) {
          fn();
        }
        } else {
        this.$apply(fn);
    }
    
      

};

};

     /* Heh, lisätäänpä fillCircle kontekstiin :) */
     Object.getPrototypeOf(document.createElement('canvas').getContext('2d')).fillCircle =
       function (x,y,r) {
         this.beginPath();
         this.arc(x, y, r, 0, Math.PI*2, false);
         this.closePath();
         this.fill();
         this.stroke();
       };

var csConsoleApp = angular.module('csConsoleApp', ['ngSanitize']);
// csApp.directive('csRunner', ['$sanitize', function ($sanitize) { csApp.sanitize = $sanitize; return csApp.directiveFunction('console', false); }]);
csConsoleApp.directive('csConsole', ['$sanitize', '$timeout', function ($sanitize, $timeout) { csConsoleApp.sanitize = $sanitize; return csConsoleApp.directiveFunction('shell', true, $timeout); }]);

csConsoleApp.directiveFunction = function (t, isInput, $timeout) {
"use strict";

// csConsoleApp.directive('csConsole', function ($timeout) {
     return {
         restrict: 'E',
         controller: csConsoleApp.Controller,
         scope: {},
         template: csConsoleApp.directiveTemplateCS(t, isInput),
         // templateUrl: function (elem, attrs) {
         //    return "http://" + elem.parent().attr('data-plugin') + "/" + 'NewConsole/Console.template.html';
         // },
         transclude: true,
         replace: true,
         link: function (scope, element, attrs) {
			csApp.set(scope,attrs,"usercode","");
  			csApp.set(scope,attrs,"type","cs");
  			csApp.set(scope,attrs,"path");
            scope.pwd = ConsolePWD.getPWD(scope);
            scope.oldpwd = scope.pwd;
            scope.isShell = languageTypes.getRunType(scope.type,false) == "shell";
  			if ( scope.usercode === "" && scope.byCode )  scope.usercode = scope.byCode.split("\n")[0];
            scope.currentInput = scope.usercode;
            if ( scope.isShell ) ConsolePWD.register(scope);
         }
     };
};


// controller: function($scope,$http,$transclude,$element, $sce ) {
csConsoleApp.Controller = function ($scope, $http, $transclude, $element, $timeout) {
"use strict";
    $scope.attrs = {};

    $transclude(function(clone, scope) {
        if ( TESTWITHOUTPLUGINS ) return;
        if ( !clone[0] ) return;
        var markJSON = "xxxJSONxxx";
        var markHex = "xxxHEXJSONxxx";
        var s = clone[0].textContent;
        var chex = s.indexOf(markHex) === 0;
        var cjson = s.indexOf(markJSON) === 0;
        if ( !chex && !cjson ) {
            $scope.byCode = s;
            return;
        }
        if ( cjson ) s = s.substring(markJSON.length);
        if ( chex ) s = csApp.Hex2Str(s.substring(markHex.length));
        $scope.attrs = JSON.parse(s);
        $scope.byCode = $scope.attrs.by || $scope.attrs.byCode;
    });
    //controller: function ($scope, $element, $sce, $http) {
    // This block could be re-used
    $scope.taskId = $element.parent().attr("id");
    $scope.plugin = $element.parent().attr("data-plugin");
    var reqPath = $scope.plugin + "/" + $scope.ident + "/";
    // $scope.content = JSON.parse($element.attr("data-content"));
    $scope.content = $scope.attrs;
    // End of generally re-usable TIM stuff

    $scope.examples = "";

    if ( $scope.content.examples) {
        var s = $scope.content.examples.replace(/'/g, '"');
        $scope.examples = JSON.parse(s);
    }

    
    $scope.setPWD = function(pwd) {
        $scope.pwd = pwd;
    }
    
    
    $scope.history = [];

    $scope.loadExample = function (i) {
        $scope.currentInput = $scope.examples[i].expr;
        $scope.focusOnInput();
    };
    $scope.focusOnInput = function () {
        var el = $element[0].querySelector(".console-input");
        el.focus();

    };
    $scope.handler = function (e) {
        var url = "/cs/answer";
        if ($scope.plugin) {
            // url = "/csPlugin" + /*$scope.plugin + */ "/" + $scope.taskId + "/answer/"; // Häck to get same origin
            url = $scope.plugin;
            var i = url.lastIndexOf("/");
            if (i > 0) url = url.substring(i);
            url += "/" + $scope.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }
        var t = languageTypes.getRunType($scope.content.type, "shell");
        var ucode = $scope.currentInput;
        var isInput = false;
        var uargs = "";
        var uinput = "";

        $http({
                method: 'PUT',
                url: url,
                data: {
                  'input': {
                      'usercode': ucode, 'userinput': uinput, 'isInput': isInput, 'userargs': uargs,
                      // "input": $scope.currentInput,
                      'markup': { 'type': t, 'taskId': $scope.taskId, 'user_id': $scope.content.user_id }
                  }
              }
        })
         .success(function (data) {
             var s = "";
             $scope.oldpwd = $scope.pwd;
             if ( data.web.pwd ) ConsolePWD.setPWD(data.web.pwd,$scope);
             if (data.web.error ) {
                 s = data.web.error;
                 s = "<pre>" + s + "</pre>";
             } else {
                 s = data.web.console
                 if (!$scope.isHtml) s = "<pre>" + s + "</pre>";
             }
             $scope.submit(s);
             // console.log(["data", data.web]);
         })
         .error(function (data, status, hdrs, cfg) {
             console.log(["protocol error", data, status, hdrs, cfg]);
             $scope.submit("Endless loop?");
         });

    };
    $scope.currentSize = "normal";
    $scope.currentInput = "";
    $scope.cursor = $scope.history.length; // $scope.history.length means new input is last command.

    $scope.toggleSize = function () {
        if ($scope.currentSize === "normal")
        { $scope.currentSize = "enlarged"; }
        else { $scope.currentSize = "normal"; }
    };

    $scope.submit = function (result) {

        $scope.history.push({
            istem: $scope.isShell ? $scope.history.length + " " + $scope.oldpwd + "$": "in_"+ $scope.history.length+": ",
            ostem: $scope.isShell ?  "" : "out_"+ $scope.history.length+": ",
            input: $scope.currentInput,
            response: result
        });
        $scope.currentInput = ""; $scope.cursor = $scope.history.length;
        $timeout(function () {
            var el = $element[0].querySelector(".console-output");
            el.scrollTop = el.scrollHeight;
        });
    };

    $scope.load = function () {
        if ($scope.cursor >= $scope.history.length)
        { $scope.currentInput = ""; $scope.cursor = $scope.history.length; return; }
        var norm = Math.min($scope.history.length - 1, Math.max(0, $scope.cursor));
        $scope.currentInput = $scope.history[norm].input;
        $scope.cursor = norm;
    };


    $scope.up = function() {
        if ( !$scope.cursor ) return;
        $scope.cursor--; $scope.load();
    }
    
    $scope.down = function() {
        $scope.cursor++; $scope.load();
    }
    
    $scope.handleKey = function (ev) {
        if (ev.which === 13) $scope.handler();// submit();
        if (ev.which === 40) { $scope.down(); }
        if (ev.which === 38) { $scope.up(); }
    };
};
 

csConsoleApp.directiveTemplateCS = function (t, isInput) {
"use strict";
    if (TESTWITHOUTPLUGINS) return '';
    return '<div class="web-console no-popup-menu {{currentSize}} "'+
     '    ng-keydown="handleKey($event)" >'+
     ''+
     '<code class="console-output">'+
     ' <div class="console-output-elem" '+
     '    ng-repeat="item in history track by $index">'+
     '<span class="console-oldinput">'+
     '  <span class="console-in">{{item.istem}}</span>'+
     '  <span class="console-userInput">{{item.input}}</span>'+
     ' </span>'+
     ' <span  class="console-oldresponse">'+
     '<span ng_if="!isShell">'+
     '  <br />' +
     '  <span class="console-out">{{item.ostem}}</span>'+
     '</span>'+
     '  <span class="console-response" ng-class="{error:item.error}"><span ng-bind-html="item.response"></span></span> <!-- Double span since ng-bind eats the innermost one --!>'+
     ' </span>'+
     '</div>'+
     '<span class="console-expander-sym"'+
     '    ng-click="toggleSize()"></span>'+
     '</code>'+
     '<div class="console-examples-box">'+
     '    <span class="examples-title"'+
     '    ng-click="examplesVisible=!examplesVisible" >'+
     '    ▼ example expressions ▲</span>' +
     '<div>Click to load:</div>'+
     '<ul >'+
     '    <li ng-repeat="example in examples track by $index">'+
     '    <a ng-click="loadExample($index)" title="{{example.expr}}">{{example.title||example.expr}}</a>' +
     '    </li>'+
     '<ul>'+
     '</div>'+
     ''+
     '<div class="console-curIndex" ng-if="isShell">{{pwd}}</div>'+
     '<span class="console-curIndex">in_{{cursor}}</span>'+
     '<input type="text" '+
     '    placeholder="type expressions here"'+
     '        class="console-input"'+
     '        ng-model="currentInput" />&nbsp;'+
     '<div class="console-buttons">'+
     '<button ng-click="up()">↑</button>&nbsp;' +
     '<button ng-click="down()">↓</button>&nbsp;' +
	 '<button ng-click="handler()">Enter</button>&nbsp;'+
     '</div>'+
     '</div>';
};
 
 
function truthTable(sentence,topbottomLines) {
"use strict";  
  var result = "";
  try {
      if ( !sentence ) return "";
      if ( !sentence.trim() ) return "";
      var replace = "v ||;^ &&;~ !;∧ &&;∨ ||;∼ !;xor ^;and &&;not !;or ||;ja &&;ei !;tai ||"; 
      var abcde = "abcdefghijklmnopqrstuxy";
      var header = "";
      var vals = '""';
      var count = 0;
      var cnt2 = 1;
      
      var input = sentence.toLowerCase();

      var repls = replace.split(";");
      for (i in repls) {
          var r = repls[i].split(" ");
          input = input.split(r[0]).join(r[1]);
      }

      for (var i=0; i<abcde.length; i++) {
          if ( input.indexOf(abcde[i]) >= 0 ) {
              header += abcde[i] + " ";
              var zv = "z["+count+"]";
              input = input.split(abcde[i]).join(zv);
              vals += '+' + zv + '+" "';
              count++;
              cnt2 *= 2;
          } 
      }
      
      
      var sents = sentence.split(";");
      var lens = [];
      var fills = [];
      for (i=0; i<sents.length; i++) {
          sents[i] = sents[i].trim();
          lens[i] = sents[i].length;
          fills[i] = "                                                               ".substring(0,lens[i]);
      }
      header += "  " + sents.join("  ");
      var line = "---------------------------------------".substring(0,header.length);
      // result += input + "\n";
      if ( topbottomLines )  result += line + "\n";
      result += header + "\n";
      result += line + "\n";
      for (var n = 0; n < cnt2; n++ ) {
          var z = [];
          for (i=0; i < count; i++)
                z[i] = (n >> (count-1-i)) & 1;
          result += eval(vals) + "= ";  
          var inp = input.split(";");
          for (i=0; i<inp.length; i++) {
            var tulos = " " + (eval(inp[i]) ? 1 : 0) + fills[i];
            result += tulos;
          }  
          result += "\n";
      } 
      if ( topbottomLines )  result += line + "\n";
      return result;
  } catch (err) {
      return result + "\n" + err + "\n";
  }
}  