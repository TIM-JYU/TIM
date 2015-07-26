(function($) { // wrap in anonymous function to not show some helper variables
   var CsParsonsWidget = function(options) {
        var defaults = {
            separator : "\n"
        };
     
        this.options = jQuery.extend({}, defaults, options);
   };

   CsParsonsWidget.prototype.init = function(text) {
       this.lines =  text.split("\n");
   };

   CsParsonsWidget.prototype.show = function() {
        var classes = "csrunEditorDiv sortable-code sortable-output";
        var parsonsEditDiv = this.options.sortable;
        var type = "div";
        if ( this.options.words ) {
            classes += " sortable-words ";
            this.options.separator = " ";
            type = "span";
        }    
        var words = this.lines
        parsonsEditDiv.innerHTML = "";
        for (var i = 0; i < words.length; i++) {
            var div;
            var w = words[i];
            if ( this.options.words && w === "" ) {
                div = document.createElement("div");
                w = "\\n";
            } else    
                div = document.createElement(type);
            var t = document.createTextNode(w);
            t.title = w;
            div.appendChild(t);
            parsonsEditDiv.appendChild(div);
        }    
        parsonsEditDiv.setAttribute('class',classes);
        parsonsEditDiv.setAttribute('style',"float: none;");
        a = $(parsonsEditDiv);
        a.sortable();
        var parson = this;
        // a.sortable( "option", "axis", "x" );
        a.on( "sortstop", function( event, ui ) { 
             if ( parson.options.onChange ) parson.options.onChange(parson);
          } );
   };    

   CsParsonsWidget.prototype.join = function(separator) {
        var result = "";
        var sep = "";
        var div = this.options.sortable;
        var nosep = false;
        if ( !div ) return "";
        for (i = 0; i < div.childElementCount; i++) {
            var node = div.childNodes[i];
            var line = node.innerText;
            if ( line === "\\n" ) { line = "\n"; sep = ""; nosep = true; }
            result += sep + line;
            sep = separator;
            if ( nosep ) sep = "";
            nosep = false;
        }
        
        return result;
   }

   
   window['CsParsonsWidget'] = CsParsonsWidget;
 }
// allows _ and $ to be modified with noconflict without changing the globals
// that parsons uses
)($);
