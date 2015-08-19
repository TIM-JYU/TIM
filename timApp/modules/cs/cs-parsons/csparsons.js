(function($) { // wrap in anonymous function to not show some helper variables
    var CsParsonsWidget = function(options) {
        var defaults = {
            separator : "\n",
            styleWords: ""
        };
     
        this.options = jQuery.extend({}, defaults, options);
    };

    
    CsParsonsWidget.prototype.shuffle = function(lines) {
        var result = lines.slice();
        var n = lines.length;
        for (var i = n-1; i >= 0; i--) {
            var j = Math.floor(Math.random() * (i+1));
            var tmp = result[i];
            result[i] = result[j];
            result[j] = tmp;
        }
        return result;
    };
    
    
    CsParsonsWidget.prototype.init = function(text,userText) {
        this.text = text;
        if ( this.options.shuffle ) {
            this.lines =  text.split("\n");
            this.lines = this.shuffle(this.lines);
        }
        else 
            this.lines =  userText.split("\n");
    };

    CsParsonsWidget.prototype.show = function() {
        var classes = "sortable-code sortable-output";
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
            var typ=type;
            var w = words[i];
            if ( this.options.words && w === "" ) {
                div = document.createElement("div");
                typ = div;
                w = "\\n";
            } else  {  
                div = document.createElement(type);
                if ( this.options.words && this.options.minWidth && w.length < 3 )
                    div.setAttribute('style',"width: " + this.options.minWidth );
            }     
            div.setAttribute('class',"sortitem");
            
            var t = document.createTextNode(w);
            t.title = w;
            div.appendChild(t);
            //var div2 = document.createElement(typ);
            //div2.appendChild(div);
            parsonsEditDiv.appendChild(div);
        }    
        parsonsEditDiv.setAttribute('class',classes);
        parsonsEditDiv.setAttribute('style',"float: left; width: 100%" + ";" + this.options.styleWords);
        a = $(parsonsEditDiv);
        a.sortable();
        var parson = this;
        // a.sortable( "option", "axis", "x" );
        a.on( "sortstop", function( event, ui ) { 
             if ( parson.options.onChange ) parson.options.onChange(parson);
             parson.clear();
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
            var line = node.textContent;
            if ( line === "\\n" ) { line = "\n"; sep = ""; nosep = true; }
            result += sep + line;
            sep = separator;
            if ( nosep ) sep = "";
            nosep = false;
        }
        
        return result;
    }

    CsParsonsWidget.prototype.check = function(userText) {
        var result = "";
        var div = this.options.sortable;
        var lines = this.text.split("\n");
        var ulines = userText.split("\n");
        if ( !div ) return "";
        for (i = 0; i < div.childElementCount; i++) {
            var node = div.childNodes[i];
            if ( lines[i] !== ulines[i] ) {
                node.setAttribute('style',"background-color: RED;");
            }
        }
        
        return result;
    }

    CsParsonsWidget.prototype.clear = function() {
        var div = this.options.sortable;
        if ( !div ) return "";
        for (i = 0; i < div.childElementCount; i++) {
            var node = div.childNodes[i];
            node.removeAttribute('style');
        }
    }

   
   
   
    window['CsParsonsWidget'] = CsParsonsWidget;
 }
// allows _ and $ to be modified with noconflict without changing the globals
// that parsons uses
)($);
