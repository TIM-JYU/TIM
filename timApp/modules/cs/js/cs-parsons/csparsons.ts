import $ from "jquery";
import "./jquery.ui.touch-punch.min.js";
import "./jquery-ui-sortable.min.js";

type CsParsonsOptions = {
    shuffle: boolean,
    sortable: Element,
    notordermatters: boolean,
    words: boolean,
    maxcheck: number,
    separator?: string,
    minWidth: string,
    styleWords: string,
    onChange?(widget: CsParsonsWidget);
};

export class CsParsonsWidget {
    options: CsParsonsOptions;
    text: string;
    lines: string[];

    constructor(options: CsParsonsOptions) {
        var defaults = {
            separator : "\n",
            styleWords: ""
        };

        this.options = $.extend({}, defaults, options);
    };

    shuffle(lines) {
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

    init(text,userText) {
        this.text = text;
        if ( this.options.shuffle ) {
            this.lines =  text.split("\n");
            this.lines = this.shuffle(this.lines);
        }
        else
            this.lines =  userText.split("\n");
    };

    show() {
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

            var t = document.createTextNode(w) as any;
            t.title = w; // TODO this may be wrong; TS complains without "any"
            div.appendChild(t);
            //var div2 = document.createElement(typ);
            //div2.appendChild(div);
            parsonsEditDiv.appendChild(div);
            if ( this.options.maxcheck && this.options.maxcheck == i+1) {
                div = document.createElement(type);
                div.className = "parsonsstatic";
                parsonsEditDiv.appendChild(div);
            }

        }
        parsonsEditDiv.setAttribute('class',classes);
        parsonsEditDiv.setAttribute('style',"float: left; width: 100%" + ";" + this.options.styleWords);
        let a = $(parsonsEditDiv);
        if ( this.options.maxcheck )
            a.sortable( {
                items: ':not(.parsonsstatic)',
                start: function(){
                    $('.parsonsstatic', this).each(function(){
                        var $this = $(this);
                        $this.data('pos', $this.index());
                    });
                },
                change: function(){
                    let $sortable = $(this);
                    let $statics = $('.parsonsstatic', this).detach();
                    let $helper = $('<'+type+'></'+type+'>').prependTo(this);
                    $statics.each(function(){
                        var $this = $(this);
                        var target = $this.data('pos');

                        $this.insertAfter($(type, $sortable).eq(target));
                    });
                    $helper.remove();
                }
            });
        else a.sortable();
        var parson = this;
        // a.sortable( "option", "axis", "x" );
        a.on( "sortstop", function( event, ui ) {
             if ( parson.options.onChange ) parson.options.onChange(parson);
             parson.clear();
          } );
    };

    join(separator) {
        var result = "";
        var sep = "";
        var div = this.options.sortable;
        var nosep = false;
        if ( !div ) return "";
        for (var i = 0; i < div.childElementCount; i++) {
            var node = div.childNodes[i];
            var line = node.textContent;
            if ( line === "" ) continue; // separatorline
            if ( line === "\\n" ) { line = "\n"; sep = ""; nosep = true; }
            result += sep + line;
            sep = separator;
            if ( nosep ) sep = "";
            nosep = false;
        }

        return result;
    }

    check(userText) {
        var result = "";
        var div = this.options.sortable;
        var lines = this.text.split("\n");
        var ulines = userText.split("\n");
        if ( !div ) return "";
        var maxn = div.childElementCount;
        if ( this.options.maxcheck && this.options.maxcheck < div.childElementCount) maxn = this.options.maxcheck
        for (var i = 0; i < maxn; i++) {
            var node = div.children[i];
            var nodeok = true;
            if ( this.options.notordermatters ) {
                nodeok = false;
                for (var j=0; j<maxn; j++) {
                    if ( lines[j] === ulines[i] ) {
                        lines[j] = "XXXXXXXXXXXXXX";
                        nodeok = true;
                        break;
                    }
                }
            } else { if ( lines[i] !== ulines[i] ) nodeok = false; }
            if ( !nodeok ) node.setAttribute('style',"background-color: RED;");
            else if ( this.options.maxcheck )  node.setAttribute('style',"background-color: LIGHTGREEN;");
        }

        return result;
    }

    clear() {
        var div = this.options.sortable;
        if ( !div ) return "";
        for (let i = 0; i < div.childElementCount; i++) {
            var node = div.children[i];
            node.removeAttribute('style');
        }
    }
}
