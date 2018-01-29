import {IScope} from "angular";
import $ from "jquery";
import * as refPopup from "tim/directives/refPopup";
import {Coords, markAsUsed} from "tim/utils";
import {$compile} from "../../ngimport";
import {onMouseOut, onMouseOver} from "./eventhandlers";
import {ViewCtrl} from "./ViewCtrl";

markAsUsed(refPopup);

export interface IPopupAttrs {
    [index: string]: string;
}

export class RefPopupHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public overReflink: boolean;
    public overPopup: boolean;

    initRefPopup(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onMouseOver(".parlink", ($this, e) => {
            this.overReflink = true;

            const $par = $this.parents(".par").find(".parContent");
            const offset = $par.offset() || {left: 0, top: 0};
            const coords = {left: e.pageX - offset.left + 10, top: e.pageY - offset.top + 10};
            let params;

            try {
                params = {
                    docid: $this[0].attributes["data-docid"].value,
                    parid: $this[0].attributes["data-parid"].value,
                };
            } catch (TypeError) {
                // The element was modified
                return;
            }

            this.showRefPopup(e, $this, coords, params);
        });

        onMouseOver(".ref-popup", ($this, e) => {
            this.overPopup = true;
        });

        onMouseOut(".ref-popup", ($this, e) => {
            this.overPopup = false;
            this.hideRefPopup();
        });

        onMouseOut(".parlink", ($this, e) => {
            this.overReflink = false;
            this.hideRefPopup();
        });
    }

    showRefPopup(e: Event, $ref: JQuery, coords: Coords, attrs: IPopupAttrs) {
        const $popup = $("<ref-popup>");
        $popup.offset(coords);

        for (const attr in attrs) {
            if (attrs.hasOwnProperty(attr)) {
                $popup.attr(attr, attrs[attr]);
            }
        }

        $ref.parent().prepend($popup); // need to prepend to DOM before compiling
        $compile($popup[0])(this.sc);
        return $popup;
    }

    hideRefPopup() {
        if (this.overReflink || this.overPopup) {
            return;
        }

        $(".refPopup").remove();
    }
}
