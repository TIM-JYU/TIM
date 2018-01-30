import {IScope} from "angular";
import $ from "jquery";
import * as refPopup from "tim/directives/refPopup";
import {Coords, markAsUsed} from "tim/utils";
import {$compile} from "../../ngimport";
import {onMouseOut, onMouseOver} from "./eventhandlers";
import {ViewCtrl} from "./viewctrl";

markAsUsed(refPopup);

export interface IRefPopupAttrs {
    docid: string;
    parid: string;
}

export class RefPopupHandler {
    public sc: IScope;
    public viewctrl: ViewCtrl;
    public overReflink: boolean;
    public overPopup: boolean;
    private popupElement?: JQLite;

    initRefPopup(sc: IScope, view: ViewCtrl) {
        this.sc = sc;
        this.viewctrl = view;
        onMouseOver(".parlink", ($this, e) => {
            if (this.overReflink || this.overPopup) {
                return;
            }
            this.overReflink = true;

            const $par = $this.parents(".par").find(".parContent");
            const offset = $par.offset() || {left: 0, top: 0};
            const coords = {left: e.pageX - offset.left + 10, top: e.pageY - offset.top + 10};

            const docid = $this.attr("data-docid");
            if (!docid) {
                return;
            }
            const parid = $this.attr("data-parid");
            if (!parid) {
                return;
            }
            this.showRefPopup(e, $this, coords, {docid, parid});
        });

        onMouseOver(".ref-popup", ($this, e) => {
            this.overPopup = true;
        });

        onMouseOut(".ref-popup", ($this, e) => {
            this.overPopup = false;
            if (!this.overReflink) {
                this.hideRefPopup();
            }
        });

        onMouseOut(".parlink", ($this, e) => {
            this.overReflink = false;
            if (!this.overPopup) {
                this.hideRefPopup();
            }
        });
    }

    showRefPopup(e: Event, $ref: JQuery, coords: Coords, attrs: IRefPopupAttrs) {
        const $popup = $("<ref-popup>");
        $popup.offset(coords);

        $popup.attr("docid", attrs.docid);
        $popup.attr("parid", attrs.parid);

        $ref.parent().prepend($popup); // need to prepend to DOM before compiling
        this.popupElement = $compile($popup[0])(this.sc);
    }

    hideRefPopup() {
        if (!this.popupElement) {
            return;
        }

        this.popupElement.remove();
        this.popupElement = undefined;
    }
}
