import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    ViewChild,
} from "@angular/core";
import {ViewCtrl} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";

import {valueDefu} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {HttpClientModule} from "@angular/common/http";
import {
    getKeyCode,
    KEY_LEFT,
    KEY_RIGHT,
} from "../../../static/scripts/tim/util/keycodes";

const ShowFileMarkup = t.intersection([
    t.partial({
        followid: t.string,
        width: t.number,
        height: t.number,
        hidetext: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        autoplay: withDefault(t.number, 0),
        repeat: withDefault(t.boolean, true),
        open: withDefault(t.boolean, true),
        files: withDefault(
            t.array(
                t.type({
                    name: t.string,
                    caption: withDefault(t.string, ""),
                    alt: withDefault(t.string, ""),
                })
            ),
            []
        ),
    }),
]);

const ShowFileAll = t.type({
    info: Info,
    markup: ShowFileMarkup,
    preview: t.boolean,
});

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-images",
    template: `
        <div [class]="imagesClass" 
             tabindex="0"
             (keydown.-)="speed(1.0/1.2, $event)"
             (keydown.1)="speed(0, $event)"
             (keydown.+)="speed(1.2/1.0, $event)"
             (keydown.arrowLeft)="jump(-1, $event)"
             (keydown.arrowRight)="jump(1, $event)"
             (keydown.control.arrowLeft)="jumpTo(1, $event)"
             (keydown.control.arrowRight)="jumpTo(1000, $event)"
        >
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <p *ngIf="header" [innerHtml]="header"></p>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>


            <div class="images-container">
                <div *ngFor="let file of files; let i = index">
                    <div class="fade" *ngIf="slideIndex===(i+1)">
                        <div class="numbertext">{{(i+1)}} / 3</div>
                        <img src="{{file.name}}" alt="{{file.alt}}" style="width:100%">
                        <div class="text">{{file.caption}}</div>
                    </div>    
                </div>
                <a class="prev" (click)="plusSlides(-1)">&#10094;</a>
                <a class="next" (click)="plusSlides(1)">&#10095;</a>
            </div>
            <br>
            
            <div style="text-align:center" class="images-control">
              <span *ngFor="let file of files; let i = index" class="dot" (click)="currentSlide(i+1)"></span>
            </div>                

            <div class="flex"  style="justify-content: flex-end">
                <div class="margin-5-right">
                    Speed:
                    <span class="text-smaller">
                    </span>
                    <a (click)="speed(1.0/1.2)" title="Slower speed (-)"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                    <a (click)="speed(0)" title="Normal speed (1)">1x</a>&ngsp;
                    <a (click)="speed(1.2)" title="Faster speed (+)"><i class="glyphicon glyphicon-plus"></i></a>&ngsp;
                </div>
            </div>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
    styleUrls: ["./images.component.scss"],
})
export class ImagesComponent extends AngularPluginBase<
    t.TypeOf<typeof ShowFileMarkup>,
    t.TypeOf<typeof ShowFileAll>,
    typeof ShowFileAll
> {
    get imagesClass() {
        return "imagesRunDiv";
    }

    get hidetext() {
        return valueDefu(this.markup.hidetext, "Hide file");
    }

    span?: string | null;
    @ViewChild("images") images?: ElementRef<HTMLImageElement>;
    width?: number;
    height?: number;
    private vctrl?: ViewCtrl;
    imagesOn: boolean = true;
    slideIndex = 1;
    files: {name: string; caption: string; alt: string}[] = [];
    duration: number = 0;

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance;
        this.width = this.markup.width;
        this.height = this.markup.height;
        this.files = this.markup.files;
    }

    eventListenersActive: boolean = false;

    private removeEventListeners() {
        if (!this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = false;
        document.removeEventListener("keydown", this.keyDownImages);
        // document.removeEventListener("click", this.onClick);
    }

    private addEventListeners() {
        return;
        if (this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = true;
        document.addEventListener("keydown", this.keyDownImages);
        // document.addEventListener("click", this.onClick);
    }

    private keyDownImages = (ev: KeyboardEvent) => {
        const keyCode = getKeyCode(ev);
        if (keyCode === KEY_LEFT) {
            ev.preventDefault();
            this.jump(-10);
        } else if (keyCode === KEY_RIGHT) {
            ev.preventDefault();
            this.jump(10);
        }
    };

    currentSlide(n: number) {
        this.showSlides((this.slideIndex = n));
    }

    plusSlides(n: number) {
        this.showSlides((this.slideIndex += n));
    }

    private showSlides(n: number) {
        let i;
        const dots = document.getElementsByClassName("dot");
        if (n > this.files.length) {
            this.slideIndex = 1;
        }
        if (n < 1) {
            this.slideIndex = this.files.length;
        }
        for (i = 0; i < dots.length; i++) {
            dots[i].className = dots[i].className.replace(" active", "");
        }
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        // slides[this.slideIndex - 1].style.display = "block";
        dots[this.slideIndex - 1].className += " active";
    }

    speed(mult: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.duration *= mult;
        if (mult === 0) {
            this.duration = this.markup.autoplay;
        }
    }

    jumpTo(value: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.currentSlide(value);
    }

    jump(value: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.currentSlide(this.slideIndex + value);
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return ShowFileAll;
    }
}

@NgModule({
    declarations: [ImagesComponent],
    imports: [BrowserModule, TimUtilityModule, HttpClientModule, FormsModule],
})
export class ImagesModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(ImagesModule)
        ),
        "timImages",
        ImagesComponent
    ),
];
