/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import * as t from "io-ts";
import $ from "jquery";
import type {AfterViewInit, ApplicationRef, DoBootstrap} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import type {OnClickArg} from "tim/document/eventhandlers";
import {onClick} from "tim/document/eventhandlers";
import type {IRights} from "tim/user/IRights";
import {genericglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AnswerSheetModule} from "tim/document/question/answer-sheet.component";
import {PurifyModule} from "tim/util/purify.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {DomSanitizer} from "@angular/platform-browser";
import {TooltipModule} from "ngx-bootstrap/tooltip";

const TimMenuMarkup = t.intersection([
    t.partial({
        backgroundColor: nullable(t.string),
        textColor: nullable(t.string),
        fontSize: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        menu: withDefault(t.string, ""),
        hoverOpen: withDefault(t.boolean, true), // Unimplemented.
        topMenu: withDefault(t.boolean, false),
        topMenuTriggerHeight: withDefault(t.number, 200),
        openAbove: withDefault(t.boolean, false),
        keepLinkColors: withDefault(t.boolean, false),
        basicColors: withDefault(t.boolean, false),
        separator: withDefault(t.string, "&nbsp;"), // Non-breaking space
        openingSymbol: withDefault(t.string, "&#9662;"), // Caret
        closeOnLinkClick: withDefault(t.boolean, false),
    }),
]);

interface ITimMenuItem {
    height?: string;
    id: string;
    items?: ITimMenuItem[];
    level: number;
    open: boolean;
    rights?: string;
    text: string;
    width?: string;
    tooltip?: string;
    tooltipPlacement?: "top" | "bottom" | "left" | "right";
}

const ITimMenuItem: t.Type<ITimMenuItem> = t.recursion("ITimMenuItem", () =>
    t.intersection([
        t.partial({
            height: t.string,
            items: t.array(ITimMenuItem),
            rights: t.string,
            width: t.string,
            tooltip: t.string,
            tooltipPlacement: t.union([
                t.literal("top"),
                t.literal("bottom"),
                t.literal("left"),
                t.literal("right"),
            ]),
        }),
        t.type({
            id: t.string,
            level: t.number,
            open: t.boolean,
            text: t.string,
        }),
    ])
);

const TimMenuAll = t.intersection([
    t.partial({
        menu: t.array(ITimMenuItem),
    }),
    t.type({
        info: Info,
        markup: TimMenuMarkup,
        preview: t.boolean,
    }),
]);

@Component({
    selector: "tim-menu-runner",
    template: `
        <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
        <span *ngIf="topMenu" class="tim-menu-placeholder"></span>
        <span *ngIf="topMenu" class="tim-menu-placeholder-content tim-menu-hidden"><br></span>
        <div class="tim-menu" [ngClass]="{'bgtim white': basicColors, 'hide-link-colors': !keepLinkColors}"
             style="{{barStyle}}" (mouseleave)="mouseLeave()" (mouseenter)="mouseEnter()">
    <span class="tim-menu-links" *ngFor="let t1 of menu; let last = last">
        <span *ngIf="t1.items && t1.items.length > 0 && hasRights(t1)" class="btn-group" style="{{setStyle(t1)}}">
          <span [innerHtml]="t1.text+openingSymbol" (click)="toggleSubmenu(t1, undefined, undefined, true)"
                (pointerover)="toggleSubmenu(t1, undefined, undefined, false, $event)"
               [tooltip]="t1.tooltip" [placement]="t1.tooltipPlacement ?? 'bottom'"></span>
          <ul class="tim-menu-dropdown" *ngIf="t1.open" [ngClass]="openDirection(t1.id)" id="{{t1.id}}">
            <li class="tim-menu-list-item" *ngFor="let t2 of t1.items" style="{{setStyle(t2)}}">
                <span class="tim-menu-item" *ngIf="t2.items && t2.items.length > 0 && hasRights(t2)">
                    <span class="tim-menu-item tim-menu-itemcontent" [innerHtml]="t2.text+openingSymbol"
                          (click)="toggleSubmenu(t2, t1, undefined, true)"
                              (pointerover)="toggleSubmenu(t2, t1, undefined, false, $event)"
                          [tooltip]="t2.tooltip" [placement]="t2.tooltipPlacement ?? 'bottom'"></span>
                    <ul class="tim-menu-dropdown tim-menu-innerlist" id="{{t2.id}}" [ngClass]="openDirection(t2.id)" *ngIf="t2.open">
                        <li class="tim-menu-list-item" *ngFor="let t3 of t2.items" style="{{setStyle(t3)}}">
                            <span class="tim-menu-item" *ngIf="t3.items && t3.items.length > 0 && hasRights(t3)">
                                <span class="tim-menu-item tim-menu-itemcontent" [innerHtml]="t3.text+openingSymbol"
                                      (click)="toggleSubmenu(t3, t2, t1, true)"
                                      (pointerover)="toggleSubmenu(t3, t2, t1, false, $event)"
                                      [tooltip]="t3.tooltip" [placement]="t3.tooltipPlacement ?? 'bottom'"></span>
                                <ul class="tim-menu-dropdown tim-menu-innerlist" id="{{t3.id}}" [ngClass]="openDirection(t3.id)"
                                    *ngIf="t3.open">
                                    <ng-container *ngFor="let t4 of t3.items">
                                        <li class="tim-menu-list-item tim-menu-itemcontent" style="{{setStyle(t4)}}"
                                            [tooltip]="t4.tooltip" [placement]="t4.tooltipPlacement ?? 'bottom'"
                                            *ngIf="hasRights(t4)"
                                            [innerHtml]="t4.text"
                                        >
                                        </li>
                                    </ng-container>
                                </ul>
                            </span>
                            <span class="tim-menu-item tim-menu-itemcontent" *ngIf="t3.items && t3.items.length < 1  && hasRights(t3)"
                                  [innerHtml]="t3.text" (click)="toggleSubmenu(t3, t2, t1, true)"
                                  (pointerover)="toggleSubmenu(t3, t2, t1, false, $event)"
                                  [tooltip]="t3.tooltip" [placement]="t3.tooltipPlacement ?? 'bottom'"></span>
                        </li>
                    </ul>
                </span>
                <span class="tim-menu-item tim-menu-itemcontent" *ngIf="t2.items && t2.items.length < 1 && hasRights(t2)"
                      [innerHtml]="t2.text" (click)="toggleSubmenu(t2, t1, undefined, true)"
                      (pointerover)="toggleSubmenu(t2, t1, undefined, false, $event)"
                      [tooltip]="t2.tooltip" [placement]="t2.tooltipPlacement ?? 'bottom'"></span>
            </li>
          </ul>
        </span>
        <span *ngIf="t1.items && t1.items.length < 1 && hasRights(t1)" class="btn-group" style="{{setStyle(t1)}}"
               (click)="toggleSubmenu(t1, undefined, undefined, true)"
              [innerHtml]="t1.text"
              (pointerover)="toggleSubmenu(t1, undefined, undefined, false, $event)"
              [tooltip]="t1.tooltip" [placement]="t1.tooltipPlacement ?? 'bottom'"></span>
        <span *ngIf="!last && hasRights(t1)" [innerHtml]="separator"></span>
    </span>
        </div>
    `,
    styleUrls: ["tim-menu-plugin.component.scss"],
})
export class TimMenuPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof TimMenuMarkup>,
        t.TypeOf<typeof TimMenuAll>,
        typeof TimMenuAll
    >
    implements AfterViewInit
{
    menu: ITimMenuItem[] = [];

    openingSymbol: string = "";
    private hoverOpen: boolean = true;
    separator: string = "";
    topMenu: boolean = false;
    basicColors: boolean = false;
    private openAbove: boolean = false;
    keepLinkColors: boolean = false;
    private previousScroll: number | undefined = 0; // Store y-value of previous scroll event for comparison.
    private previousItem: ITimMenuItem | undefined;
    private previouslyClicked: ITimMenuItem | undefined;
    barStyle: string = "";
    private mouseInside: boolean = false; // Whether mouse cursor is inside the menu.
    private clickedInside: boolean = false; // Whether a menu has been opened with mouse click.
    private userRights: IRights | undefined;
    private previousSwitch: number | undefined;
    topMenuTriggerHeight: number | undefined; // Pixels to scroll before topMenu appears.
    topMenuVisible: boolean = false; // Meant to curb unnecessary topMenu state changes.
    private previouslyScrollingDown: boolean = true;
    private userPrefersHoverDisabled: boolean = false;
    private touchMode: boolean = false; // If a touch event has been detected in HTML body.
    requiresTaskId = false;

    getDefaultMarkup() {
        return {};
    }

    constructor(
        el: ElementRef,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
    }

    ngOnInit() {
        super.ngOnInit();
        if (this.vctrl == null || this.vctrl.isSlideOrShowSlideView()) {
            return;
        }
        if (!this.attrsall.menu) {
            return;
        }
        this.menu = this.attrsall.menu;
        // console.log(this.menu);
        this.separator = this.markup.separator;
        this.topMenu = this.markup.topMenu;
        this.openAbove = this.markup.openAbove;
        this.keepLinkColors = this.markup.keepLinkColors;
        this.basicColors = this.markup.basicColors;
        this.openingSymbol = this.markup.openingSymbol;
        // Turn default symbol upwards if menu opens above.
        if (this.markup.openAbove && this.openingSymbol == "&#9662;") {
            this.openingSymbol = "&#9652;";
        }
        if (this.topMenu && !this.attrsall.preview) {
            // Divided by two because the check is half of the user given height towards either direction.
            this.topMenuTriggerHeight = Math.floor(
                this.markup.topMenuTriggerHeight / 2
            );
            if (this.topMenuTriggerHeight == 0) {
                // Logic doesn't work if the height is zero. Negative number makes it stay after appearing
                // until scrolled to its original location in document.
                this.topMenuTriggerHeight = 1;
            }
            window.onscroll = () => {
                this.toggleSticky();
            };
            window.onresize = () => {
                this.setBarStyles();
            };
        }
        onClick("body", ($this, e) => {
            this.onClick(e);
        });
        if (genericglobals().userPrefs.disable_menu_hover) {
            this.userPrefersHoverDisabled =
                genericglobals().userPrefs.disable_menu_hover;
        }
        this.hoverOpen =
            this.markup.hoverOpen && !this.userPrefersHoverDisabled;
        this.userRights = this.vctrl.item.rights;
        this.setBarStyles();

        if (this.markup.closeOnLinkClick) {
            $(this.element).on("click", "a", (e) => {
                this.closeMenus();
            });
        }
    }

    ngAfterViewInit() {
        const el = this.element[0];
        const links = el.querySelectorAll(".tim-menu-links a");
        const currentUrl = new URL(window.location.href);

        for (const link of links) {
            const a = link as HTMLAnchorElement;
            if (a.href) {
                const fullHref = new URL(a.href, window.location.href);
                if (
                    fullHref.hostname == currentUrl.hostname &&
                    fullHref.pathname == currentUrl.pathname
                ) {
                    a.classList.add("active");
                }
            }
        }
    }

    getAttributeType() {
        return TimMenuAll;
    }

    /**
     * Close other menus and toggle clicked menu open or closed.
     * TODO: Better way to do this (for deeper menus).
     * @param item Clicked menu item.
     * @param parent1 Closest menu item parent.
     * @param parent2 Further menu item parent.
     * @param clicked Toggled by a mouse click.
     * @param event Event that triggered the toggle
     */
    toggleSubmenu(
        item: ITimMenuItem,
        parent1: ITimMenuItem | undefined,
        parent2: ITimMenuItem | undefined,
        clicked: boolean,
        event?: PointerEvent
    ) {
        // ignore touch-based hover, because direct touch press raises both hover and click events
        if (event?.pointerType === "touch" && !clicked) {
            return;
        }
        // If called by mouseenter and either hover open is off or touch mode is on, do nothing.
        if (!clicked && (!this.hoverOpen || this.touchMode)) {
            return;
        }
        // Toggle open menu closed and back again when clicking.
        if (this.previousItem && this.previousItem === item && clicked) {
            item.open = !item.open;
            this.previousItem = item;
            this.previouslyClicked = item;
            return;
        }
        if (this.previouslyClicked === item && !clicked) {
            return;
        }
        this.previouslyClicked = undefined;
        // Close all menus when clicking menu that isn't child of previously clicked.
        if (parent1 && parent1 !== this.previousItem) {
            for (const menu of this.menu) {
                this.closeAllInMenuItem(menu);
            }
            parent1.open = true;
            if (parent2) {
                parent2.open = true;
            }
        }
        // A first level menu doesn't have a parent; close all other menus.
        if (!parent1 && item !== this.previousItem) {
            for (const menu of this.menu) {
                this.closeAllInMenuItem(menu);
            }
        }
        // Unless already open, clicked item always opens.
        item.open = true;
        this.previousItem = item;
    }

    /**
     * Closes all levels of a menu item recursively.
     * @param t1 First level menu item.
     */
    closeAllInMenuItem(t1: ITimMenuItem) {
        t1.open = false;
        if (!t1.items) {
            return;
        }
        for (const t2 of t1.items) {
            this.closeAllInMenuItem(t2);
        }
    }

    /**
     * Makes the element show at top when scrolling towards it from below.
     */
    toggleSticky() {
        // TODO: Multiple topMenus.
        // TODO: Check the trigger height logic.
        const menu = this.element.find(".tim-menu")[0];
        const placeholder = this.element.find(".tim-menu-placeholder")[0];
        const scrollY = $(window).scrollTop();

        if (
            !menu ||
            !placeholder ||
            !this.topMenuTriggerHeight ||
            scrollY == undefined
        ) {
            return;
        }
        if (!this.previousScroll) {
            this.previousScroll = scrollY;
            return;
        }
        // Placeholder and its content are separate, because when hidden y is 0.
        const placeholderContent = this.element.find(
            ".tim-menu-placeholder-content"
        )[0];
        const belowPlaceholder = placeholder.getBoundingClientRect().bottom < 0;
        const scrollingDown = scrollY > this.previousScroll;
        const scrollDirHasChanged =
            (this.previouslyScrollingDown && !scrollingDown) ||
            (!this.previouslyScrollingDown && scrollingDown);
        if (scrollDirHasChanged) {
            // If scrolling direction has changed, set the scroll trigger distance to be centered at that y-coordinate.
            this.previousSwitch = scrollY;
        }
        this.previouslyScrollingDown = scrollingDown;
        this.previousScroll = $(window).scrollTop();
        const isSmallScreen =
            $(".device-xs").is(":visible") || $(".device-sm").is(":visible");

        // Sticky can only show when the element's place in document goes outside upper bounds.
        if (belowPlaceholder) {
            // When scrolling downwards, don't show fixed menu and hide placeholder content.
            // Otherwise (i.e. scrolling upwards), show menu as fixed and let placeholder take its place in document
            // to mitigate page length changes.
            if (scrollingDown) {
                // If topMenu is already hidden, don't try hiding it again.
                // If scroll distance is smaller than min height, don't do anything.
                if (
                    !this.topMenuVisible ||
                    !this.previousSwitch ||
                    scrollY > this.previousSwitch + this.topMenuTriggerHeight
                ) {
                    return;
                }
                menu.classList.remove("top-menu");
                menu.classList.remove("top-menu-mobile");
                placeholderContent.classList.add("tim-menu-hidden");
                this.topMenuVisible = false;
            } else {
                // If topMenu is already visible, the rest are redundant.
                if (
                    this.topMenuVisible ||
                    (this.previousSwitch &&
                        !(
                            scrollY >
                                this.previousSwitch +
                                    this.topMenuTriggerHeight ||
                            scrollY <
                                this.previousSwitch - this.topMenuTriggerHeight
                        ))
                ) {
                    return;
                }
                menu.classList.add("top-menu");
                if (isSmallScreen) {
                    menu.classList.add("top-menu-mobile");
                }
                placeholderContent.classList.remove("tim-menu-hidden");
                this.topMenuVisible = true;
            }
        } else {
            if (!this.topMenuVisible) {
                return;
            }
            menu.classList.remove("top-menu");
            menu.classList.remove("top-menu-mobile");
            placeholderContent.classList.add("tim-menu-hidden");
            this.topMenuVisible = false;
        }
    }

    /**
     * Set styles for menu bar defined in optional attributes.
     * Includes adjusting topMenu width to fit document area width.
     */
    private setBarStyles() {
        if (this.markup.backgroundColor) {
            this.barStyle += `background-color: ${this.markup.backgroundColor}; `;
        }
        if (this.markup.textColor) {
            this.barStyle += `color: ${this.markup.textColor}; `;
        }
        if (this.markup.fontSize) {
            this.barStyle += `font-size: ${this.markup.fontSize}; `;
        }
        // If menu isn't sticky, default width is fine.
        if (this.topMenu) {
            const menuWidth = this.element.parent().width();
            if (menuWidth) {
                this.barStyle += `width: ${menuWidth}px; `;
            } else {
                this.barStyle = `width: 100%; `;
            }
        }
    }

    /**
     * Sets style based on the object's attributes.
     * @param item Menu item.
     */
    setStyle(item: ITimMenuItem) {
        let style = "";
        if (item.width) {
            style += `width: ${item.width}; `;
        }
        if (item.height) {
            style += `height: ${item.height}; `;
        }
        return style;
    }

    /**
     * Checks whether the user has required rights to see the menu item.
     * If no requirements have been set, return true.
     * Rights can can specified according to users and groups as well,
     * separating them with a semicolon ';' if necessary.
     * @param item Menu item.
     */
    hasRights(item: ITimMenuItem) {
        if (
            item.rights &&
            !["edit", "teacher", "manage", "owner"].includes(item.rights)
        ) {
            function intersect(a: Set<string>, b: Set<string>): Set<string> {
                if (b.size < a.size) {
                    [a, b] = [b, a];
                }
                const intersection = new Set<string>();
                for (const itm of a) {
                    if (b.has(itm)) {
                        intersection.add(itm);
                    }
                }
                return intersection;
            }

            // it's a group or user, or a combination of those
            const rights = new Set(item.rights.split(";").map((r) => r.trim()));
            const groups = new Set(
                genericglobals().current_user.groups.map((g) => g.name)
            );

            return (
                intersect(groups, rights).size > 0 ||
                groups.has("Administrators") // for easier debugging
            );
        }

        // TODO: Limit the amount of checks.
        // If item has no set rights, show to everyone.
        if (!item.rights) {
            return true;
        }
        if (this.userRights) {
            switch (item.rights) {
                case "edit":
                    return this.userRights.editable;
                case "teacher":
                    return this.userRights.teacher;
                case "manage":
                    return this.userRights.manage;
                case "owner":
                    return this.userRights.owner;
                default:
                    // Return true, if user has none of the supported rights.
                    // View not included, since it's redundant: without view the whole document is hidden.
                    return true;
            }
        } else {
            // Non-logged in users who see the page have only view rights.
            return !(
                item.rights == "edit" ||
                item.rights == "manage" ||
                item.rights == "owner"
            );
        }
    }

    /**
     * Decide what direction submenus open towards.
     * @param id Element id.
     */
    openDirection(id: string) {
        const horizontal = ""; // Default: centered.
        let vertical = ""; // Default: below.
        // If true, opens all menus above.
        if (this.openAbove) {
            vertical = "tim-menu-up";
        }
        return `${horizontal} ${vertical}`;
    }

    /**
     * Closes the menu structure if mouse is outside all menu elements.
     * @param e Click event.
     */
    private onClick(e: OnClickArg) {
        // console.log("onClick, " + e.type);
        // Detect touch screen when touch event happens.
        this.touchMode = e.type.includes("touch");
        if (this.mouseInside) {
            this.clickedInside = true;
        } else {
            this.clickedInside = false;
            this.closeMenus();
        }
    }

    /**
     * Handle mouseleave event.
     */
    mouseLeave() {
        // console.log("mouseLeave");
        this.mouseInside = false;
        this.previouslyClicked = undefined;
        // Only close menus on mouseleave, if hoverOpen is enabled and menu hasn't been clicked.
        if (!this.clickedInside && this.hoverOpen) {
            this.closeMenus();
        }
    }

    mouseEnter() {
        // console.log("mouseEnter");
        this.mouseInside = true;
    }

    /**
     * Closes all menus and handles updating.
     */
    private closeMenus() {
        for (const t1 of this.menu) {
            this.closeAllInMenuItem(t1);
        }
        this.cdr.detectChanges();
    }
}

@NgModule({
    declarations: [TimMenuPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        AnswerSheetModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class TimMenuPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("timmenu-runner", TimMenuPluginModule, TimMenuPluginComponent);
