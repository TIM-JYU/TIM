/**
 * Helper component that lazily loads formula editor component.
 * Passes all inputs to formulaeditor and subscribes to all
 * its events
 *
 * @author Juha Reinikainen
 * @license MIT
 * @date 21.4.2023
 */

import type {AfterViewInit, OnDestroy} from "@angular/core";
import {
    Component,
    EventEmitter,
    Input,
    Output,
    ViewChild,
    ViewContainerRef,
} from "@angular/core";
import type {ITemplateButton} from "../../csPlugin";
import {IEditor} from "../editor";
import {FormulaEvent} from "./symbol-button-menu.component";
import type {FormulaEditorComponent} from "./formula-editor.component";

@Component({
    selector: "cs-formula-editor-loader",
    template: `
        <ng-container #formulaEditor></ng-container>
    `,
})
export class FormulaEditorLoaderComponent implements OnDestroy, AfterViewInit {
    private parEditor!: IEditor;
    private buttons!: ITemplateButton[];
    private isVisible!: boolean;
    private buttonSymbol!: FormulaEvent;

    @ViewChild("formulaEditor", {read: ViewContainerRef, static: true})
    formulaEditorContainer!: ViewContainerRef;

    formulaEditorComponent!: FormulaEditorComponent;

    @Output() okClose = new EventEmitter<number>();
    @Output() cancelClose = new EventEmitter<number>();
    @Output() toggle = new EventEmitter<void>();
    @Output() componentLoaded = new EventEmitter<() => Promise<boolean>>();

    /**
     * Gets editor value.
     */
    @Input()
    get editor(): IEditor {
        return this.parEditor;
    }

    /**
     * Sets editor value.
     * @param value editor to set
     */
    set editor(value: IEditor) {
        this.parEditor = value;
        if (this.formulaEditorComponent) {
            this.formulaEditorComponent.editor = value;
        }
    }

    /**
     * Gets template buttons.
     */
    @Input()
    get templateButtons(): ITemplateButton[] {
        return this.buttons;
    }

    /**
     * Sets template buttons.
     * @param value buttons to set
     */
    set templateButtons(value: ITemplateButton[]) {
        this.buttons = value;
        if (this.formulaEditorComponent) {
            this.formulaEditorComponent.templateButtons = value;
        }
    }

    /**
     * Gets visible value
     */
    @Input()
    get visible(): boolean {
        return this.isVisible;
    }

    /**
     * Sets visible value
     * @param isVis value to set
     */
    set visible(isVis: boolean) {
        this.isVisible = isVis;
        if (this.formulaEditorComponent) {
            this.formulaEditorComponent.visible = isVis;
        }
    }

    /**
     * Gets currentSymbol.
     */
    @Input()
    get currentSymbol(): FormulaEvent {
        return this.buttonSymbol;
    }

    /**
     * Sets currentSymbol.
     * @param value value to set
     */
    set currentSymbol(value: FormulaEvent) {
        this.buttonSymbol = value;
        if (this.formulaEditorComponent) {
            this.formulaEditorComponent.currentSymbol = value;
        }
    }

    /**
     * Loads component into container and sets its inputs
     * and subscribes to its events.
     */
    async loadComponent() {
        const component = (await import("./formula-editor.component"))
            .FormulaEditorComponent;
        const comp = this.formulaEditorContainer.createComponent(component);
        this.formulaEditorComponent = comp.instance;
        comp.instance.editor = this.editor;
        comp.instance.visible = this.visible;
        comp.instance.currentSymbol = this.currentSymbol;
        comp.instance.templateButtons = this.templateButtons;

        comp.instance.cancelClose.subscribe((val) =>
            this.cancelClose.emit(val)
        );
        comp.instance.okClose.subscribe((val) => this.okClose.emit(val));
        comp.instance.toggle.subscribe((val) => this.toggle.emit(val));

        this.componentLoaded.emit(() => comp.instance.handleFormulaCancel());
    }

    /**
     * Loads component.
     */
    ngAfterViewInit() {
        void this.loadComponent();
    }

    /**
     * Unsubscribes from events.
     */
    ngOnDestroy(): void {
        if (!this.formulaEditorComponent) {
            return;
        }
        this.formulaEditorComponent.cancelClose.unsubscribe();
        this.formulaEditorComponent.okClose.unsubscribe();
        this.formulaEditorComponent.toggle.unsubscribe();
    }
}
