import {
    ChangeDetectorRef,
    ComponentFactoryResolver,
    Directive,
    EventEmitter,
    Input,
    OnInit,
    Output,
    ViewContainerRef,
} from "@angular/core";
import {to2} from "tim/util/utils";
import {AngularDialogComponent, IDialogOptions} from "tim/ui/angulardialog/angular-dialog-component.directive";

export type DialogConstructor = new (...args: unknown[]) => AngularDialogComponent<unknown, unknown>;

export interface IDialogInstanceEvent {
    id: number;
    instance: AngularDialogComponent<unknown, unknown>;
}

@Directive({
    selector: "[timDialogHost]",
})
export class DialogHostDirective implements OnInit {
    @Input() timDialogHost!: DialogConstructor;
    @Input() hostData: unknown;
    @Input() dialogOptions?: IDialogOptions;
    @Input() instanceId!: number;
    @Output() instanceCreated = new EventEmitter<IDialogInstanceEvent>();
    @Output() instanceClosed = new EventEmitter<IDialogInstanceEvent>();

    constructor(
        private componentFactoryResolver: ComponentFactoryResolver,
        private viewContainerRef: ViewContainerRef,
        private cdr: ChangeDetectorRef,
    ) {
    }

    ngOnInit() {
        const componentFactory = this.componentFactoryResolver.resolveComponentFactory(
            this.timDialogHost,
        );
        this.viewContainerRef.clear();
        const ref = this.viewContainerRef.createComponent(componentFactory);
        ref.instance.data = this.hostData;
        ref.instance.dialogOptions = this.dialogOptions;
        const event = {instance: ref.instance, id: this.instanceId};
        this.cdr.detectChanges();
        this.instanceCreated.emit(event);
        (async () => {
            const r = await to2(ref.instance.result);
            this.instanceClosed.emit(event);
        })();
    }
}
