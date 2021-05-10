import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    OnDestroy,
    OnInit,
    Output,
    ViewChild,
} from "@angular/core";
import {BrowserMultiFormatReader, Exception, Result} from "@zxing/library";
import * as t from "io-ts";
import {TimStorage} from "../../util/utils";
import {
    cameraConstraintSupported,
    hasVideoDevices,
    setStreamConstraints,
} from "./util";

@Component({
    selector: "tim-code-scanner",
    template: `
        <video [class.hidden]="!codeReaderStream" #barcodeOutput></video>
        <a role="button" class="camera-settings-toggle" (click)="showSettings = !showSettings" *ngIf="availableCameras.length > 1 || (hasCameras && supportsConstraint('torch'))">
            <i class="glyphicon" [class.glyphicon-menu-right]="!showSettings" [class.glyphicon-menu-down]="showSettings"></i>
            <span i18n>Camera settings</span>
        </a>
        <div class="camera-settings" [class.hidden]="!showSettings">
            <div class="input-group camera-select" *ngIf="availableCameras.length > 1">
                <span class="input-group-addon" i18n>Selected camera</span>
                <select class="form-control" [(ngModel)]="selectedCamera" (ngModelChange)="cameraSelected()">
                    <option *ngFor="let camera of availableCameras" [ngValue]="camera.id">{{camera.name}}</option>
                </select>
            </div>
            <button class="btn btn-sm" [class.btn-default]="!enableTorch" [class.btn-warning]="enableTorch"
                    *ngIf="hasCameras && supportsConstraint('torch')" (click)="toggleFlashlight()">
                <ng-container *ngIf="!enableTorch; else disableFlashlight" i18n>
                    Enable flashlight
                </ng-container>
                <ng-template #disableFlashlight i18n>
                    Disable flashlight
                </ng-template>
            </button>
        </div>
        <div class="info-messages">
            <span *ngIf="!hasCameras" class="label label-danger not-supported" i18n>No cameras found</span>
            <span *ngIf="hasCameras && !supportsConstraint('torch')" class="label label-default not-supported" i18n>Flashlight is not supported</span>
        </div>
    `,
    styleUrls: ["./code-scanner.component.scss"],
})
export class CodeScannerComponent implements OnInit, OnDestroy {
    @ViewChild("barcodeOutput") barcodeOutput!: ElementRef<HTMLVideoElement>;
    @Input() scanInterval = 1500;
    @Output() successfulRead: EventEmitter<Result> = new EventEmitter<Result>();
    @Output() readError: EventEmitter<Exception> = new EventEmitter<
        Exception
    >();

    codeReader: BrowserMultiFormatReader;
    codeReaderStream?: MediaStream;
    availableCameras: {id: string; name: string}[] = [];
    hasCameras = true;
    enableTorch = false;
    showSettings = false;
    private selectedCameraStorage = new TimStorage(
        "codeScannerSelectedCamera",
        t.string
    );
    selectedCamera = this.selectedCameraStorage.get();

    constructor() {
        this.codeReader = new BrowserMultiFormatReader(
            undefined,
            this.scanInterval
        );
    }

    supportsConstraint(name: string) {
        return cameraConstraintSupported(name);
    }

    ngOnInit() {
        void this.startCamera();
    }

    async ngOnDestroy() {
        if (!this.hasCameras) {
            return;
        }

        await this.destroyCamera();
    }

    cameraSelected() {
        this.selectedCameraStorage.set(this.selectedCamera!);
        // Reset the camera to the selected one
        // We might need to re-query the cameras as well in case they have been unplugged
        void this.startCamera();
    }

    toggleFlashlight() {
        this.enableTorch = !this.enableTorch;

        if (this.codeReaderStream) {
            void setStreamConstraints(this.codeReaderStream, {
                torch: this.enableTorch,
            });
        }
    }

    private async startCamera() {
        await this.destroyCamera();

        // Always query for video devices before doing anything to cameras because some devices give access only by permission
        this.hasCameras = await hasVideoDevices();
        if (!this.hasCameras) {
            return;
        }

        const devices = await this.codeReader.listVideoInputDevices();
        this.availableCameras = devices.map((d) => ({
            id: d.deviceId,
            name: d.label,
        }));

        // Reset camera ID if it's not found in the list of available IDs
        if (
            !this.selectedCamera &&
            !this.availableCameras.find(({id}) => id == this.selectedCamera)
        ) {
            this.selectedCamera = undefined;
        }

        // Try to first select by ID. If none is selected, ask browser for back-facing camera
        const queryParams = this.selectedCamera
            ? {deviceId: this.selectedCamera}
            : {facingMode: "environment"};
        this.codeReaderStream = await navigator.mediaDevices.getUserMedia({
            video: queryParams,
        });

        const trackSettings = this.codeReaderStream
            .getVideoTracks()[0]
            .getSettings();
        this.selectedCamera = trackSettings.deviceId ?? "";

        // Sync currently selected camera ID with the storage
        this.selectedCameraStorage.set(this.selectedCamera);

        const readHandle = this.codeReader.decodeFromStream(
            this.codeReaderStream,
            this.barcodeOutput.nativeElement,
            (result, error) => {
                if (error) {
                    this.readError.emit(error);
                } else {
                    this.successfulRead.emit(result);
                }
            }
        );

        await setStreamConstraints(this.codeReaderStream, {focusMode: "auto"});
        await setStreamConstraints(this.codeReaderStream, {
            torch: this.enableTorch,
        });
        await readHandle;
    }

    private async destroyCamera() {
        if (!this.codeReaderStream) {
            return;
        }

        await setStreamConstraints(this.codeReaderStream, {torch: false});
        this.codeReader.reset();
        this.codeReaderStream
            ?.getVideoTracks()
            .forEach((track) => track.stop());

        this.codeReaderStream = undefined;
    }
}
