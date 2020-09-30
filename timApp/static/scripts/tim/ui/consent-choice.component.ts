import {Component, Input} from "@angular/core";

@Component({
    selector: "tim-consent-choice",
    template: `
        <div class="radio">
            <label>
                <input [(ngModel)]="consent"
                       [value]="2"
                       type="radio">
                <ng-container i18n>
                    I allow
                    <a href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
                        the collected data
                    </a>
                    to be used for scientific research purposes after anonymization.
                </ng-container>
            </label>
            <label>
                <input [(ngModel)]="consent"
                       [value]="1"
                       type="radio">
                <ng-container i18n>
                    I do not allow
                    <a href="/view/tim/Rekisteriseloste#rekisterin-tietosis%C3%A4lt%C3%B6">
                        the collected data
                    </a>
                    to be used for scientific research purposes after anonymization.
                </ng-container>
            </label>
        </div>
    `,
})
export class ConsentChoiceComponent {
    @Input() consent?: number;
}
