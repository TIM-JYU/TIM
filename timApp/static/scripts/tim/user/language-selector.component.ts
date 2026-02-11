import {Component, EventEmitter, Input, Output} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {toPromise} from "tim/util/utils";

const webBrowser = "UseWebBrowser";

@Component({
    selector: "tim-language-selector",
    template: `
        <label><ng-container i18n>Language</ng-container>:
            <select class="form-control"
                    [(ngModel)]="language"
                    (ngModelChange)="onChange($event)"
            >
                <option [ngValue]="webBrowser" i18n>Use web browser preference</option>
                <option [ngValue]="'en-US'">English</option>
                <option [ngValue]="'es'">Español</option>
                <option [ngValue]="'fi'">Suomi</option>
                <option [ngValue]="'sv'">Svenska</option>
            </select>
        </label>
    `,
    styleUrls: ["./language-selector.component.scss"],
})
export class LanguageSelectorComponent {
    @Input() saveOnChange = false;
    @Input() language: string | null = "";
    @Output() languageChange = new EventEmitter<string>();
    webBrowser = webBrowser;

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.language = this.language ?? webBrowser;
        this.languageChange.emit(this.language);
    }

    async onChange(newLanguage: string) {
        this.language = newLanguage;
        this.languageChange.emit(newLanguage);
        if (this.saveOnChange) {
            await toPromise(
                this.http.put("/settings/save/lang", {
                    lang: this.language || null,
                })
            );
            location.reload();
        }
    }
}
