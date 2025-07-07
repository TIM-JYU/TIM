import {Users} from "./userService";
import {Injectable} from "@angular/core";

@Injectable({providedIn: "root"})
export class LinkLanguageService {
    getLangLink(link: string): string {
        const currentLang = Users.getCurrentLanguage();
        // For the basecase, with language fi, return unmodified link
        if (currentLang == "fi") {
            return link;
        }
        // For languages not handled, default to english
        return link + "/en-US";
    }
}
