import {Injectable} from "@angular/core";
import {Users} from "tim/user/userService";

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
