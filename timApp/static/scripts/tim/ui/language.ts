import {escapeRegExp} from "../util/utils";

export const FRONT_PAGE_DEFAULT_LANGUAGE = "fi";

const translations: Record<string, {fi: string | {default: string, [index: string]: string | undefined}, en?: string}> = {
    "Add a user to this session": {fi: "Lisää käyttäjä istuntoon"},
    "All documents": {fi: "Kaikki dokumentit"},
    "and": {fi: "ja"},
    "Available courses": {fi: "Saatavilla olevat kurssit"},
    "Cancel": {fi: "Peruuta"},
    "Close": {fi: "Sulje"},
    "Continue": {fi: "Jatka"},
    "Create a new document": {fi: "Luo uusi dokumentti"},
    "Create a new password": {fi: "Luo uusi salasana"},
    "Create a TIM account": {fi: "Luo TIM-tili"},
    "Do not type anything here": {fi: "Älä kirjoita tähän mitään"},
    "Email or username": {fi: "Sähköpostiosoite tai käyttäjätunnus"},
    "Enter your name (Lastname Firstname)": {fi: "Kirjoita nimesi (Sukunimi Etunimi)"},
    "everyone": {fi: "kaikki"},
    "Examples": {fi: "Esimerkkejä"},
    "Finish": {fi: "Valmis"},
    "Get started": {fi: "Aloita"},
    "Haka login": {fi: "Haka-kirjautuminen"},
    "I forgot my password": {fi: "Unohdin salasanani"},
    "If you don't have an existing TIM or {org} account, you can create a TIM account here.": {fi: "Jos sinulla ei ole TIM-tiliä tai {org}-tunnusta, voit luoda TIM-tilin tässä."},
    "Introduction": {fi: "Esittely"},
    "Log in": {fi: "Kirjaudu sisään"},
    "Log out": {fi: "Kirjaudu ulos"},
    "Log": {fi: {self: "Kirjaudu", default: "Kirjaa"}},
    "My documents": {fi: "Omat dokumentit"},
    "Not a {org} student or staff member and don't have a TIM account?": {fi: "Etkö ole {org} opiskelija tai henkilökuntaa eikä sinulla ole TIM-tiliä?"},
    "Now you can": {fi: "Nyt voit"},
    "other": {fi: "muu"},
    "others": {fi: "muuta"},
    "Others, please log in with your TIM account.": {fi: "Muut, kirjautukaa sisään TIM-tilillä."},
    "out": {fi: "ulos"},
    "Password you received": {fi: "Salasana, jonka sait"},
    "Password": {fi: "Salasana"},
    "Please input your email address to receive a temporary password.": {fi: "Kirjoita sähköpostiosoitteesi saadaksesi väliaikaisen salasanan."},
    "Programming languages": {fi: "Ohjelmointikieliä"},
    "refresh": {fi: "virkistää"},
    "Retype password": {fi: "Kirjoita salasana uudelleen"},
    "Retype the above password": {fi: "Kirjoita yllä oleva salasana uudelleen"},
    "Select your home organization...": {fi: "Valitse kotiorganisaatiosi..."},
    "Sign up": {fi: "Luo tili"},
    "Thank you!": {fi: "Kiitos!"},
    "the page to log in.": {fi: "sivun kirjautuaksesi sisään."},
    "TIM is a document-based cloud service for producing interactive materials.": {fi: "TIM on dokumenttipohjainen pilvipalvelu interaktiivisten materiaalien tuottamiseksi."},
    "TIM only": {fi: "vain TIM"},
    "TIM sent you a temporary password. Please check your email and type the password below to continue.": {fi: "TIM lähetti sinulle väliaikaisen salasanan. Katso sähköpostisi ja kirjoita salasana alle jatkaaksesi."},
    "TIM's possibilities": {fi: "TIMin mahdollisuuksia"},
    "To reset password, enter your email or username first.": {fi: "Salasanan vaihtamiseksi kirjoita sähköpostiosoitteesi tai käyttäjätunnuksesi."},
    "Usage in different subjects": {fi: "Käyttö eri oppiaineissa"},
    "User guide": {fi: "Käyttöohje"},
    "What is TIM?": {fi: "Mikä on TIM?"},
    "You're logged in": {fi: "Olet kirjautunut sisään"},
    "Your information was updated successfully.": {fi: "Tietosi päivitettiin onnistuneesti."},
    "Your name": {fi: "Nimesi"},
    "Members from universities and other Haka organizations, please use Haka to log in.": {fi: "Yliopistojen ja muiden Haka-organisaatioiden jäsenet, kirjautukaa Hakalla."},
};

export type Lang = "fi" | "en";
export const language: {lang: Lang} = {lang: "en"};

export function tr(s: string, opts?: {plural?: boolean}, replacements?: Record<string, string>): string {
    const entry = translations[s];
    if (!entry) {
        console.log(`Missing translation for: ${s}`);
        return s;
    }
    const e = entry[language.lang];
    let result;
    if (!e) {
        result = s;
    } else if (typeof e === "string") {
        result = e || s;
    } else if (opts) {
        for (const [k, v] of Object.entries(opts)) {
            if (v) {
                result = e[k] ?? s;
            }
        }
        result = e.default;
    } else {
        result = e.default;
    }
    if (!replacements) {
        return result;
    }
    for (const [k, v] of Object.entries(replacements)) {
        result = result.replace(new RegExp(escapeRegExp(`{${k}}`), "g"), v);
    }
    return result;
}

// tr is not a pure function, so need to tell AngularJS about it.
tr.$stateful = true;
