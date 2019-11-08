export const FRONT_PAGE_DEFAULT_LANGUAGE = "fi";

const translations: Record<string, {fi: string | {default: string, [index: string]: string | undefined}, en?: string}> = {
    "Add a user to this session": {fi: "Lisää käyttäjä istuntoon"},
    "and": {fi: "ja"},
    "Cancel": {fi: "Peruuta"},
    "Close": {fi: "Sulje"},
    "Continue": {fi: "Jatka"},
    "Create a new password": {fi: "Luo uusi salasana"},
    "Do not type anything here": {fi: "Älä kirjoita tähän mitään"},
    "Email or username": {fi: "Sähköpostiosoite tai käyttäjätunnus"},
    "Enter your name (Lastname Firstname)": {fi: "Kirjoita nimesi (Sukunimi Etunimi)"},
    "everyone": {fi: "kaikki"},
    "Examples": {fi: "Esimerkkejä"},
    "Finish": {fi: "Valmis"},
    "Haka login": {fi: "Haka-kirjautuminen"},
    "I forgot my password": {fi: "Unohdin salasanani"},
    "If you don't have an existing TIM or JYU account, you can create a TIM account here.": {fi: "Jos sinulla ei ole TIM-tiliä tai JYU-tunnusta, voit luoda TIM-tilin tässä."},
    "Introduction": {fi: "Esittely"},
    "JYU students and staff, please log in with JYU account:": {fi: "JYU opiskelijat ja henkilökunta, kirjautukaa JYU-tunnuksella:"},
    "Log in with JYU account": {fi: "Kirjaudu JYU-tunnuksella"},
    "Log in": {fi: "Kirjaudu sisään"},
    "Log out": {fi: "Kirjaudu ulos"},
    "Log": {fi: {self: "Kirjaudu", default: "Kirjaa"}},
    "My documents": {fi: "Omat dokumentit"},
    "Not a JYU student or staff member and don't have a TIM account?": {fi: "Etkö ole JYUn opiskelija tai henkilökuntaa eikä sinulla ole TIM-tiliä?"},
    "Now you can": {fi: "Nyt voit"},
    "other": {fi: "muu"},
    "others": {fi: "muuta"},
    "Others, please log in with your TIM account:": {fi: "Muut, kirjautukaa sisään TIM-tilillä:"},
    "out": {fi: "ulos"},
    "Password you received": {fi: "Salasana, jonka sait"},
    "Password": {fi: "Salasana"},
    "Please input your email address to receive a temporary password.": {fi: "Kirjoita sähköpostiosoitteesi saadaksesi väliaikaisen salasanan."},
    "Problems logging in?": {fi: "Ongelmia kirjautumisessa?"},
    "Programming languages": {fi: "Ohjelmointikieliä"},
    "refresh": {fi: "virkistää"},
    "Retype password": {fi: "Kirjoita salasana uudelleen"},
    "Retype the above password": {fi: "Kirjoita yllä oleva salasana uudelleen"},
    "Select your home organisation...": {fi: "Valitse kotiorganisaatiosi..."},
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
};

export type Lang = "fi" | "en";
export let language: {lang: Lang} = {lang: "en"};

export function tr(s: string, opts?: {plural?: boolean}): string {
    const entry = translations[s];
    if (!entry) {
        console.log(`Missing translation for: ${s}`);
        return s;
    }
    const e = entry[language.lang];
    if (!e) {
        return s;
    }
    if (typeof e === "string") {
        return e || s;
    } else if (opts) {
        for (const [k, v] of Object.entries(opts)) {
            if (v) {
                return e[k] || s;
            }
        }
        return e.default;
    } else {
        return e.default;
    }
}

// tr is not a pure function, so need to tell AngularJS about it.
tr.$stateful = true;
