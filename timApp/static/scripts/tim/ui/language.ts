export const FRONT_PAGE_DEFAULT_LANGUAGE = "fi";

const translations: Record<string, {fi: string, en?: string}> = {
    "Add a user to this session": {fi: "Lisää käyttäjä istuntoon"},
    "Cancel": {fi: "Peruuta"},
    "Close": {fi: "Sulje"},
    "Continue": {fi: "Jatka"},
    "Create a new password": {fi: "Luo uusi salasana"},
    "Do not type anything here": {fi: "Älä kirjoita tähän mitään"},
    "Email or username": {fi: "Sähköpostiosoite tai käyttäjätunnus"},
    "Enter your name (Lastname Firstname)": {fi: "Kirjoita nimesi (Sukunimi Etunimi)"},
    "Finish": {fi: "Valmis"},
    "Haka login": {fi: "Haka-kirjautuminen"},
    "I forgot my password": {fi: "Unohdin salasanani"},
    "If you don't have an existing TIM or JYU account, you can create a TIM account here.": {fi: "Jos sinulla ei ole TIM-tiliä tai JYU-tunnusta, voit luoda TIM-tilin tässä."},
    "JYU students and staff, please log in with JYU account:": {fi: "JYU opiskelijat ja henkilökunta, kirjautukaa JYU-tunnuksella:"},
    "Log in with JYU account": {fi: "Kirjaudu JYU-tunnuksella"},
    "Log in": {fi: "Kirjaudu sisään"},
    "Not a JYU student or staff member and don't have a TIM account?": {fi: "Etkö ole JYUn opiskelija tai henkilökuntaa eikä sinulla ole TIM-tiliä?"},
    "Now you can": {fi: "Nyt voit"},
    "Others, please log in with your TIM account:": {fi: "Muut, kirjautukaa sisään TIM-tilillä:"},
    "Password you received": {fi: "Salasana, jonka sait"},
    "Password": {fi: "Salasana"},
    "Please input your email address to receive a temporary password.": {fi: "Kirjoita sähköpostiosoitteesi saadaksesi väliaikaisen salasanan."},
    "Problems logging in?": {fi: "Ongelmia kirjautumisessa?"},
    "refresh": {fi: "virkistää"},
    "Retype password": {fi: "Kirjoita salasana uudelleen"},
    "Retype the above password": {fi: "Kirjoita yllä oleva salasana uudelleen"},
    "Select your home organisation...": {fi: "Valitse kotiorganisaatiosi..."},
    "Sign up": {fi: "Luo tili"},
    "Thank you!": {fi: "Kiitos!"},
    "the page to log in.": {fi: "sivun kirjautuaksesi sisään."},
    "TIM sent you a temporary password. Please check your email and type the password below to continue.": {fi: "TIM lähetti sinulle väliaikaisen salasanan. Katso sähköpostisi ja kirjoita salasana alle jatkaaksesi."},
    "To reset password, enter your email or username first.": {fi: "Salasanan vaihtamiseksi kirjoita sähköpostiosoitteesi tai käyttäjätunnuksesi."},
    "Your information was updated successfully.": {fi: "Tietosi päivitettiin onnistuneesti."},
    "Your name": {fi: "Nimesi"},
    "What is TIM?": {fi: "Mikä on TIM?"},
    "TIM is a document-based cloud service for producing interactive materials.": {fi: "TIM on dokumenttipohjainen pilvipalvelu interaktiivisten materiaalien tuottamiseksi."},
    "Introduction": {fi: "Esittely"},
    "User guide": {fi: "Käyttöohje"},
    "Examples": {fi: "Esimerkkejä"},
    "TIM's possibilities": {fi: "TIMin mahdollisuuksia"},
    "Programming languages": {fi: "Ohjelmointikieliä"},
    "Usage in different subjects": {fi: "Käyttö eri oppiaineissa"},
};

export type Lang = "fi" | "en";
export let language: {lang: Lang} = {lang: "en"};

export function tr(s: string) {
    const entry = translations[s];
    if (!entry) {
        console.log(`Missing translation for: ${s}`);
        return s;
    }
    return entry[language.lang] || s;
}

// tr is not a pure function, so need to tell AngularJS about it.
tr.$stateful = true;
