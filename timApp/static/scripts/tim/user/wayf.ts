/* tslint:disable:variable-name */
/* tslint:disable:no-bitwise */

import * as t from "io-ts";
import {$http} from "../util/ngimport";
import {to, valueOr} from "../util/utils";
import {Users} from "./userService";

// Copyright (c) 2018, SWITCH
// License: BSD (see https://www.switch.ch/aai/support/tools/wayf/)

interface IIdp {
    name: string;
    entityID: string;
    SAML1SSOurl: string;
    logoURL: string;
    data: string;
    type?: string;
}

interface IWayfOptionalSettings {
    wayf_use_discovery_service: boolean;
    wayf_use_improved_drop_down_list: boolean;
    wayf_disable_remote_idp_logos: boolean;
    wayf_enable_entityid_matching: boolean;
    wayf_use_small_logo: boolean;
    wayf_width: number | string;
    wayf_height: number | string;
    wayf_background_color: string;
    wayf_border_color: string;
    wayf_font_color: string;
    wayf_font_size: number;
    wayf_hide_logo: boolean;
    wayf_auto_login: boolean;
    wayf_logged_in_message: string;
    wayf_auto_redirect_if_logged_in: boolean;
    wayf_hide_after_login: boolean;
    wayf_most_used_idps: string[];
    wayf_overwrite_last_used_idps_text: string;
    wayf_overwrite_most_used_idps_text: string;
    wayf_overwrite_checkbox_label_text: string;
    wayf_overwrite_submit_button_text: string;
    wayf_overwrite_intro_text: string;
    wayf_overwrite_from_other_federations_text: string;
    wayf_default_idp: string;
    wayf_num_last_used_idps: number;
    wayf_show_categories: boolean;
    wayf_hide_categories: string[];
    wayf_hide_idps: string[];
    wayf_unhide_idps: string[];
    wayf_show_remember_checkbox: boolean;
    wayf_force_remember_for_session: boolean;
    wayf_additional_idps: IIdp[];
    wayf_sp_samlDSURL: string;
    wayf_sp_samlACURL: string;
    wayf_use_disco_feed: boolean;
    wayf_discofeed_url: string;
}

interface IWayfSettings extends Partial<IWayfOptionalSettings> {
    wayf_sp_entityID: string;
    wayf_URL: string;
    wayf_return_url: string;
    wayf_sp_handlerURL: string;
}

const vars: IWayfSettings = {
    wayf_sp_entityID: getGETArgument("entityID") || "https://timdevs02-5.it.jyu.fi/saml",
    wayf_URL: "https://testsp.funet.fi/shibboleth/WAYF", // "https://haka.funet.fi/shibboleth/wayf.php",
    wayf_return_url: document.location.toString(),
    wayf_sp_handlerURL: "",  // not mandatory if wayf_use_discovery_service = true

    wayf_discofeed_url: "/getproxy?url=https://testsp.funet.fi/Shibboleth.sso/DiscoFeed",
    wayf_use_disco_feed: true,
    wayf_sp_samlDSURL: "/saml/sso",
    wayf_show_remember_checkbox: false,
};

// Essential settings
const wayf_sp_entityID = vars.wayf_sp_entityID;
const wayf_URL = vars.wayf_URL;
const wayf_return_url = vars.wayf_return_url;
const wayf_sp_handlerURL = vars.wayf_sp_handlerURL;

// Other settings
const wayf_use_discovery_service = valueOr(vars.wayf_use_discovery_service, true);
const wayf_use_improved_drop_down_list = valueOr(vars.wayf_use_improved_drop_down_list, false);
const wayf_disable_remote_idp_logos = valueOr(vars.wayf_disable_remote_idp_logos, false);
const wayf_enable_entityid_matching = valueOr(vars.wayf_enable_entityid_matching, true);
const wayf_use_small_logo = valueOr(vars.wayf_use_small_logo, true);
const wayf_width = valueOr(vars.wayf_width, "auto");
const wayf_height = valueOr(vars.wayf_height, "auto");
const wayf_background_color = valueOr(vars.wayf_background_color, "#F0F0F0");
const wayf_border_color = valueOr(vars.wayf_border_color, "#848484");
const wayf_font_color = valueOr(vars.wayf_font_color, "black");
const wayf_font_size = valueOr(vars.wayf_font_size, 12);
const wayf_hide_logo = valueOr(vars.wayf_hide_logo, false);
const wayf_auto_login = valueOr(vars.wayf_auto_login, true);
const wayf_logged_in_message = valueOr(vars.wayf_logged_in_message, "You are already authenticated.".replace(/%s/, wayf_return_url));
const wayf_auto_redirect_if_logged_in = valueOr(vars.wayf_auto_redirect_if_logged_in, false);
const wayf_hide_after_login = valueOr(vars.wayf_hide_after_login, true);
const wayf_most_used_idps = valueOr(vars.wayf_most_used_idps, []);
const wayf_overwrite_last_used_idps_text = valueOr(vars.wayf_overwrite_last_used_idps_text, "Last used");
const wayf_overwrite_most_used_idps_text = valueOr(vars.wayf_overwrite_most_used_idps_text, "Most often used Home Organisations");
const wayf_overwrite_checkbox_label_text = valueOr(vars.wayf_overwrite_checkbox_label_text, "Remember selection for this web browser session.");
const wayf_overwrite_submit_button_text = valueOr(vars.wayf_overwrite_submit_button_text, "Login");
const wayf_overwrite_intro_text = valueOr(vars.wayf_overwrite_intro_text, "Login with:");
const wayf_overwrite_from_other_federations_text = valueOr(vars.wayf_overwrite_from_other_federations_text, "From other federations");
const wayf_default_idp = valueOr(vars.wayf_default_idp, "");
const wayf_num_last_used_idps = valueOr(vars.wayf_num_last_used_idps, 3);
const wayf_show_categories = valueOr(vars.wayf_show_categories, true);
const wayf_hide_categories = valueOr(vars.wayf_hide_categories, []);
const wayf_hide_idps = valueOr(vars.wayf_hide_idps, []);
const wayf_unhide_idps = valueOr(vars.wayf_unhide_idps, []);
const wayf_show_remember_checkbox = valueOr(vars.wayf_show_remember_checkbox, true);
const wayf_force_remember_for_session = valueOr(vars.wayf_force_remember_for_session, false);
const wayf_additional_idps = valueOr(vars.wayf_additional_idps, []);
const wayf_sp_samlDSURL = valueOr(vars.wayf_sp_samlDSURL, wayf_sp_handlerURL + "/Login");
const wayf_sp_samlACURL = valueOr(vars.wayf_sp_samlACURL, wayf_sp_handlerURL + "/SAML/POST");
const wayf_use_disco_feed = valueOr(vars.wayf_use_disco_feed, false);
const wayf_discofeed_url = valueOr(vars.wayf_discofeed_url, "/Shibboleth.sso/DiscoFeed");

// Internal variables
let wayf_disco_feed_idps;
const wayf_categories: Record<string, {type: string, name: string} | undefined> = {
    Haka: {
        type: "category",
        name: "Haka",
    },
};

const wayf_check_login_state_function = () => Users.isLoggedIn();

interface IWayfIdp {
    type: string;
    name: string;
    logoURL: string;
    data: string;
    selected?: boolean;
}

function wayfEntry(obj: {type?: string, name: string, data: string, logoURL?: string}): IWayfIdp {
    return {
        ...obj,
        logoURL: obj.logoURL || "",
        type: obj.type || "Haka",
    };
}

const wayf_idps = {
    "https://idp.aalto.fi/idp/shibboleth": wayfEntry({
        name: "Aalto University",
        data: "aalto.fi Aalto University Aalto-universitetet Aalto-yliopisto",
    }),
    "https://tullbommen.arcada.fi/simplesaml/": wayfEntry({
        name: "Arcada",
        data: "arcada.fi Arcada Arcada Arcada",
    }),
    "https://idp.cou.fi/idp/shibboleth": wayfEntry({
        name: "Centria University of Applied Sciences",
        data: "cou.fi Centria University of Applied Sciences Centria yrkeshögskola Centria ammattikorkeakoulu",
    }),
    "https://salpa.certia.fi/idp/shibboleth": wayfEntry({
        name: "Certia Oy",
        data: "certia.fi Certia Oy Certia Oy Certia Oy",
    }),
    "https://idp.csc.fi/idp/shibboleth": wayfEntry({
        name: "CSC - IT Center for Science Ltd.",
        data: "csc.fi CSC - IT Center for Science Ltd. CSC - Tieteen tietotekniikan keskus Oy CSC - Tieteen tietotekniikan keskus Oy",
    }),
    "https://idp.diak.fi/idp/shibboleth": wayfEntry({
        name: "Diaconia University of Applied Sciences",
        data: "diak.fi Diaconia University of Applied Sciences Diakonia-ammattikorkeakoulu Diakonia-ammattikorkeakoulu",
    }),
    "http://fs.ymparisto.fi/adfs/services/trust": wayfEntry({
        name: "Finnish Environment Institute",
        data: "ymparisto.fi Finnish Environment Institute Finlands miljöcentral Suomen ympäristökeskus",
    }),
    "https://sipuli.fmi.fi/idp/shibboleth": wayfEntry({
        name: "Finnish Meteorological Institute",
        data: "fmi.fi Finnish Meteorological Institute Meteorologiska institutet Ilmatieteen laitos",
    }),
    "https://idp.oph.fi/idp/shibboleth": wayfEntry({
        name: "Finnish National Agency for Education",
        data: "oph.fi Finnish National Agency for Education Utbildningsstyrelsen Opetushallitus",
    }),
    "https://idp.funidata.fi/idp/shibboleth": wayfEntry({
        name: "Funidata IDP",
        data: "funidata.fi Funidata IDP Funidata IDP Funidata IDP",
    }),
    "http://fs.valtori.fi/adfs/services/trust": wayfEntry({
        name: "Government ICT Centre Valtori",
        data: "valtori.fi Government ICT Centre Valtori Statens center för informations- och kommunikationsteknik Valtion tieto- ja viestintätekniikkakeskus Valtori",
    }),
    "https://idp.haaga-helia.fi/idp/shibboleth": wayfEntry({
        name: "HAAGA-HELIA University of Applied Sciences",
        data: "haaga-helia.fi HAAGA-HELIA University of Applied Sciences HAAGA-HELIA  yrkeshögskola HAAGA-HELIA  ammattikorkeakoulu",
    }),
    "https://idp.shh.fi/idp/shibboleth": wayfEntry({
        name: "Hanken School of Economics",
        data: "shh.fi Hanken School of Economics Hanken Svenska handelshögskolan Hanken Svenska handelshögskolan",
    }),
    "https://rap.humak.fi/idp/shibboleth": wayfEntry({
        name: "HUMAK University of Applied Sciences",
        data: "humak.fi HUMAK University of Applied Sciences Humanistiska Yrkeshögskola Humanistinen ammattikorkeakoulu",
    }),
    "https://shibbo.hamk.fi/shibboleth": wayfEntry({
        name: "Häme University of Applied Sciences",
        data: "hamk.fi Häme University of Applied Sciences HAMK Yrkeshögskola Hämeen ammattikorkeakoulu",
    }),
    "https://kotusidp.vyv.fi/IDP/app/Haka": wayfEntry({
        name: "Institute for the Languages of Finland",
        data: "vyv.fi Institute for the Languages of Finland Institutet för de inhemska språken Kotimaisten kielten keskus",
    }),
    "https://idp2.jamk.fi/idp/shibboleth": wayfEntry({
        name: "Jyväskylä University of Applied Sciences",
        data: "jamk.fi Jyväskylä University of Applied Sciences JAMK Jyväskylän ammattikorkeakoulu",
    }),
    "https://kamidp01.kamit.fi/idp/shibboleth": wayfEntry({
        name: "Kajaani University of Applied Sciences",
        data: "kamit.fi Kajaani University of Applied Sciences Kajaanin ammattikorkeakoulu Kajaanin ammattikorkeakoulu",
    }),
    "https://idp.pkamk.fi/idp/shibboleth": wayfEntry({
        name: "Karelia University of Applied Sciences",
        data: "pkamk.fi Karelia University of Applied Sciences Karelia yrkehögskola Karelia-ammattikorkeakoulu",
    }),
    "https://sts.psshp.fi/idp/haka": wayfEntry({
        name: "Kuopio University Hospital",
        data: "psshp.fi Kuopio University Hospital Kuopio universitetssjukhus Kuopion yliopistollinen sairaala",
    }),
    "https://haka.lpt.fi/idp/shibboleth": wayfEntry({
        name: "Lahti University of Applied Sciences",
        data: "lpt.fi Lahti University of Applied Sciences Lahtis yrkeshögskola Lahden ammattikorkeakoulu",
    }),
    "https://idp.lapinamk.fi/idp/shibboleth": wayfEntry({
        name: "Lapland University of Applied Sciences",
        data: "lapinamk.fi Lapland University of Applied Sciences Lapplands Yrkeshögskola Lapin ammattikorkeakoulu",
    }),
    "https://idp.lut.fi": wayfEntry({
        name: "Lappeenranta University of Technology",
        data: "lut.fi Lappeenranta University of Technology Lappeenranta Tekniska Högskolan Lappeenrannan teknillinen yliopisto",
    }),
    "http://tunnistus.laurea.fi/adfs/services/trust": wayfEntry({
        name: "Laurea University of Applied Sciences",
        data: "laurea.fi Laurea University of Applied Sciences Laurea yrkehögskola Laurea-ammattikorkeakoulu",
    }),
    "https://idp.metropolia.fi/idp": wayfEntry({
        name: "Metropolia University of Applied Sciences",
        data: "metropolia.fi Metropolia University of Applied Sciences Metropolia yrkeshögskola Metropolia Ammattikorkeakoulu",
    }),
    "http://fs.narc.fi/adfs/services/trust": wayfEntry({
        name: "National Archives Service",
        data: "narc.fi National Archives Service Riksarkivet Kansallisarkisto",
    }),
    "http://fs.kavi.fi/adfs/services/trust": wayfEntry({
        name: "National Audiovisual institute",
        data: "kavi.fi National Audiovisual institute Nationella audiovisuella institutet Kansallinen audiovisuaalinen instituutti",
    }),
    "https://tunnistus.mpkkfu.fi/idp/shibboleth": wayfEntry({
        name: "National Defence University",
        data: "mpkkfu.fi National Defence University Förvarshögskolan Maanpuolustuskorkeakoulu",
    }),
    "https://tunnistus.thl.fi/idp": wayfEntry({
        name: "National Institute for Health and Welfare",
        data: "thl.fi National Institute for Health and Welfare Institutet för hälsa och välfärd Terveyden ja hyvinvoinnin laitos",
    }),
    "https://idp1.novia.fi/idp/shibboleth": wayfEntry({
        name: "Novia University of Applied Sciences",
        data: "novia.fi Novia University of Applied Sciences Yrkeshögskolan Novia Yrkeshögskolan Novia",
    }),
    "https://idp.oamk.fi/idp/shibboleth": wayfEntry({
        name: "Oulu University of Applied Sciences",
        data: "oamk.fi Oulu University of Applied Sciences Yrkeshögskolan i Uleåborg Oulun ammattikorkeakoulu",
    }),
    "https://tunnistus.smedu.fi/idp/shibboleth": wayfEntry({
        name: "Police University College",
        data: "smedu.fi Police University College Polisyrkeshögskolan Poliisiammattikorkeakoulu",
    }),
    "https://haka.saimia.fi/idp/shibboleth": wayfEntry({
        name: "Saimaa University of Applied Sciences",
        logoURL: "https://idp.saimia.fi/idp/images/saimaa_uas_logo_16x16px.png",
        data: "saimia.fi Saimaa University of Applied Sciences Yrkehögskola Saimaa Saimaan ammattikorkeakoulu",
    }),
    "https://idp.samk.fi/idp/shibboleth": wayfEntry({
        name: "Satakunta University of Applied Sciences",
        data: "samk.fi Satakunta University of Applied Sciences samk Satakunnan ammattikorkeakoulu samk Satakunnan ammattikorkeakoulu samk",
    }),
    "https://idp.savonia.fi/idp/shibboleth": wayfEntry({
        name: "Savonia University of Applied Sciences",
        data: "savonia.fi Savonia University of Applied Sciences Savonia Yrkeshögskola Savonia-ammattikorkeakoulu",
    }),
    "https://idp2.epedu.fi/idp/shibboleth": wayfEntry({
        name: "Seinäjoki University of Applied Sciences",
        data: "epedu.fi Seinäjoki University of Applied Sciences Seinäjoki yrkeshögskola Seinäjoen ammattikorkeakoulu",
    }),
    "https://xidp.xamk.fi/idp/shibboleth": wayfEntry({
        name: "South-Eastern Finland University of Applied Sciences",
        data: "xamk.fi South-Eastern Finland University of Applied Sciences xamk xamk.fi south-eastern finland Kaakkois-Suomen ammattikorkeakoulu xamk xamk.fi Kaakkois-Suomen ammattikorkeakoulu xamk xamk.fi kaakkois-suomi",
    }),
    "https://idp.tuni.fi/idp/shibboleth": wayfEntry({
        name: "Tampere Universities",
        data: "tuni.fi Tampere Universities Tampere tuni.fi TUNI Tampere3 Tampere Universities Tampereen korkeakoulut Tampere tuni.fi TUNI Tampere3",
    }),
    "https://idp2.tamk.fi": wayfEntry({
        name: "Tampere University of Applied Sciences",
        data: "tamk.fi Tampere University of Applied Sciences Tampereen ammattikorkeakoulu Tampereen ammattikorkeakoulu",
    }),
    "https://idp.tut.fi/shibboleth2": wayfEntry({
        name: "Tampere University of Technology",
        logoURL: "https://www.tut.fi/TTY-ratas/TTY_ratas-16x16.png",
        data: "tut.fi Tampere University of Technology TUT Tammerfors tekniska universitet Tampereen teknillinen yliopisto TTY TTKK",
    }),
    "https://tunnistus.pelastusopisto.fi/idp/shibboleth": wayfEntry({
        name: "The Emergency Services College",
        data: "pelastusopisto.fi The Emergency Services College Räddningsinstitutet Pelastusopisto",
    }),
    "https://idp1.turkuamk.fi/idp/shibboleth": wayfEntry({
        name: "Turku University of Applied Sciences",
        data: "turkuamk.fi Turku University of Applied Sciences Åbo yrkeshögskola Turun ammattikorkeakoulu",
    }),
    "https://idp.uef.fi/idp/shibboleth": wayfEntry({
        name: "University of Eastern Finland",
        data: "uef.fi University of Eastern Finland Östra Finlands universitet Itä-Suomen yliopisto",
    }),
    "https://login.helsinki.fi/shibboleth": wayfEntry({
        name: "University of Helsinki",
        data: "helsinki.fi University of Helsinki Helsingfors universitet Helsingin yliopisto",
    }),
    "https://login.jyu.fi/idp/shibboleth": wayfEntry({
        name: "University of Jyväskylä",
        data: "jyu.fi University of Jyväskylä Jyväskylä universitet Jyväskylän yliopisto",
    }),
    "https://idp.ulapland.fi/idp/shibboleth": wayfEntry({
        name: "University of Lapland",
        data: "ulapland.fi University of Lapland Lapplands universitet Lapin yliopisto",
    }),
    "https://login.oulu.fi/idp/shibboleth": wayfEntry({
        name: "University of Oulu",
        data: "oulu.fi University of Oulu Uleåborgs universitet Oulun yliopisto",
    }),
    "https://shibboleth.uta.fi/idp/shibboleth": wayfEntry({
        name: "University of Tampere",
        data: "uta.fi University of Tampere uta.fi uta tay Tammerfors universitet uta.fi uta tay Tampereen yliopisto uta.fi uta tay",
    }),
    "https://idp.uniarts.fi/shibboleth": wayfEntry({
        name: "University of the Arts Helsinki",
        data: "uniarts.fi University of the Arts Helsinki Konstuniversitetet Taideyliopisto",
    }),
    "https://sso.utu.fi/sso-idp": wayfEntry({
        name: "University of Turku",
        data: "utu.fi University of Turku utu Åbo universitet utu Turun yliopisto utu",
    }),
    "https://login.uwasa.fi/idp/shibboleth": wayfEntry({
        name: "University of Vaasa",
        data: "uwasa.fi University of Vaasa Vasa Universitet Vaasan yliopisto",
    }),
    "https://idp.vamk.fi/saml2/idp/metadata.php": wayfEntry({
        name: "Vaasa University of Applied Sciences",
        data: "vamk.fi Vaasa University of Applied Sciences Vasa yrkeshögskola Vaasan ammattikorkeakoulu",
    }),
    "https://idp.abo.fi/idp/shibboleth": wayfEntry({
        name: "Åbo Akademi University",
        data: "abo.fi Åbo Akademi University ÅA Åbo Akademi ÅA Åbo Akademi",
    }),
};

const KnownIdpNames = t.keyof(wayf_idps);

const wayf_other_fed_idps: Record<string, IWayfIdp | undefined> = {};

// Functions
function redirectTo(url: string) {
    // Make sure the redirect always is being done in parent window
    if (window.parent) {
        window.parent.location.href = url;
    } else {
        window.location.href = url;
    }
}

function submitForm(eventObj: Event) {

    const idPList = eventObj.currentTarget as HTMLFormElement;
    if (!(idPList.user_idp instanceof HTMLSelectElement)) {
        alert("select element not found.");
        return false;
    }
    if (idPList.user_idp.selectedIndex == 0) {
        alert("You must select a valid Home Organisation.");
        return false;
    }

    const option = idPList.user_idp[idPList.user_idp.selectedIndex] as HTMLOptionElement;
    const selectedIdP = option.value;
    // Set local cookie
    setDomainSAMLDomainCookie(selectedIdP);

    // User chose federation IdP entry
    if (KnownIdpNames.is(selectedIdP)) {
        return true;
    }

    // User chose IdP from other federation
    let redirect_url: string;

    // Redirect user to SP handler
    if (wayf_use_discovery_service) {

        const entityIDGETParam = getGETArgument("entityID");
        const returnGETParam = getGETArgument("return");
        if (entityIDGETParam != "" && returnGETParam != "") {
            redirect_url = returnGETParam;
        } else {
            redirect_url = wayf_sp_samlDSURL;
            redirect_url += getGETArgumentSeparator(redirect_url) + "target=" + encodeURIComponent(wayf_return_url);
        }

        // Prevent default submit action
        eventObj.preventDefault();

        // Append selected Identity Provider
        redirect_url += "&entityID=" + encodeURIComponent(selectedIdP);

        redirectTo(redirect_url);
    } else {
        redirect_url = wayf_sp_handlerURL + "?providerId="
            + encodeURIComponent(selectedIdP)
            + "&target=" + encodeURIComponent(wayf_return_url);

        // Prevent default submit action
        eventObj.preventDefault();

        redirectTo(redirect_url);
    }

    // If input type button is used for submit, we must return false
    return false;
}

function isEmptyObject(obj: Record<string, unknown>) {

    if (typeof (obj) != "object") {
        return true;
    }
    return Object.keys(obj).length === 0;
}

function isAllowedIdP(IdP: string) {

    let type = "";
    const other = wayf_other_fed_idps[IdP];
    if (KnownIdpNames.is(IdP)) {
        type = wayf_idps[IdP].type;
    } else if (other) {
        type = other.type;
    }

    // Check if IdP should be hidden
    if (wayf_hide_idps.includes(IdP)) {
        return false;
    }

    // Check if category should be hidden
    // Check if IdP is unhidden in this category
    for (const ctg of wayf_hide_categories) {

        if (ctg === "all" || ctg == type) {

            if (wayf_unhide_idps.includes(IdP)) {
                return true;
            }

            // If IdP is not unhidden, the default applies
            return false;
        }
    }

    // Default
    return true;
}

function setDomainSAMLDomainCookie(entityID: string) {
    // Create and store SAML domain cookie on host where WAYF is embedded
    let currentDomainCookie = getCookie("_saml_idp");
    const encodedEntityID = encodeBase64(entityID);
    if (currentDomainCookie == null) {
        currentDomainCookie = "";
    }

    // Ensure current IdP is not already in array
    const currentIdPs = currentDomainCookie.split(" ");
    const newIdPs = [];
    for (const idp of currentIdPs) {
        if (idp != encodedEntityID && idp != "") {
            newIdPs.push(idp);
        }
    }

    // Add new IdP
    newIdPs.push(encodedEntityID);

    // Ensure array is no longer than 5 IdPs
    while (newIdPs.length > 5) {
        newIdPs.shift();
    }

    // Compose cookie value
    let newDomainCookie = "";
    for (const newIdP of newIdPs) {
        newDomainCookie += newIdP + " ";
    }

    // Set cookie value
    setCookie("_saml_idp", newDomainCookie.trim(), 100);
}

function setCookie(c_name: string, value: string, expiredays: number) {
    const exdate = new Date();
    exdate.setDate(exdate.getDate() + expiredays);
    document.cookie = c_name + "=" + escape(value) +
        ((expiredays == null) ? "" : "; expires=" + exdate.toUTCString());
}

function getCookie(check_name: string) {
    // First we split the cookie up into name/value pairs
    // Note: document.cookie only returns name=value, not the other components
    const a_all_cookies = document.cookie.split(";");
    let cookie_value = "";

    for (const c of a_all_cookies) {
        // now we'll split apart each name=value pair
        const a_temp_cookie = c.split("=");

        // and trim left/right whitespace while we're at it
        const cookie_name = a_temp_cookie[0].replace(/^\s+|\s+$/g, "");

        // if the extracted name matches passed check_name
        if (cookie_name == check_name) {
            // We need to handle case where cookie has no value but exists (no = sign, that is):
            if (a_temp_cookie.length > 1) {
                cookie_value = unescape(a_temp_cookie[1].replace(/^\s+|\s+$/g, ""));
            }
            // note that in cases where cookie is initialized but no value, null is returned
            return cookie_value;
        }
    }

    return null;
}

// Loads Identity Provider from DiscoFeed and adds them to additional IdPs
async function loadDiscoFeedIdPs() {
    const result = await to($http.get<IDiscoveryFeedEntry[]>(wayf_discofeed_url));
    if (result.ok) {
        return result.result.data;
    } else {
        return [];
    }
}

// Adds unknown IdPs to wayf_additional_idps and hides IdPs that are not
// contained in the Discovery Feed
function processDiscoFeedIdPs(IdPs: IDiscoveryFeedEntry[]) {

    if (typeof (IdPs) === "undefined") {
        return;
    }

    // Hide IdPs that are not in the Discovery Feed
    for (const entityID of Object.keys(wayf_idps)) {
        let foundIdP = false;
        for (const idp of IdPs) {
            if (idp.entityID == entityID) {
                foundIdP = true;
            }
        }

        if (!foundIdP) {
            wayf_hide_idps.push(entityID);
        }
    }

    // Add unknown IdPs to wayf_additional_idps
    for (const idp of IdPs) {

        // Skip IdPs that are already known
        if (KnownIdpNames.is(idp.entityID)) {
            continue;
        }

        const newIdP = getIdPFromDiscoFeedEntry(idp);

        wayf_additional_idps.push(newIdP);
    }
}

interface IDiscoveryFeedEntry {
    entityID: string;
    DisplayNames?: Array<{value: string, lang: string}>;
    Keywords?: Array<{value: string}>;
    Logos?: Array<{value: string, height: number, width: number}>;
}

function getIdPFromDiscoFeedEntry(IdPData: IDiscoveryFeedEntry) {
    let name = IdPData.entityID;
    let name_default = "";
    let name_requested = "";
    let data = "";
    let logo = "";

    if (IdPData.DisplayNames) {
        for (const dname of IdPData.DisplayNames) {

            name = dname.value;

            if (dname.lang == "en") {
                name_requested = name;
            } else if (dname.lang == "en") {
                name_default = name;
            }

            data += " " + dname.value;
        }

        if (name_requested != "") {
            name = name_requested;
        } else if (name_default != "") {
            name = name_default;
        }
    }

    if (IdPData.Keywords) {
        for (const k of IdPData.Keywords) {
            data += " " + k.value;
        }
    }

    if (IdPData.Logos) {
        for (const logodata of IdPData.Logos) {
            if (logodata.height == 16 && logodata.width == 16) {
                logo = logodata.value;
            }
        }
    }

    return {
        entityID: IdPData.entityID,
        name: name,
        type: "unknown",
        SAML1SSOurl: "https://this.url.does.not.exist/test",
        data: data,
        logoURL: logo,
    };
}

// Sorts Discovery feed entries
function sortEntities(a: IIdp, b: IIdp) {
    const nameA = a.name.toLowerCase();
    const nameB = b.name.toLowerCase();

    if (nameA < nameB) {
        return -1;
    }

    if (nameA > nameB) {
        return 1;
    }

    return 0;
}

// Returns true if user is logged in
function isUserLoggedIn() {
    return wayf_check_login_state_function();
}

function encodeBase64(input: string) {
    return btoa(input);
}

function decodeBase64(input: string) {
    return atob(input);
}

function getGETArgument(name: string) {
    name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
    const regexString = "[\\?&]" + name + "=([^&#]*)";
    const regex = new RegExp(regexString);
    const results = regex.exec(window.location.href);

    if (results == null) {
        return "";
    } else {
        return decodeURIComponent(results[1]);
    }
}

function getGETArgumentSeparator(url: string) {
    if (url.indexOf("?") >= 0) {
        return "&";
    } else {
        return "?";
    }
}

function getOptionHTML(entityID: string) {

    let IdPData;
    const other_fed = wayf_other_fed_idps[entityID];
    if (KnownIdpNames.is(entityID)) {
        IdPData = wayf_idps[entityID];
    } else if (other_fed) {
        IdPData = other_fed;
    } else {
        return "";
    }

    let data = "";
    let logo = "";
    let selected = "";

    if (IdPData.data) {
        data = ` data="${IdPData.data}"`;
    }

    if (IdPData.logoURL) {
        logo = ` logo="${IdPData.logoURL}"`;
    }

    if (IdPData.selected) {
        selected = ' selected="selected"';
    }

    return `<option value="${entityID}"${data}${logo}${selected}>${IdPData.name}</option>`;
}

export async function createWayfElement() {
    // Get GET parameters that maybe are set by Shibboleth
    const returnGETParam = getGETArgument("return");

    // Check if user is logged in already:
    const user_logged_in = isUserLoggedIn();

    // Check if user is authenticated already and should
    // be redirected to wayf_return_url
    if (
        user_logged_in
        && wayf_auto_redirect_if_logged_in
    ) {
        redirectTo(wayf_return_url);
        return;
    }

    // Check if user is authenticated already and
    // whether something has to be drawn
    if (
        wayf_hide_after_login
        && user_logged_in
        && wayf_logged_in_message === ""
    ) {

        // Exit script without drawing
        return;
    }
    let wayf_html = "";

    function writeHTML(a: string) {
        wayf_html += a;
    }

    function writeOptGroup(IdPElements: string, category: string) {

        const cat = wayf_categories[category];
        if (!cat) {
            writeHTML(IdPElements);
            return;
        }

        if (IdPElements === "") {
            return;
        }

        const categoryName = cat.name;

        if (wayf_show_categories) {
            writeHTML(`<optgroup label="${categoryName}">`);
        }

        writeHTML(IdPElements);

        if (wayf_show_categories) {
            writeHTML("</optgroup>");
        }
    }

    // Now start generating the HTML for outer box
    if (
        wayf_hide_after_login
        && user_logged_in
    ) {
        writeHTML(`<div style="background:${wayf_background_color};border-style: solid;border-color: ${wayf_border_color};border-width: 1px;padding: 10px; height: auto;width: ${wayf_width};text-align: left;overflow: hidden;">`);
    } else {
        writeHTML(`<div style="background:${wayf_background_color};border-style: solid;border-color: ${wayf_border_color};border-width: 1px;padding: 10px; height: ${wayf_height};width: ${wayf_width};text-align: left;overflow: hidden;">`);
    }

    // Do we have to display the logo
    if (!wayf_hide_logo) {

        // Write header of logo div
        writeHTML('<div style="float: right;"><a href="https://wiki.eduuni.fi/display/CSCHAKA" target="_blank" style="border:0; margin-bottom: 4px;">');

        // Which size of the logo should we display
        let embeddedLogoURL = "";
        if (wayf_use_small_logo) {
            embeddedLogoURL = "https://haka.funet.fi/shibboleth/images/Haka_nega_tiivis_pieni.svg";
        } else {
            embeddedLogoURL = "https://haka.funet.fi/shibboleth/images/virtu_symboli_text_Valtori_pieni.png";
        }

        // Only show logo if it is not empty
        if (embeddedLogoURL != "") {
            writeHTML(`<img src="${embeddedLogoURL}" alt="Federation Logo" style="border:0; margin-bottom: 4px;">`);
        }

        // Write footer of logo div
        writeHTML("</a></div>");
    }

    // Start login check
    // If session exists, we only draw the logged_in_message
    if (
        wayf_hide_after_login
        && user_logged_in
    ) {
        writeHTML(`<p style="float:left;font-size:${wayf_font_size}px;color:${wayf_font_color};">${wayf_logged_in_message}</p>`);

    } else {
        // Else draw embedded WAYF

        // Draw intro text
        writeHTML(`<label for="user_idp" style="float:left; min-width:80px; font-size:${wayf_font_size}px;color:${wayf_font_color};">${wayf_overwrite_intro_text}</label>`);

        let wayf_authReq_URL = "";

        if (wayf_use_discovery_service) {
            // New SAML Discovery Service protocol

            wayf_authReq_URL = wayf_URL;
            const entityIDGETParam = getGETArgument("entityID");
            // Use GET arguments or use configuration parameters
            if (entityIDGETParam != "" && returnGETParam != "") {
                wayf_authReq_URL += "?entityID=" + encodeURIComponent(entityIDGETParam);
                wayf_authReq_URL += "&amp;return=" + encodeURIComponent(returnGETParam);
            } else {
                let return_url = wayf_sp_samlDSURL + getGETArgumentSeparator(wayf_sp_samlDSURL);
                return_url += "SAMLDS=1&target=" + encodeURIComponent(wayf_return_url);
                wayf_authReq_URL += "?entityID=" + encodeURIComponent(wayf_sp_entityID);
                wayf_authReq_URL += "&amp;return=" + encodeURIComponent(return_url);
            }
        } else {
            // Old Shibboleth WAYF protocol
            wayf_authReq_URL = wayf_URL;
            wayf_authReq_URL += "?providerId=" + encodeURIComponent(wayf_sp_entityID);
            wayf_authReq_URL += "&amp;target=" + encodeURIComponent(wayf_return_url);
            wayf_authReq_URL += "&amp;shire=" + encodeURIComponent(wayf_sp_samlACURL);
            wayf_authReq_URL += "&amp;time=1571211916";
        }

        // Do auto login if redirect cookie exists
        if ("" != "" && wayf_auto_login) {

            // Redirect user automatically to WAYF
            const redirect_url = wayf_authReq_URL.replace(/&amp;/g, "&");

            redirectTo(redirect_url);
            return;
        }

        // Get local cookie
        const saml_idp_cookie = getCookie("_saml_idp");
        let last_idp = "";
        let last_idps: string[] = [];

        // Get last used IdP from local host cookie
        if (saml_idp_cookie && saml_idp_cookie.length > 0) {
            last_idps = saml_idp_cookie.split(/[ \+]/);
            if (last_idps.length > 0) {
                last_idp = last_idps[(last_idps.length - 1)];
                if (last_idp.length > 0) {
                    last_idp = decodeBase64(last_idp);
                }
            }
        }

        // Load additional IdPs from DiscoFeed if feature is enabled
        if (wayf_use_disco_feed) {
            wayf_disco_feed_idps = await loadDiscoFeedIdPs();

            // Hide IdPs for which SP doesnt have metadata and add unknown IdPs
            // Add to additional IdPs
            processDiscoFeedIdPs(wayf_disco_feed_idps);
        }

        // Sort additional IdPs and add IdPs to sorted associative array of other federation IdPs
        if (wayf_additional_idps.length > 0) {
            wayf_additional_idps.sort(sortEntities);

            for (const IdP of wayf_additional_idps) {

                if (!IdP) {
                    continue;
                }

                let selected = false;
                if (IdP.entityID && last_idp !== "" && IdP.entityID == last_idp) {
                    selected = true;
                } else if (IdP.entityID && last_idp === "" && IdP.entityID == wayf_default_idp) {
                    selected = true;
                }

                wayf_other_fed_idps[IdP.entityID] = {
                    ...IdP,
                    data: IdP.data || IdP.name,
                    type: IdP.type || "unknown",
                    selected: selected,
                };
            }
        }

        // Set default IdP if no last used IdP exists
        if (last_idp === "" && wayf_default_idp !== "") {
            if (KnownIdpNames.is(wayf_default_idp)) {
                wayf_idps[wayf_default_idp].selected = true;
            }
        }

        writeHTML(`<form method="post" target="_parent" action="${wayf_authReq_URL}">`);
        writeHTML('<input name="request_type" type="hidden" value="embedded">');
        writeHTML('<select class="form-control" id="user_idp" name="user_idp" style="margin-top: 6px; width: 100%;">');

        // Add first entry: "Select your IdP..."
        writeHTML('<option value="-">Select the organisation you are affiliated with ...</option>');

        // Last used
        if (wayf_show_categories && wayf_num_last_used_idps > 0 && last_idps.length > 0) {

            // Add new category
            const category = "wayf_last_used_idps";
            wayf_categories.wayf_last_used_idps = {
                type: category,
                name: wayf_overwrite_last_used_idps_text,
            };

            let IdPElements = "";
            let counter = wayf_num_last_used_idps;
            for (let i = (last_idps.length - 1); i >= 0; i--) {

                if (counter <= 0) {
                    break;
                }

                const currentIdP = decodeBase64(last_idps[i]);

                // Skip if IdP is hidden explicitly
                if (!isAllowedIdP(currentIdP)) {
                    continue;
                }

                // Add option
                const content = getOptionHTML(currentIdP);

                if (content != "") {
                    counter--;
                    IdPElements += content;
                }

            }

            writeOptGroup(IdPElements, category);
        }

        // Most used and Favourites
        if (wayf_show_categories && wayf_most_used_idps.length > 0) {

            // Add new category
            const category = "wayf_most_used_idps";
            wayf_categories.wayf_most_used_idps = {
                type: category,
                name: wayf_overwrite_most_used_idps_text,
            };

            // Show most used IdPs in the order they are defined
            let IdPElements = "";
            for (const currentIdP of wayf_most_used_idps) {

                // Add option if IdP exists in lists of IdPs
                if (KnownIdpNames.is(currentIdP) || wayf_other_fed_idps[currentIdP]) {
                    IdPElements += getOptionHTML(currentIdP);
                }
            }

            writeOptGroup(IdPElements, category);
        }

        {
            // Draw drop down list
            let category = "";
            let IdPElements = "";
            for (const [entityID, value] of Object.entries(wayf_idps)) {

                const idp_type = value.type;

                // Draw category
                if (category != idp_type) {

                    // Finish category if a new one starts that exists
                    if (IdPElements != "") {
                        writeOptGroup(IdPElements, category);
                    }

                    // Reset content
                    IdPElements = "";
                }

                // Add IdP if it is allowed
                if (isAllowedIdP(entityID)) {
                    IdPElements += getOptionHTML(entityID);
                }

                // Set current category/type
                category = idp_type;
            }
            writeOptGroup(IdPElements, category);
        }
        {

            // Output last remaining element

            // Show IdPs from other federations
            if (!isEmptyObject(wayf_other_fed_idps)) {

                // Add new category
                const category = "wayf_other_federations_idps";
                wayf_categories.wayf_other_federations_idps = {
                    type: category,
                    name: wayf_overwrite_from_other_federations_text,
                };

                // Show additional IdPs
                let IdPElements = "";
                for (const entityID of Object.keys(wayf_other_fed_idps)) {
                    if (isAllowedIdP(entityID)) {
                        IdPElements += getOptionHTML(entityID);
                    }
                }

                writeOptGroup(IdPElements, category);
            }
        }
        writeHTML("</select>");

        // Do we have to show the remember settings checkbox?
        if (wayf_show_remember_checkbox) {

            // Draw checkbox table
            writeHTML('<div style="float: left;margin-top:6px;"><table style="border: 0; border-collapse: collapse;"><tr><td style="vertical-align: top;">');

            // Is the checkbox forced to be checked
            if (wayf_force_remember_for_session) {
                // First draw the dummy checkbox ...
                writeHTML('<input id="wayf_remember_checkbox" type="checkbox" name="session_dummy" value="true" checked="checked" disabled="disabled" style="margin:2px 2px 0 0; border: 0; padding:0;">');
                // ... and now the real but hidden checkbox
                writeHTML('<input type="hidden" name="session" value="true">');
            } else {
                writeHTML('<input id="wayf_remember_checkbox" type="checkbox" name="session" value="true"  style="margin:2px 2px 0 0; border: 0; padding:0;">');
            }

            // Draw label
            writeHTML(`</td><td style="vertical-align: top;"><label for="wayf_remember_checkbox" id="wayf_remember_checkbox_label" style="font-size:${wayf_font_size}px;color:${wayf_font_color};">${wayf_overwrite_checkbox_label_text}</label>`);

            writeHTML("</td></tr></table></div>");
        } else if (wayf_force_remember_for_session) {
            // Is the checkbox forced to be checked but hidden
            writeHTML('<input id="wayf_remember_checkbox" type="hidden" name="session" value="true">');
        }

        // Draw submit button
        writeHTML(`<input class="timButton" type="submit" name="Login" accesskey="s" value="${wayf_overwrite_submit_button_text}" style="float: right; margin-top:6px;">`);

        // Close form
        writeHTML("</form>");

    }  // End login check

    // Close box
    writeHTML("</div>");
    const div = document.createElement("div");
    div.innerHTML = wayf_html;
    div.querySelector("form")!.addEventListener("submit", (eventObj) => {
        return submitForm(eventObj);
    });
    return div;
}
