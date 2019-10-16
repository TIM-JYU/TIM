(function(global, factory) {
   if (typeof global.define === 'function' && global.define.amd) {
       define("wayfGlobalObject", [], global);
       define("embeddedWayf", ["jquery", "wayfGlobalObject"], factory);
   } else {
       typeof global.$ === 'function' ? factory(global.$, global) :
       typeof global.jQuery === 'function' ? factory(global.jQuery, global) :
       typeof global.jquery === 'function' ? factory(global.jquery, global) :
       factory(null, global); // allow dynamic loading of the libraries on default
   }
})(this, function($, global) {


// Copyright (c) 2018, SWITCH
// To use this JavaScript, please access:
// https://haka.funet.fi/shibboleth/wayf.php/embedded-wayf.js/snippet.html// and copy/paste the resulting HTML snippet to an unprotected web page that
// you want the embedded WAYF to be displayed

// ############################################################################

// Declare all global variables

// Essential settings
var wayf_sp_entityID = global.wayf_sp_entityID;
var wayf_URL = global.wayf_URL;
var wayf_return_url = global.wayf_return_url;
var wayf_sp_handlerURL = global.wayf_sp_handlerURL;

// Other settings
var wayf_use_discovery_service = global.wayf_use_discovery_service;
var wayf_use_improved_drop_down_list = global.wayf_use_improved_drop_down_list;
var wayf_disable_remote_idp_logos = global.wayf_disable_remote_idp_logos;
var wayf_enable_entityid_matching = global.wayf_enable_entityid_matching;
var wayf_use_small_logo = global.wayf_use_small_logo;
var wayf_width = global.wayf_width;
var wayf_height = global.wayf_height;
var wayf_background_color = global.wayf_background_color;
var wayf_border_color = global.wayf_border_color;
var wayf_font_color = global.wayf_font_color;
var wayf_font_size = global.wayf_font_size;
var wayf_hide_logo = global.wayf_hide_logo;
var wayf_auto_login = global.wayf_auto_login;
var wayf_logged_in_messsage = global.wayf_logged_in_messsage;
var wayf_auto_redirect_if_logged_in = global.wayf_auto_redirect_if_logged_in;
var wayf_hide_after_login = global.wayf_hide_after_login;
var wayf_most_used_idps = global.wayf_most_used_idps;
var wayf_overwrite_last_used_idps_text = global.wayf_overwrite_last_used_idps_text;
var wayf_overwrite_most_used_idps_text = global.wayf_overwrite_most_used_idps_text;
var wayf_overwrite_checkbox_label_text = global.wayf_overwrite_checkbox_label_text;
var wayf_overwrite_submit_button_text = global.wayf_overwrite_submit_button_text;
var wayf_overwrite_intro_text = global.wayf_overwrite_intro_text;
var wayf_overwrite_from_other_federations_text = global.wayf_overwrite_from_other_federations_text;
var wayf_default_idp = global.wayf_default_idp;
var wayf_num_last_used_idps = global.wayf_num_last_used_idps;
var wayf_show_categories = global.wayf_show_categories;
var wayf_hide_categories = global.wayf_hide_categories;
var wayf_hide_idps = global.wayf_hide_idps;
var wayf_unhide_idps = global.wayf_unhide_idps;
var wayf_show_remember_checkbox = global.wayf_show_remember_checkbox;
var wayf_force_remember_for_session = global.wayf_force_remember_for_session;
var wayf_additional_idps = global.wayf_additional_idps;
var wayf_sp_samlDSURL = global.wayf_sp_samlDSURL;
var wayf_sp_samlACURL = global.wayf_sp_samlACURL;
var wayf_use_disco_feed = global.wayf_use_disco_feed;
var wayf_discofeed_url = global.wayf_discofeed_url;

// Internal variables
var wayf_improved_dropdown_url = 'https://haka.funet.fi/shibboleth/js/improvedDropDown.js';
var wayf_jquery_url = 'https://haka.funet.fi/shibboleth/js/jquery.js';
var wayf_dropdown_icon_url = 'https://haka.funet.fi/shibboleth/images/drop_icon.png';
var wayf_disco_feed_idps;
var wayf_html = "";
var wayf_categories = {
"Haka":{
	type:"category",
	name:"Haka"
}
};
var wayf_idps = {
"https://idp.aalto.fi/idp/shibboleth":{
	type:"Haka",
	name:"Aalto University",
	logoURL:"",
	data:"aalto.fi Aalto University Aalto-universitetet Aalto-yliopisto"
},
"https://tullbommen.arcada.fi/simplesaml/":{
	type:"Haka",
	name:"Arcada",
	logoURL:"",
	data:"arcada.fi Arcada Arcada Arcada"
},
"https://idp.cou.fi/idp/shibboleth":{
	type:"Haka",
	name:"Centria University of Applied Sciences",
	logoURL:"",
	data:"cou.fi Centria University of Applied Sciences Centria yrkeshögskola Centria ammattikorkeakoulu"
},
"https://salpa.certia.fi/idp/shibboleth":{
	type:"Haka",
	name:"Certia Oy",
	logoURL:"",
	data:"certia.fi Certia Oy Certia Oy Certia Oy"
},
"https://idp.csc.fi/idp/shibboleth":{
	type:"Haka",
	name:"CSC - IT Center for Science Ltd.",
	logoURL:"",
	data:"csc.fi CSC - IT Center for Science Ltd. CSC - Tieteen tietotekniikan keskus Oy CSC - Tieteen tietotekniikan keskus Oy"
},
"https://idp.diak.fi/idp/shibboleth":{
	type:"Haka",
	name:"Diaconia University of Applied Sciences",
	logoURL:"",
	data:"diak.fi Diaconia University of Applied Sciences Diakonia-ammattikorkeakoulu Diakonia-ammattikorkeakoulu"
},
"http://fs.ymparisto.fi/adfs/services/trust":{
	type:"Haka",
	name:"Finnish Environment Institute",
	logoURL:"",
	data:"ymparisto.fi Finnish Environment Institute Finlands miljöcentral Suomen ympäristökeskus"
},
"https://sipuli.fmi.fi/idp/shibboleth":{
	type:"Haka",
	name:"Finnish Meteorological Institute",
	logoURL:"",
	data:"fmi.fi Finnish Meteorological Institute Meteorologiska institutet Ilmatieteen laitos"
},
"https://idp.oph.fi/idp/shibboleth":{
	type:"Haka",
	name:"Finnish National Agency for Education",
	logoURL:"",
	data:"oph.fi Finnish National Agency for Education Utbildningsstyrelsen Opetushallitus"
},
"https://idp.funidata.fi/idp/shibboleth":{
	type:"Haka",
	name:"Funidata IDP",
	logoURL:"",
	data:"funidata.fi Funidata IDP Funidata IDP Funidata IDP"
},
"http://fs.valtori.fi/adfs/services/trust":{
	type:"Haka",
	name:"Government ICT Centre Valtori",
	logoURL:"",
	data:"valtori.fi Government ICT Centre Valtori Statens center för informations- och kommunikationsteknik Valtion tieto- ja viestintätekniikkakeskus Valtori"
},
"https://idp.haaga-helia.fi/idp/shibboleth":{
	type:"Haka",
	name:"HAAGA-HELIA University of Applied Sciences",
	logoURL:"",
	data:"haaga-helia.fi HAAGA-HELIA University of Applied Sciences HAAGA-HELIA  yrkeshögskola HAAGA-HELIA  ammattikorkeakoulu"
},
"https://idp.shh.fi/idp/shibboleth":{
	type:"Haka",
	name:"Hanken School of Economics",
	logoURL:"",
	data:"shh.fi Hanken School of Economics Hanken Svenska handelshögskolan Hanken Svenska handelshögskolan"
},
"https://rap.humak.fi/idp/shibboleth":{
	type:"Haka",
	name:"HUMAK University of Applied Sciences",
	logoURL:"",
	data:"humak.fi HUMAK University of Applied Sciences Humanistiska Yrkeshögskola Humanistinen ammattikorkeakoulu"
},
"https://shibbo.hamk.fi/shibboleth":{
	type:"Haka",
	name:"Häme University of Applied Sciences",
	logoURL:"",
	data:"hamk.fi Häme University of Applied Sciences HAMK Yrkeshögskola Hämeen ammattikorkeakoulu"
},
"https://kotusidp.vyv.fi/IDP/app/Haka":{
	type:"Haka",
	name:"Institute for the Languages of Finland",
	logoURL:"",
	data:"vyv.fi Institute for the Languages of Finland Institutet för de inhemska språken Kotimaisten kielten keskus"
},
"https://idp2.jamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Jyväskylä University of Applied Sciences",
	logoURL:"",
	data:"jamk.fi Jyväskylä University of Applied Sciences JAMK Jyväskylän ammattikorkeakoulu"
},
"https://kamidp01.kamit.fi/idp/shibboleth":{
	type:"Haka",
	name:"Kajaani University of Applied Sciences",
	logoURL:"",
	data:"kamit.fi Kajaani University of Applied Sciences Kajaanin ammattikorkeakoulu Kajaanin ammattikorkeakoulu"
},
"https://idp.pkamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Karelia University of Applied Sciences",
	logoURL:"",
	data:"pkamk.fi Karelia University of Applied Sciences Karelia yrkehögskola Karelia-ammattikorkeakoulu"
},
"https://sts.psshp.fi/idp/haka":{
	type:"Haka",
	name:"Kuopio University Hospital",
	logoURL:"",
	data:"psshp.fi Kuopio University Hospital Kuopio universitetssjukhus Kuopion yliopistollinen sairaala"
},
"https://haka.lpt.fi/idp/shibboleth":{
	type:"Haka",
	name:"Lahti University of Applied Sciences",
	logoURL:"",
	data:"lpt.fi Lahti University of Applied Sciences Lahtis yrkeshögskola Lahden ammattikorkeakoulu"
},
"https://idp.lapinamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Lapland University of Applied Sciences",
	logoURL:"",
	data:"lapinamk.fi Lapland University of Applied Sciences Lapplands Yrkeshögskola Lapin ammattikorkeakoulu"
},
"https://idp.lut.fi":{
	type:"Haka",
	name:"Lappeenranta University of Technology",
	logoURL:"",
	data:"lut.fi Lappeenranta University of Technology Lappeenranta Tekniska Högskolan Lappeenrannan teknillinen yliopisto"
},
"http://tunnistus.laurea.fi/adfs/services/trust":{
	type:"Haka",
	name:"Laurea University of Applied Sciences",
	logoURL:"",
	data:"laurea.fi Laurea University of Applied Sciences Laurea yrkehögskola Laurea-ammattikorkeakoulu"
},
"https://idp.metropolia.fi/idp":{
	type:"Haka",
	name:"Metropolia University of Applied Sciences",
	logoURL:"",
	data:"metropolia.fi Metropolia University of Applied Sciences Metropolia yrkeshögskola Metropolia Ammattikorkeakoulu"
},
"http://fs.narc.fi/adfs/services/trust":{
	type:"Haka",
	name:"National Archives Service",
	logoURL:"",
	data:"narc.fi National Archives Service Riksarkivet Kansallisarkisto"
},
"http://fs.kavi.fi/adfs/services/trust":{
	type:"Haka",
	name:"National Audiovisual institute",
	logoURL:"",
	data:"kavi.fi National Audiovisual institute Nationella audiovisuella institutet Kansallinen audiovisuaalinen instituutti"
},
"https://tunnistus.mpkkfu.fi/idp/shibboleth":{
	type:"Haka",
	name:"National Defence University",
	logoURL:"",
	data:"mpkkfu.fi National Defence University Förvarshögskolan Maanpuolustuskorkeakoulu"
},
"https://tunnistus.thl.fi/idp":{
	type:"Haka",
	name:"National Institute for Health and Welfare",
	logoURL:"",
	data:"thl.fi National Institute for Health and Welfare Institutet för hälsa och välfärd Terveyden ja hyvinvoinnin laitos"
},
"https://idp1.novia.fi/idp/shibboleth":{
	type:"Haka",
	name:"Novia University of Applied Sciences",
	logoURL:"",
	data:"novia.fi Novia University of Applied Sciences Yrkeshögskolan Novia Yrkeshögskolan Novia"
},
"https://idp.oamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Oulu University of Applied Sciences",
	logoURL:"",
	data:"oamk.fi Oulu University of Applied Sciences Yrkeshögskolan i Uleåborg Oulun ammattikorkeakoulu"
},
"https://tunnistus.smedu.fi/idp/shibboleth":{
	type:"Haka",
	name:"Police University College",
	logoURL:"",
	data:"smedu.fi Police University College Polisyrkeshögskolan Poliisiammattikorkeakoulu"
},
"https://haka.saimia.fi/idp/shibboleth":{
	type:"Haka",
	name:"Saimaa University of Applied Sciences",
	logoURL:"https://idp.saimia.fi/idp/images/saimaa_uas_logo_16x16px.png",
	data:"saimia.fi Saimaa University of Applied Sciences Yrkehögskola Saimaa Saimaan ammattikorkeakoulu"
},
"https://idp.samk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Satakunta University of Applied Sciences",
	logoURL:"",
	data:"samk.fi Satakunta University of Applied Sciences samk Satakunnan ammattikorkeakoulu samk Satakunnan ammattikorkeakoulu samk"
},
"https://idp.savonia.fi/idp/shibboleth":{
	type:"Haka",
	name:"Savonia University of Applied Sciences",
	logoURL:"",
	data:"savonia.fi Savonia University of Applied Sciences Savonia Yrkeshögskola Savonia-ammattikorkeakoulu"
},
"https://idp2.epedu.fi/idp/shibboleth":{
	type:"Haka",
	name:"Seinäjoki University of Applied Sciences",
	logoURL:"",
	data:"epedu.fi Seinäjoki University of Applied Sciences Seinäjoki yrkeshögskola Seinäjoen ammattikorkeakoulu"
},
"https://xidp.xamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"South-Eastern Finland University of Applied Sciences",
	logoURL:"",
	data:"xamk.fi South-Eastern Finland University of Applied Sciences xamk xamk.fi south-eastern finland Kaakkois-Suomen ammattikorkeakoulu xamk xamk.fi Kaakkois-Suomen ammattikorkeakoulu xamk xamk.fi kaakkois-suomi"
},
"https://idp.tuni.fi/idp/shibboleth":{
	type:"Haka",
	name:"Tampere Universities",
	logoURL:"",
	data:"tuni.fi Tampere Universities Tampere tuni.fi TUNI Tampere3 Tampere Universities Tampereen korkeakoulut Tampere tuni.fi TUNI Tampere3"
},
"https://idp2.tamk.fi":{
	type:"Haka",
	name:"Tampere University of Applied Sciences",
	logoURL:"",
	data:"tamk.fi Tampere University of Applied Sciences Tampereen ammattikorkeakoulu Tampereen ammattikorkeakoulu"
},
"https://idp.tut.fi/shibboleth2":{
	type:"Haka",
	name:"Tampere University of Technology",
	logoURL:"https://www.tut.fi/TTY-ratas/TTY_ratas-16x16.png",
	data:"tut.fi Tampere University of Technology TUT Tammerfors tekniska universitet Tampereen teknillinen yliopisto TTY TTKK"
},
"https://tunnistus.pelastusopisto.fi/idp/shibboleth":{
	type:"Haka",
	name:"The Emergency Services College",
	logoURL:"",
	data:"pelastusopisto.fi The Emergency Services College Räddningsinstitutet Pelastusopisto"
},
"https://idp1.turkuamk.fi/idp/shibboleth":{
	type:"Haka",
	name:"Turku University of Applied Sciences",
	logoURL:"",
	data:"turkuamk.fi Turku University of Applied Sciences Åbo yrkeshögskola Turun ammattikorkeakoulu"
},
"https://idp.uef.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Eastern Finland",
	logoURL:"",
	data:"uef.fi University of Eastern Finland Östra Finlands universitet Itä-Suomen yliopisto"
},
"https://login.helsinki.fi/shibboleth":{
	type:"Haka",
	name:"University of Helsinki",
	logoURL:"",
	data:"helsinki.fi University of Helsinki Helsingfors universitet Helsingin yliopisto"
},
"https://login.jyu.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Jyväskylä",
	logoURL:"",
	data:"jyu.fi University of Jyväskylä Jyväskylä universitet Jyväskylän yliopisto"
},
"https://idp.ulapland.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Lapland",
	logoURL:"",
	data:"ulapland.fi University of Lapland Lapplands universitet Lapin yliopisto"
},
"https://login.oulu.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Oulu",
	logoURL:"",
	data:"oulu.fi University of Oulu Uleåborgs universitet Oulun yliopisto"
},
"https://shibboleth.uta.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Tampere",
	logoURL:"",
	data:"uta.fi University of Tampere uta.fi uta tay Tammerfors universitet uta.fi uta tay Tampereen yliopisto uta.fi uta tay"
},
"https://idp.uniarts.fi/shibboleth":{
	type:"Haka",
	name:"University of the Arts Helsinki",
	logoURL:"",
	data:"uniarts.fi University of the Arts Helsinki Konstuniversitetet Taideyliopisto"
},
"https://sso.utu.fi/sso-idp":{
	type:"Haka",
	name:"University of Turku",
	logoURL:"",
	data:"utu.fi University of Turku utu Åbo universitet utu Turun yliopisto utu"
},
"https://login.uwasa.fi/idp/shibboleth":{
	type:"Haka",
	name:"University of Vaasa",
	logoURL:"",
	data:"uwasa.fi University of Vaasa Vasa Universitet Vaasan yliopisto"
},
"https://idp.vamk.fi/saml2/idp/metadata.php":{
	type:"Haka",
	name:"Vaasa University of Applied Sciences",
	logoURL:"",
	data:"vamk.fi Vaasa University of Applied Sciences Vasa yrkeshögskola Vaasan ammattikorkeakoulu"
},
"https://idp.abo.fi/idp/shibboleth":{
	type:"Haka",
	name:"Åbo Akademi University",
	logoURL:"",
	data:"abo.fi Åbo Akademi University ÅA Åbo Akademi ÅA Åbo Akademi"
} };
var wayf_other_fed_idps = {};

// Functions
function redirectTo(url){
	// Make sure the redirect always is being done in parent window
	if (window.parent){
		window.parent.location = url;
	} else {
		window.location = url;
	}
}

function submitForm(eventObj){

	if (document.IdPList.user_idp && document.IdPList.user_idp.selectedIndex == 0){
		alert('You must select a valid Home Organisation.');
		return false;
	}
	// Set local cookie
	var selectedIdP = document.IdPList.user_idp[document.IdPList.user_idp.selectedIndex].value;
	setDomainSAMLDomainCookie(selectedIdP);

	// User chose federation IdP entry
	if( wayf_idps[selectedIdP]) {
		return true;
	}

	// User chose IdP from other federation
	var redirect_url;

	// Redirect user to SP handler
	if (wayf_use_discovery_service){

		var entityIDGETParam = getGETArgument("entityID");
		var returnGETParam = getGETArgument("return");
		if (entityIDGETParam != "" && returnGETParam != ""){
			redirect_url = returnGETParam;
		} else {
			redirect_url = wayf_sp_samlDSURL;
			redirect_url += getGETArgumentSeparator(redirect_url) + 'target=' + encodeURIComponent(wayf_return_url);
		}

		// Prevent default submit action
		eventObj.preventDefault();

		// Append selected Identity Provider
		redirect_url += '&entityID=' + encodeURIComponent(selectedIdP);

		redirectTo(redirect_url);
	} else {
		redirect_url = wayf_sp_handlerURL + '?providerId='
		+ encodeURIComponent(selectedIdP)
		+ '&target=' + encodeURIComponent(wayf_return_url);

				// Prevent default submit action
		eventObj.preventDefault();

		redirectTo(redirect_url);
	}

	// If input type button is used for submit, we must return false
	return false;
}

function writeOptGroup(IdPElements, category){

	if (!wayf_categories[category]){
		writeHTML(IdPElements);
		return;
	}

	if (IdPElements === ''){
		return;
	}

	var categoryName = wayf_categories[category].name;

	if (wayf_show_categories){
		writeHTML('<optgroup label="' + categoryName + '">');
	}

	writeHTML(IdPElements);

	if (wayf_show_categories){
		writeHTML('</optgroup>');
	}
}

function writeHTML(a){
	wayf_html += a;
}

function isEmptyObject(obj){

	if (typeof(obj) != "object"){
		return true;
	}

	for (var index in obj){
		return false;
	}

	return true;
}

function isAllowedIdP(IdP){

	var type = '';
	if (wayf_idps[IdP]){
		type = wayf_idps[IdP].type;
	} else if (wayf_other_fed_idps[IdP]){
		type = wayf_other_fed_idps[IdP].type;
	}

	// Check if IdP should be hidden
	for ( var i = 0; i < wayf_hide_idps.length; i++){
		if (wayf_hide_idps[i] == IdP){
			return false;
		}
	}

	// Check if category should be hidden
	// Check if IdP is unhidden in this category
	for ( var i = 0; i < wayf_hide_categories.length; i++){

		if (wayf_hide_categories[i] === "all" || wayf_hide_categories[i] == type){

			for ( var i=0; i < wayf_unhide_idps.length; i++){
				// Show IdP if it has to be unhidden
				if (wayf_unhide_idps[i] == IdP){
					return true;
				}
			}

			// If IdP is not unhidden, the default applies
			return false;
		}
	}

	// Default
	return true;
}

function setDomainSAMLDomainCookie(entityID){
	// Create and store SAML domain cookie on host where WAYF is embedded
	var currentDomainCookie = getCookie('_saml_idp');
	var encodedEntityID = encodeBase64(entityID);
	if (currentDomainCookie == null){
		currentDomainCookie = '';
	}

	// Ensure current IdP is not already in array
	var currentIdPs = currentDomainCookie.split(' ');
	var newIdPs = new Array();
	for (var i = 0; i < currentIdPs.length; i++) {
		if (currentIdPs[i] != encodedEntityID && currentIdPs[i] != ''){
			newIdPs.push(currentIdPs[i]);
		}
	}

	// Add new IdP
	newIdPs.push(encodedEntityID);

	// Ensure array is no longer than 5 IdPs
	while (newIdPs.length > 5){
		newIdPs.shift();
	}

	// Compose cookie value
	var newDomainCookie = '';
	for (var i = 0; i < newIdPs.length; i++) {
		newDomainCookie += newIdPs[i] + ' ';
	}

	// Set cookie value
	setCookie('_saml_idp', newDomainCookie.trim() , 100);
}

function setCookie(c_name, value, expiredays){
	var exdate = new Date();
	exdate.setDate(exdate.getDate() + expiredays);
	document.cookie=c_name + "=" + escape(value) +
	((expiredays==null) ? "" : "; expires=" + exdate.toGMTString());
}

function getCookie(check_name){
	// First we split the cookie up into name/value pairs
	// Note: document.cookie only returns name=value, not the other components
	var a_all_cookies = document.cookie.split( ';' );
	var a_temp_cookie = '';
	var cookie_name = '';
	var cookie_value = '';

	for ( var i = 0; i < a_all_cookies.length; i++ ){
		// now we'll split apart each name=value pair
		a_temp_cookie = a_all_cookies[i].split('=');


		// and trim left/right whitespace while we're at it
		cookie_name = a_temp_cookie[0].replace(/^\s+|\s+$/g, '');

		// if the extracted name matches passed check_name
		if ( cookie_name == check_name )
		{
			// We need to handle case where cookie has no value but exists (no = sign, that is):
			if ( a_temp_cookie.length > 1 )
			{
				cookie_value = unescape( a_temp_cookie[1].replace(/^\s+|\s+$/g, '') );
			}
			// note that in cases where cookie is initialized but no value, null is returned
			return cookie_value;
			break;
		}
		a_temp_cookie = null;
		cookie_name = '';
	}

	return null;
}

// Query Shibboleth Session handler and process response afterwards
// This method has to be used because HttpOnly prevents reading
// the shib session cookies via JavaScript
function isShibbolethSession(url){

	var result = queryGetURL(url);

	// Return true if session handler shows valid session
	if (result && result.search(/Authentication Time/i) > 0){
		return true;
	}

	return false;
}

// Loads Identity Provider from DiscoFeed and adds them to additional IdPs
function loadDiscoFeedIdPs(){

	var result = queryGetURL(wayf_discofeed_url);
	var IdPs = {};

	// Load JSON
	if (result != ''){
		IdPs = eval("(" +result + ")");
	}

	return IdPs;
}

// Makes a synchronous AJAX request with the given URL and returns
// returned string or '' in case of a problem
function queryGetURL(url){
	var xmlhttp;

	if (window.XMLHttpRequest){
		xmlhttp = new XMLHttpRequest();
	}  else {
		xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	}

	// Send synchronous request
	try {
		xmlhttp.open("GET", url, false);
		xmlhttp.send();
	} catch (e) {
		// Something went wrong, send back false
		return false;
	}

	// Check response code
	if (xmlhttp.readyState != 4 || xmlhttp.status != 200 ){
		return '';
	}

	return xmlhttp.responseText;
}


// Adds unknown IdPs to wayf_additional_idps and hides IdPs that are not
// contained in the Discovery Feed
function processDiscoFeedIdPs(IdPs){

	if (typeof(IdPs) === "undefined"){
		return;
	}

	// Hide IdPs that are not in the Discovery Feed
	for (var entityID in wayf_idps){
		var foundIdP = false;
		for ( var i = 0; i < IdPs.length; i++) {
			if (IdPs[i].entityID == entityID){
				foundIdP = true;
			}
		}

		if (foundIdP == false){
			wayf_hide_idps.push(entityID);
		}
	}

	// Add unkown IdPs to wayf_additional_idps
	for ( var i = 0; i < IdPs.length; i++) {

		// Skip IdPs that are already known
		if (wayf_idps[IdPs[i].entityID]){
			continue;
		}

		var newIdP = getIdPFromDiscoFeedEntry(IdPs[i]);

		wayf_additional_idps.push(newIdP);
	}
}


function getIdPFromDiscoFeedEntry(IdPData){
	var name = IdPData.entityID;
	var name_default = '';
	var name_requested = '';
	var data = '';
	var logo = '';

	if (IdPData.DisplayNames){
		for (var i = 0; i < IdPData.DisplayNames.length; i++){

			name = IdPData.DisplayNames[i].value;

			if (IdPData.DisplayNames[i].lang == 'en'){
				name_requested = name;
			} else if (IdPData.DisplayNames[i].lang == 'en'){
				name_default = name;
			}

			data += ' ' + IdPData.DisplayNames[i].value;
		}

		if (name_requested != ''){
			name = name_requested;
		} else if (name_default != ''){
			name = name_default;
		}
	}

	if (IdPData.Keywords){
		for (var i = 0; i < IdPData.Keywords.length; i++){
			data += ' ' + IdPData.Keywords[i].value;
		}
	}

	if (IdPData.Logos){
		for (var i = 0; i < IdPData.Logos.length; i++){
			if (IdPData.Logos[i].height == 16 && IdPData.Logos[i].width == 16){
				logo = IdPData.Logos[i].value;
			}
		}
	}

	var newIdP = {
		"entityID":IdPData.entityID,
		"name": name,
		"type": "unknown",
		"SAML1SSOurl":"https://this.url.does.not.exist/test",
		"data": data,
		"logoURL":logo
	};

	return newIdP;
}


// Sorts Discovery feed entries
function sortEntities(a, b){
	var nameA = a.name.toLowerCase();
	var nameB = b.name.toLowerCase();

	if (nameA < nameB){
		return -1;
	}

	if (nameA > nameB){
		return 1;
	}

	return 0;
}

// Returns true if user is logged in
function isUserLoggedIn(){

	if (
		   typeof(wayf_check_login_state_function) !== "undefined"
		&& typeof(wayf_check_login_state_function) === "function" ){

		// Use custom function
		return wayf_check_login_state_function();

	} else {
		// Check Shibboleth session handler
		return isShibbolethSession(wayf_sp_handlerURL + '/Session');
	}
}

function encodeBase64(input) {
	var base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
	var output = "", c1, c2, c3, e1, e2, e3, e4;

	for ( var i = 0; i < input.length; ) {
		c1 = input.charCodeAt(i++);
		c2 = input.charCodeAt(i++);
		c3 = input.charCodeAt(i++);
		e1 = c1 >> 2;
		e2 = ((c1 & 3) << 4) + (c2 >> 4);
		e3 = ((c2 & 15) << 2) + (c3 >> 6);
		e4 = c3 & 63;
		if (isNaN(c2)){
			e3 = e4 = 64;
		} else if (isNaN(c3)){
			e4 = 64;
		}
		output += base64chars.charAt(e1) + base64chars.charAt(e2) + base64chars.charAt(e3) + base64chars.charAt(e4);
	}

	return output;
}

function decodeBase64(input) {
	var base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
	var output = "", chr1, chr2, chr3, enc1, enc2, enc3, enc4;
	var i = 0;

	// Remove all characters that are not A-Z, a-z, 0-9, +, /, or =
	var base64test = /[^A-Za-z0-9\+\/\=]/g;
	input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

	do {
		enc1 = base64chars.indexOf(input.charAt(i++));
		enc2 = base64chars.indexOf(input.charAt(i++));
		enc3 = base64chars.indexOf(input.charAt(i++));
		enc4 = base64chars.indexOf(input.charAt(i++));

		chr1 = (enc1 << 2) | (enc2 >> 4);
		chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
		chr3 = ((enc3 & 3) << 6) | enc4;

		output = output + String.fromCharCode(chr1);

		if (enc3 != 64) {
			output = output + String.fromCharCode(chr2);
		}
		if (enc4 != 64) {
			output = output + String.fromCharCode(chr3);
		}

		chr1 = chr2 = chr3 = "";
		enc1 = enc2 = enc3 = enc4 = "";

	} while (i < input.length);

	return output;
}

function getGETArgument(name){
	name = name.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
	var regexString = "[\\?&]"+name+"=([^&#]*)";
	var regex = new RegExp(regexString);
	var results = regex.exec(window.location.href);

	if( results == null ){
		return "";
	} else {
		return decodeURIComponent(results[1]);
	}
}

function getGETArgumentSeparator(url){
	if (url.indexOf('?') >=0 ){
		return '&';
	} else {
		return '?';
	}
}

function ieLoadBugFix(scriptElement, callback){
	if (scriptElement.readyState && (scriptElement.readyState=='loaded' || scriptElement.readyState=='completed')){
		callback();
	 } else {
		setTimeout(function() {
			ieLoadBugFix(scriptElement, callback);
		}, 100);
	 }
}

function getOptionHTML(entityID){

	var IdPData;
	if (wayf_idps[entityID]){
		IdPData = wayf_idps[entityID];
	} else if (wayf_other_fed_idps[entityID]){
		IdPData = wayf_other_fed_idps[entityID];
	} else {
		return '';
	}

	var content = '';
	var data = '';
	var logo = '';
	var selected = '';

	if (IdPData.data){
		data = ' data="' + IdPData.data + '"';
	}

	if (IdPData.logoURL){
		logo = ' logo="' + IdPData.logoURL + '"';
	}

	if (IdPData.selected){
		selected = ' selected="selected"';
	}

	content = '<option value="' + entityID + '"' + data + logo + selected + '>' + IdPData.name + '</option>';

	return content;
}

function loadJQuery() {
	var head = document.getElementsByTagName('head')[0];
	var script = document.createElement('script');
	var improvedDropDownLoaded = false;

	script.src = 'https://haka.funet.fi/shibboleth/js/jquery.js';
	script.type = 'text/javascript';
	script.onload = function() {
		// Extra security check, because jquery cannot set our local $
		// Sometimes $ does not even exist.
		if (typeof jQuery === "function") {
			$ = jQuery;
		}
		loadImprovedDropDown();
		improvedDropDownLoaded = true;
	};
	head.appendChild(script);

	// Fix for IE Browsers
	ieLoadBugFix(script, function(){
		if (!improvedDropDownLoaded){
			loadImprovedDropDown();
		}
	});
}

function loadImprovedDropDown(){


	// Load CSS
	$('head').append('<link rel="stylesheet" type="text/css" href="https://haka.funet.fi/shibboleth/css/default-ImprovedDropDown.css">');

	// Load Improved Drop Down Javascript
	$.getScript( 'https://haka.funet.fi/shibboleth/js/improvedDropDown.js', function() {

		var searchText = 'Enter the name of the organisation you are affiliated with...';
		$("#user_idp:enabled option[value='-']").text(searchText);

		// Ensure that the extension is loaded in a Asynchronous Module Definition (AMD) environment.
		// Due to async processing the AMD environment can appear during runtime.
		typeof define === "function" && define.amd ? require(["improvedDropDown"], runImproveDropDown) : runImproveDropDown();

	});
}

function runImproveDropDown() {

	// Convert select element into improved drop down list
	$("#user_idp:enabled").improveDropDown({
			iconPath:'https://haka.funet.fi/shibboleth/images/drop_icon.png',
			noMatchesText: 'No organisation found for this search text',
			noItemsText: 'No organisation available',
			disableRemoteLogos: wayf_disable_remote_idp_logos,
			enableValueMatching: wayf_enable_entityid_matching
	});
}

(function() {

	var config_ok = true;

	// Get GET parameters that maybe are set by Shibboleth
	var returnGETParam = getGETArgument("return");
	var entityIDGETParam = getGETArgument("entityID");

	// First lets make sure properties are available
	if(
		typeof wayf_use_discovery_service === "undefined"
		|| typeof wayf_use_discovery_service !== "boolean"
	){
		wayf_use_discovery_service = true;
	}

	if(
		typeof wayf_use_improved_drop_down_list === "undefined"
		|| typeof wayf_use_improved_drop_down_list !== "boolean"
	){
		wayf_use_improved_drop_down_list = false;
	}

	if(
		typeof wayf_disable_remote_idp_logos === "undefined"
		|| typeof wayf_disable_remote_idp_logos !== "boolean"
	){
		wayf_disable_remote_idp_logos = false;
	}

	if(
		typeof wayf_enable_entityid_matching === "undefined"
		|| typeof wayf_enable_entityid_matching !== "boolean"
	){
		wayf_enable_entityid_matching = true;
	}

	// Overwrite entityID with GET argument if present
	var entityIDGETParam = getGETArgument("entityID");
	if (entityIDGETParam !== ""){
		wayf_sp_entityID = entityIDGETParam;
	}

	if(
		typeof wayf_sp_entityID === "undefined"
		|| typeof wayf_sp_entityID !== "string"
		){
		alert('The mandatory parameter \'wayf_sp_entityID\' is missing. Please add it as a javascript variable on this page.');
		config_ok = false;
	}

	if(
		typeof wayf_URL === "undefined"
		|| typeof wayf_URL !== "string"
		){
		alert('The mandatory parameter \'wayf_URL\' is missing. Please add it as a javascript variable on this page.');
		config_ok = false;
	}

	if(
		typeof wayf_return_url === "undefined"
		|| typeof wayf_return_url !== "string"
		){
		alert('The mandatory parameter \'wayf_return_url\' is missing. Please add it as a javascript variable on this page.');
		config_ok = false;
	}

	if(
		wayf_use_discovery_service === false
		&& typeof wayf_sp_handlerURL === "undefined"
		){
		alert('The mandatory parameter \'wayf_sp_handlerURL\' is missing. Please add it as a javascript variable on this page.');
		config_ok = false;
	}

	if(
		wayf_use_discovery_service === true
		&& typeof wayf_sp_samlDSURL === "undefined"
		){
		// Set to default DS handler
		wayf_sp_samlDSURL = wayf_sp_handlerURL + "/Login";
	}

	if (
		typeof wayf_sp_samlACURL === "undefined"
		|| typeof wayf_sp_samlACURL !== "string"
		){
		wayf_sp_samlACURL = wayf_sp_handlerURL + '/SAML/POST';
	}

	if(
		typeof wayf_font_color === "undefined"
		|| typeof wayf_font_color !== "string"
		){
		wayf_font_color = 'black';
	}

	if(
		typeof wayf_font_size === "undefined"
		|| typeof wayf_font_size !== "number"
		){
		wayf_font_size = 12;
	}

	if(
		typeof wayf_border_color === "undefined"
		|| typeof wayf_border_color !== "string"
		){
		wayf_border_color = '#848484';
	}

	if(
		typeof wayf_background_color === "undefined"
		|| typeof wayf_background_color !== "string"
		){
		wayf_background_color = '#F0F0F0';
	}

	if(
		typeof wayf_use_small_logo === "undefined"
		|| typeof wayf_use_small_logo !== "boolean"
		){
		wayf_use_small_logo = true;
	}

	if(
		typeof wayf_hide_logo === "undefined"
		|| typeof wayf_use_small_logo !== "boolean"
		){
		wayf_hide_logo = false;
	}

	if(
		typeof wayf_width === "undefined"
		|| typeof wayf_width !== "number"
	){
		wayf_width = "auto";
	} else {
		wayf_width += 'px';
	}

	if(
		typeof wayf_height === "undefined"
		|| typeof wayf_height !== "number"
		){
		wayf_height = "auto";
	} else {
		wayf_height += "px";
	}

	if(
		typeof wayf_show_remember_checkbox === "undefined"
		|| typeof wayf_show_remember_checkbox !== "boolean"
		){
		wayf_show_remember_checkbox = true;
	}

	if(
		typeof wayf_force_remember_for_session === "undefined"
		|| typeof wayf_force_remember_for_session !== "boolean"
		){
		wayf_force_remember_for_session = false;
	}

	if(
		typeof wayf_auto_login === "undefined"
		|| typeof wayf_auto_login !== "boolean"
		){
		wayf_auto_login = true;
	}

	if(
		typeof wayf_hide_after_login === "undefined"
		|| typeof wayf_hide_after_login !== "boolean"
		){
		wayf_hide_after_login = true;
	}

	if(
		typeof wayf_logged_in_messsage === "undefined"
		|| typeof wayf_logged_in_messsage !== "string"
		){
		wayf_logged_in_messsage = "You are already authenticated.".replace(/%s/, wayf_return_url);
	}

	if(
		typeof wayf_auto_redirect_if_logged_in === "undefined"
		|| typeof wayf_auto_redirect_if_logged_in !== "boolean"
		){
		wayf_auto_redirect_if_logged_in = false;
	}

	if(
		typeof wayf_default_idp === "undefined"
		|| typeof wayf_default_idp !== "string"
		){
		wayf_default_idp = '';
	}

	if(
		typeof wayf_num_last_used_idps === "undefined"
		|| typeof wayf_num_last_used_idps !== "number"
		){
		wayf_num_last_used_idps = 3;
	}

	if(
		typeof wayf_most_used_idps === "undefined"
		|| typeof wayf_most_used_idps !== "object"
		){
		wayf_most_used_idps = new Array();
	}

	if(
		typeof wayf_overwrite_last_used_idps_text === "undefined"
		|| typeof wayf_overwrite_last_used_idps_text !== "string"
		){
		wayf_overwrite_last_used_idps_text = "Last used";
	}

	if(
		typeof wayf_overwrite_most_used_idps_text === "undefined"
		|| typeof wayf_overwrite_most_used_idps_text !== "string"
		){
		wayf_overwrite_most_used_idps_text = "Most often used Home Organisations";
	}

	if(
		typeof wayf_overwrite_checkbox_label_text === "undefined"
		|| typeof wayf_overwrite_checkbox_label_text !== "string"
		){
		wayf_overwrite_checkbox_label_text = "Remember selection for this web browser session.";
	}

	if(
		typeof wayf_overwrite_submit_button_text === "undefined"
		|| typeof wayf_overwrite_submit_button_text !== "string"
		){
		wayf_overwrite_submit_button_text = "Login";
	}

	if(
		typeof wayf_overwrite_intro_text === "undefined"
		|| typeof wayf_overwrite_intro_text !== "string"
		){
		wayf_overwrite_intro_text = "Login with:";
	}

	if(
		typeof wayf_overwrite_from_other_federations_text === "undefined"
		|| typeof wayf_overwrite_from_other_federations_text !== "string"
		){
		wayf_overwrite_from_other_federations_text = "From other federations";
	}

	if(
		typeof wayf_show_categories === "undefined"
		|| typeof wayf_show_categories !== "boolean"
		){
		wayf_show_categories = true;
	}

	if(
		typeof wayf_hide_categories === "undefined"
		|| typeof wayf_hide_categories !== "object"
		){
		wayf_hide_categories = new Array();
	}

	if(
		typeof wayf_unhide_idps === "undefined"
		||  typeof wayf_unhide_idps !== "object"
	){
		wayf_unhide_idps = new Array();
	}

	if(
		typeof wayf_hide_idps === "undefined"
		|| typeof wayf_hide_idps !== "object"
		){
		wayf_hide_idps = new Array();
	}

	if(
		typeof wayf_additional_idps === "undefined"
		|| typeof wayf_additional_idps !== "object"
		){
		wayf_additional_idps = [];
	}

	if(
		typeof wayf_use_disco_feed === "undefined"
		|| typeof wayf_use_disco_feed !== "boolean"
		){
		wayf_use_disco_feed = false;
	}

	if(
		typeof wayf_discofeed_url === "undefined"
		|| typeof wayf_discofeed_url !== "string"
		){
		wayf_discofeed_url = "/Shibboleth.sso/DiscoFeed";
	}

	// Exit without outputting html if config is not ok
	if (config_ok != true){
		return;
	}

	// Check if user is logged in already:
	var user_logged_in = isUserLoggedIn();

	// Check if user is authenticated already and should
	// be redirected to wayf_return_url
	if (
		user_logged_in
		&& wayf_auto_redirect_if_logged_in
	){
		redirectTo(wayf_return_url);
		return;
	}

	// Check if user is authenticated already and
	// whether something has to be drawn
	if (
		wayf_hide_after_login
		&& user_logged_in
		&& wayf_logged_in_messsage === ''
	){

		// Exit script without drawing
		return;
	}

	// Now start generating the HTML for outer box
	if(
		wayf_hide_after_login
		&& user_logged_in
	){
		writeHTML('<div id="wayf_div" style="background:' + wayf_background_color + ';border-style: solid;border-color: ' + wayf_border_color + ';border-width: 1px;padding: 10px; height: auto;width: ' + wayf_width + ';text-align: left;overflow: hidden;">');
	} else {
		writeHTML('<div id="wayf_div" style="background:' + wayf_background_color + ';border-style: solid;border-color: ' + wayf_border_color + ';border-width: 1px;padding: 10px; height: ' + wayf_height + ';width: ' + wayf_width + ';text-align: left;overflow: hidden;">');
	}

	// Do we have to display the logo
	if (wayf_hide_logo != true){

		// Write header of logo div
		writeHTML('<div id="wayf_logo_div" style="float: right;"><a href="https://wiki.eduuni.fi/display/CSCHAKA" target="_blank" style="border:0px; margin-bottom: 4px;">');

		// Which size of the logo should we display
		var embeddedLogoURL = '';
		if (wayf_use_small_logo){
			embeddedLogoURL = "https://haka.funet.fi/shibboleth/images/Haka_nega_tiivis_pieni.svg";
		} else {
			embeddedLogoURL = "https://haka.funet.fi/shibboleth/images/virtu_symboli_text_Valtori_pieni.png";
		}

		// Only show logo if it is not empty
		if (embeddedLogoURL != ''){
			writeHTML('<img id="wayf_logo" src="' + embeddedLogoURL +  '" alt="Federation Logo" style="border:0px; margin-bottom: 4px;">');
		}

		// Write footer of logo div
		writeHTML('</a></div>');
	}

	// Start login check
	// If session exists, we only draw the logged_in_message
	if(
		wayf_hide_after_login
		&& user_logged_in
	){
		writeHTML('<p id="wayf_intro_div" style="float:left;font-size:' + wayf_font_size + 'px;color:' + wayf_font_color + ';">' + wayf_logged_in_messsage + '</p>');

	} else {
	// Else draw embedded WAYF

		// Draw intro text
		writeHTML('<label for="user_idp" id="wayf_intro_label" style="float:left; min-width:80px; font-size:' + wayf_font_size + 'px;color:' + wayf_font_color + ';">' + wayf_overwrite_intro_text + '</label>');

		var wayf_authReq_URL = '';
		var form_start = '';

		if (wayf_use_discovery_service === true){
			// New SAML Discovery Service protocol

			wayf_authReq_URL = wayf_URL;

			// Use GET arguments or use configuration parameters
			if (entityIDGETParam != "" && returnGETParam != ""){
				wayf_authReq_URL += '?entityID=' + encodeURIComponent(entityIDGETParam);
				wayf_authReq_URL += '&amp;return=' + encodeURIComponent(returnGETParam);
			} else {
				var return_url = wayf_sp_samlDSURL + getGETArgumentSeparator(wayf_sp_samlDSURL);
				return_url += 'SAMLDS=1&target=' + encodeURIComponent(wayf_return_url);
				wayf_authReq_URL += '?entityID=' + encodeURIComponent(wayf_sp_entityID);
				wayf_authReq_URL += '&amp;return=' + encodeURIComponent(return_url);
			}
		} else {
			// Old Shibboleth WAYF protocol
			wayf_authReq_URL = wayf_URL;
			wayf_authReq_URL += '?providerId=' + encodeURIComponent(wayf_sp_entityID);
			wayf_authReq_URL += '&amp;target=' + encodeURIComponent(wayf_return_url);
			wayf_authReq_URL += '&amp;shire=' + encodeURIComponent(wayf_sp_samlACURL);
			wayf_authReq_URL += '&amp;time=1571211916';
		}

		// Add form element
		form_start = '<form id="IdPList" name="IdPList" method="post" target="_parent" action="' + wayf_authReq_URL + '">';

		// Do auto login if redirect cookie exists
		if ('' != '' && wayf_auto_login){

			// Redirect user automatically to WAYF
			var redirect_url = wayf_authReq_URL.replace(/&amp;/g, '&');

			redirectTo(redirect_url);
			return;
		}

		// Get local cookie
		var saml_idp_cookie = getCookie('_saml_idp');
		var last_idp = '';
		var last_idps = new Array();

		// Get last used IdP from local host cookie
		if (saml_idp_cookie && saml_idp_cookie.length > 0){
			last_idps = saml_idp_cookie.split(/[ \+]/);
			if (last_idps.length > 0){
				last_idp = last_idps[(last_idps.length - 1)];
				if (last_idp.length > 0){
					last_idp = decodeBase64(last_idp);
				}
			}
		}

		// Load additional IdPs from DiscoFeed if feature is enabled
		if (wayf_use_disco_feed){
			wayf_disco_feed_idps = loadDiscoFeedIdPs();

			// Hide IdPs for which SP doesnt have metadata and add unknown IdPs
			// Add to additional IdPs
			processDiscoFeedIdPs(wayf_disco_feed_idps);
		}

		// Sort additional IdPs and add IdPs to sorted associative array of other federation IdPs
		if (wayf_additional_idps.length > 0){
			wayf_additional_idps.sort(sortEntities);

			for ( var i = 0; i < wayf_additional_idps.length; i++){
				var IdP = wayf_additional_idps[i];

				if (!IdP){
					continue;
				}

				if (IdP.entityID && last_idp !== '' && IdP.entityID == last_idp){
					IdP.selected = true;
				} else if (IdP.entityID && last_idp === '' && IdP.entityID == wayf_default_idp){
					IdP.selected = true;
				}

				if (!IdP.type){
					IdP.type = "unknown";
				}

				if (!IdP.data){
					IdP.data = IdP.name;
				}

				wayf_other_fed_idps[IdP.entityID] = IdP;
			}
		}

		// Set default IdP if no last used IdP exists
		if (last_idp === '' && wayf_default_idp !== ''){
			if (wayf_idps[wayf_default_idp]){
				wayf_idps[wayf_default_idp].selected = true;
			}
		}


		writeHTML(form_start);
		writeHTML('<input name="request_type" type="hidden" value="embedded">');
		writeHTML('<select id="user_idp" name="user_idp" style="margin-top: 6px; width: 100%;">');

		// Add first entry: "Select your IdP..."
		writeHTML('<option value="-">Select the organisation you are affiliated with ...</option>');

		// Last used
		if (wayf_show_categories === true && wayf_num_last_used_idps > 0 && last_idps.length > 0){

			// Add new category
			var category = "wayf_last_used_idps";
			wayf_categories.wayf_last_used_idps = {
				"type": category,
				"name": wayf_overwrite_last_used_idps_text
			}

			var IdPElements = '';
			var counter = wayf_num_last_used_idps;
			for ( var i= (last_idps.length - 1); i >= 0; i--){

				if (counter <= 0){
					break;
				}

				var currentIdP = decodeBase64(last_idps[i]);

				// Skip if IdP is hidden explicitly
				if (!isAllowedIdP(currentIdP)){
					continue;
				}

				// Add option
				var content = getOptionHTML(currentIdP);

				if (content != ''){
					counter--;
					IdPElements += content;
				}

			}

			writeOptGroup(IdPElements, category);
		}

		// Most used and Favourites
		if (wayf_show_categories === true && wayf_most_used_idps.length > 0){

			// Add new category
			var category = "wayf_most_used_idps";
			wayf_categories.wayf_most_used_idps = {
				"type": category,
				"name": wayf_overwrite_most_used_idps_text
			}

			// Show most used IdPs in the order they are defined
			var IdPElements = '';
			for ( var i=0; i < wayf_most_used_idps.length; i++){

				var currentIdP = wayf_most_used_idps[i];

				// Add option if IdP exists in lists of IdPs
				if (wayf_idps[currentIdP] || wayf_other_fed_idps[currentIdP]){
					IdPElements += getOptionHTML(currentIdP);
				}
			}

			writeOptGroup(IdPElements, category);
		}

		// Draw drop down list
		var category = '';
		var IdPElements = '';
		for(var entityID in wayf_idps){

			var idp_type = wayf_idps[entityID].type;

			// Draw category
			if (category != idp_type){

				// Finish category if a new one starts that exists
				if (IdPElements != ''){
					writeOptGroup(IdPElements, category);
				}

				// Reset content
				IdPElements = '';
			}

			// Add IdP if it is allowed
			if (isAllowedIdP(entityID)){
				IdPElements += getOptionHTML(entityID);
			}

			// Set current category/type
			category = idp_type;
		}

		// Output last remaining element
		writeOptGroup(IdPElements, category);

		// Show IdPs from other federations
		if ( ! isEmptyObject(wayf_other_fed_idps)){

			// Add new category
			var category = "wayf_other_federations_idps";
			wayf_categories.wayf_other_federations_idps = {
				"type": category,
				"name": wayf_overwrite_from_other_federations_text
			}

			// Show additional IdPs
			var IdPElements = '';
			for (entityID in wayf_other_fed_idps){
				if (isAllowedIdP(entityID)){
					IdPElements += getOptionHTML(entityID)
				}
			}

			writeOptGroup(IdPElements, category);
		}

		writeHTML('</select>');

		// Do we have to show the remember settings checkbox?
		if (wayf_show_remember_checkbox){

			// Draw checkbox table
			writeHTML('<div id="wayf_remember_checkbox_div" style="float: left;margin-top:6px;"><table style="border: 0; border-collapse: collapse;"><tr><td style="vertical-align: top;">');

			// Is the checkbox forced to be checked
			if (wayf_force_remember_for_session){
				// First draw the dummy checkbox ...
				writeHTML('<input id="wayf_remember_checkbox" type="checkbox" name="session_dummy" value="true" checked="checked" disabled="disabled" style="margin:2px 2px 0 0; border: 0; padding:0;">');
				// ... and now the real but hidden checkbox
				writeHTML('<input type="hidden" name="session" value="true">');
			} else {
				writeHTML('<input id="wayf_remember_checkbox" type="checkbox" name="session" value="true"  style="margin:2px 2px 0 0; border: 0; padding:0;">');
			}

			// Draw label
			writeHTML('</td><td style="vertical-align: top;"><label for="wayf_remember_checkbox" id="wayf_remember_checkbox_label" style="font-size:' + wayf_font_size + 'px;color:' + wayf_font_color + ';">' + wayf_overwrite_checkbox_label_text + '</label>');

			writeHTML('</td></tr></table></div>');
		} else if (wayf_force_remember_for_session){
			// Is the checkbox forced to be checked but hidden
			writeHTML('<input id="wayf_remember_checkbox" type="hidden" name="session" value="true">');
		}


		// Draw submit button
		writeHTML('<input id="wayf_submit_button" type="submit" name="Login" accesskey="s" value="' + wayf_overwrite_submit_button_text + '" style="float: right; margin-top:6px;">');

		// Close form
		writeHTML('</form>');

	}  // End login check

	// Close box
	writeHTML('</div>');

	// Now output HTML all at once
	document.write(wayf_html);

	// Ensure that form submit calls validation function
	document.getElementById("IdPList").addEventListener("submit", function(eventObj){
		submitForm(eventObj);
	});

	// Load JQuery and improved drop down list code if feature is enabled
	if (wayf_use_improved_drop_down_list){
		// Check if jQuery is alread loaded or version is older that this version's
		if (typeof jQuery === "undefined"){
			loadJQuery();
		} else {
			// Check JQuery version and load our version if it is newer
			var version = jQuery.fn.jquery.split('.');
			var versionMajor = parseFloat(version[0]);
			var versionMinor = parseFloat(version[1]);
			if (versionMajor <= 1 && versionMinor < 5){
				loadJQuery();
			} else {
				loadImprovedDropDown();
			}
		}
	}
})();

});
