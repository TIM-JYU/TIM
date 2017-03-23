import * as sessionsettings from "tim/session";
import $ = require("jquery");

export function GetURLParameter(sParam: string) {
    const sPageURL = window.location.search.substring(1);
    const sURLVariables = sPageURL.split('&');
    for (let i = 0; i < sURLVariables.length; i++) {
        const sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) {
            return decodeURIComponent(sParameterName[1]);
        }
    }
}

export function setsetting(setting, value) {
    $.ajax({
        type: 'POST',
        url: '/sessionsetting/' + setting + '/' + value,
        success: function (data) {
            console.log(data);
            sessionsettings[setting] = value;
        },
        error: function () {
            console.log("Could not set setting.");
        }
    });
}
