import sessionsettings from "tim/session";
import $ from "jquery";

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

/**
 * Marks seemingly unused imports as used so that TypeScript compiler won't optimize them out when compiling.
 *
 * For example, timApp module needs ngSanitize, but it is specified as a string reference in the angular.module(...)
 * call, which is why TypeScript compiler cannot see the connection to the import statement. See app.ts.
 *
 * @param modules The modules to mark as used.
 */
export function markAsUsed(...modules: any[]) {
    // no need to do anything here
}
