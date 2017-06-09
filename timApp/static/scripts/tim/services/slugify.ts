import {timApp} from "tim/app";

timApp.factory("Slugify", [function() {
    "use strict";
    return {
        // from https://stackoverflow.com/a/5782563
        slugify(str) {
            str = str.replace(/^\s+|\s+$/g, ""); // trim
            str = str.toLowerCase();

            // remove accents, swap ñ for n, etc.
            const f = "ãàáäâåẽèéëêìíïîõòóöôùúüûñç·/,:;";
            const t = "aaaaaaeeeeeiiiiooooouuuunc-----";
            for (let i = 0, l = f.length; i < l; i++) {
                str = str.replace(new RegExp(f.charAt(i), "g"), t.charAt(i));
            }

            str = str.replace(/[^a-z0-9 _-]/g, "") // remove invalid chars
                .replace(/\s+/g, "-") // collapse whitespace and replace by -
                .replace(/-+/g, "-"); // collapse dashes

            return str;
        },
    };
}]);
