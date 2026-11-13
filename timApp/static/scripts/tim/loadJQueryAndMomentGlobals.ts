import jquery from "jquery";
import moment from "moment";

// Some libraries, such as MathQuill and jQuery UI, require jQuery to be globally defined.
// Scripts in documents may also use the global jQuery and Moment.js.
const w = window as unknown as Record<string, unknown>;
w.jQuery = jquery;
w.$ = jquery;
w.moment = moment;
