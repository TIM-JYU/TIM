import jquery from "jquery";
import moment from "moment";

// Some libraries, such as Bootstrap 3, require jQuery to be globally defined.
// eonasdan-bootstrap-datetimepicker requires Moment.js.
const w = (window as unknown) as Record<string, unknown>;
w.jQuery = jquery;
w.$ = jquery;
w.moment = moment;
