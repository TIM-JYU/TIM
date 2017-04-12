
// All 3rd party libraries without type definitions should be listed here:
declare module 'humanize-duration'
declare module 'angular-messages'
declare module 'angular-timer'
declare module 'angular-eonasdan-datetimepicker'
declare module 'bootstrap-sass'
declare module 'angular-touch'
declare module 'angular-bootstrap-colorpicker'
declare module 'ng-file-upload'
// For some reason, importing ui-grid does not work ("cannot find name 'uiGrid'"), so we declare it manually.
declare module 'ui-grid'
declare module 'katex-auto-render'
declare module 'reveal'
declare module 'ace' {
    import Ace = AceAjax.Ace;
    let ace: Ace;
    export default ace;
}
declare module 'ace/snippets'
declare module 'jqueryui'
declare module 'rangyinputs'

// All inline module definitions that exist in HTML templates should be listed here:
declare module 'tim/plugins'
declare module 'tim/angularmodules'
declare module 'tim/extramodules'
declare module 'tim/session'
declare module 'tim/show_slide_vars'
declare let require: typeof SystemJS.amdRequire;
