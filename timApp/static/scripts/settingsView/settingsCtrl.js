var settingsApp = angular.module('timApp');

settingsApp.controller('SettingsCtrl', ['$scope', '$http', function (sc, http) {
    sc.settings = settings;
    sc.submit = function (saveUrl) {
        sc.saving = true;
        http.post(saveUrl, sc.settings).success(
            function (data, status, error, headers) {
            }).error(function () {
                alert('Failed to save settings.')
            }).then(function () {
                sc.saving = false;
            });

        sc.updateCss();
    };

    sc.updateCss = function () {
        var css_filenames = sc.getCssNames(false);
        $(".availableCss").each(function () {
            this.disabled = true;
            for (var name in css_filenames) {
                if (css_filenames.hasOwnProperty(name)) {
                    var href = css_filenames[name] + '.css';
                    if (this.href.indexOf(href) > -1) {
                        this.disabled = false;
                    }
                }
            }
        });
    };

    sc.getCssNames = function(noCheck) {
        var css_filenames = [];
        for (var css in sc.settings["css_files"]) {
            if (sc.settings["css_files"].hasOwnProperty(css))
            {
                if (noCheck || sc.settings["css_files"][css])
                {
                    css_filenames.push(css);
                }
            }
        }
        return css_filenames;
    };

    sc.loadCssFiles = function() {
        var css_filenames = sc.getCssNames(true);
        for (var name in css_filenames)
        {
            if (css_filenames.hasOwnProperty(name)) {
                var link = document.createElement('link');
                link.className = 'availableCss';
                link.rel = 'stylesheet';
                link.type = 'text/css';
                link.href = '../static/gen/css/' + css_filenames[name] + '.css';
                document.getElementsByTagName('head')[0].appendChild(link);
                link.disabled = true;
            }
        }
    };

    $(".docEditor").change(function() {
        sc.style.innerHTML = sc.settings.custom_css;
    });

    sc.loadCssFiles();
    sc.updateCss();
    sc.style = document.createElement("style");
    sc.style.type = 'text/css';
    document.getElementsByTagName('head')[0].appendChild(sc.style);
}]);