    <script>
    // These are the various possible pages accessible by the user
    // during a Moodle session.
    var intro_pages = "quiz/view.php?";
    var quiz_pages = "quiz/attempt.php?";
    var review_pages = "quiz/review.php?";

    // Pages to ignore.
    //var ignore_pages = ["tool.php?id=9", "quiz/view.php?id=2"];

    // current page url
    var page_url = window.location.href;
    // hacky solution to get at the username stuff
    var user_full = document.getElementsByClassName('logininfo')[0].children[0].innerText.split(" ");
    // all TIM users are prefixed with TIMUSER_
    var prefix = "TIMUSER_";

    // Check if TIM user (as opposed to a regular Moodle user)
    function isValidUser() {
        return (user_full[0].indexOf(prefix) > -1) && user_full[1] === "Learner";
    }

    // Is not a page to ignore
    function isValidPage() {
        var catches = ignore_pages.filter(function(val) {
            return page_url.indexOf(val) > -1;
        });
        //console.log(catches);
        return (catches.length === 0);
    }

    // Is both a TIM user and not an "admin" of some description
    if (isValidUser()) {

        sessionStorage.setItem("TIMUSER", true);

        HTMLCollection.prototype.reduce = Array.prototype.reduce;
        HTMLCollection.prototype.map = Array.prototype.map;
        HTMLCollection.prototype.filter = Array.prototype.filter;

        function isPage(type) {
            var lookup = {
                'intro': intro_pages,
                'quiz': quiz_pages,
                'review': review_pages
            };
            var pageSuffix = lookup[type];
            return (page_url.indexOf(pageSuffix) > -1);
        }

        // Not OAuth'd at the moment, but sends a POST request with an XML body
        // to the /lti/grades route so that Angular can do a GET on the same route
        // using a (user, question)-specific hash, e.g. "GET /lti/grades?hash=123456"
        function sendXMLRequest(sourcedid, score) {
            var xhttp = new XMLHttpRequest();
            var xml = '' +
                '<?xml version = "1.0" encoding = "UTF-8"?>' +
                '<imsx_POXEnvelopeRequest xmlns = "http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">' +
                '<imsx_POXHeader>' +
                '<imsx_POXRequestHeaderInfo>' +
                '<imsx_version>V1.0</imsx_version>' +
                '<imsx_messageIdentifier>999999123</imsx_messageIdentifier>' +
                '</imsx_POXRequestHeaderInfo>' +
                '</imsx_POXHeader>' +
                '<imsx_POXBody>' +
                '<replaceResultRequest>' +
                '<resultRecord>' +
                '<sourcedGUID>' +
                '<sourcedId>' + sourcedid + '</sourcedId>' +
                '</sourcedGUID>' +
                '<result>' +
                '<resultScore>' +
                '<language>en</language>' +
                '<textString>' + score + '</textString>' +
                '</resultScore>' +
                '</result>' +
                '</resultRecord>' +
                '</replaceResultRequest>' +
                '</imsx_POXBody>' +
                '</imsx_POXEnvelopeRequest>';
            xhttp.open("POST", "http://timstack.it.jyu.fi/lti/grades/", true);
            xhttp.setRequestHeader("Content-Type", "text/xml");
            xhttp.send(xml);
        }

        // Convert comma to a period
        function decimalPointify(num) {
            return num.replace(/,/g, ".");
        }

        // Scrape input fields for the answers the user has given.
        function getAnswers() {
            var inputs = document.getElementsByTagName('input');
            var answerInputs = inputs.filter(function(input) {
                return (input.id).indexOf("ans") > -1;
            });
            //console.log(answerInputs);
            return answerInputs;
        }

        // If num is a string, need to test properly if we can consider it to be a valid number
        function isNumeric(num) {
            var n = (typeof num === "string" ? decimalPointify(num) : num);
            return !isNaN(parseFloat(n)) && isFinite(n);
        }

        // Scrape floating point grades from the "grade" <div>s on the various quiz pages.
        function scrapeGrade(gradeDiv) {
            if (gradeDiv) {
                var gradeText = gradeDiv.innerText.split(" ");
                var gradeNums = gradeText.filter(function(val) {
                    return isNumeric(val);
                }).map(function(val) {
                    var n = decimalPointify(val);
                    return parseFloat(n);
                });
                if (gradeNums.length === 2) {
                    return gradeNums[0] / gradeNums[1];
                }
            }
            return undefined;
        }

        // Force a "clean slate" page, where the user can actually provide answers to questions.
        // (as opposed to a "review" page where <input> fields are disabled)
        function forceCleanSlate() {
            var btn = document.getElementsByClassName('mod_quiz-redo_question_button')[0];
            btn.click();
        }

        // Hide extraneous buttons
        function hideButtons() {
            ['im-controls', 'mod_quiz-redo_question_button'].map(function(name) {
                var elemsColl = document.getElementsByClassName(name);
                elemsColl.map(function(elem) {
                    elem.style.display = 'none';
                });
            });
        }

        // Make submitbtns anchor link(s) instead point toward logout link
        // This is so that the user has an "out" and won't be stuck inside a Moodle session.
        function enableLogout() {
            // Find href for logout (with sesskey query)
            var logoutHref = "";
            var menuAnchors = document.getElementsByClassName('menu-action');
            for (var i = 0; i < menuAnchors.length; i++) {
                if (menuAnchors[i].href.indexOf("logout.php?sesskey") > -1) {
                    logoutHref = menuAnchors[i].href;
                    break;
                }
            }

            var submitBtns = document.getElementsByClassName('submitbtns');
            submitBtns = submitBtns.map(function(group) {
                group.innerHTML = '<a href="' + logoutHref + '">Lopeta istunto</a>';
            });
        }

        // Do handling of grade <div>s and scraping (provided no input errors exist)
        function handleGrades() {
            var validation_errs = document.getElementsByClassName('validationerror');
            if (validation_errs.length === 0) {
                // No validation errors --> final answer possibly scrapeable
                var gradeDivs = document.getElementsByClassName('grade');
                console.log(gradeDivs);
                var grade = gradeDivs.reduce(function(prev, curr) {
                    return prev + scrapeGrade(curr);
                }, 0);
                //var answers = getAnswers();
                console.log(grade);
                if (isNumeric(grade)) {
                    console.log(grade);
                    var sourcedid = username + '_' + sessionStorage.getItem("page_url");
                    console.log(sourcedid);
                    sendXMLRequest(sourcedid, grade);
                    enableLogout();
                    hideButtons(); // TODO: placeholder (not globalized for all 'review' pages)
                }
            }
        }

        // Grab username part from user_full
        var username = user_full[0].substring(prefix.length);

        // "Intro" pages
        if (isPage('intro')) {
            console.log("Intro page");
            var page = document.getElementById('page');
            var quizStartDiv = document.getElementsByClassName('quizstartbuttondiv')[0];
            var submitBtn = quizStartDiv.children[0];

            // Next level hackery
            sessionStorage.setItem("page_url", page_url);

            submitBtn.submit(); // Simulate quiz start button click
            page.innerHTML = '<h1 style="text-align: center;">Ladataan...</h1>';

            // force redirection (thanks, Moodle)
            sessionStorage.setItem("redirectToCleanSlate", true);
        }

        // Actual quiz page
        else if (isPage('quiz')) {
            console.log("Quiz page");
            // ensure that redirection does not happen (if everything worked OK)
            sessionStorage.setItem("redirectToCleanSlate", false);
            handleGrades();
        }

        // Review page
        else if (isPage('review')) {
            console.log("Review page");
            handleGrades(); // grade scraping
            if (sessionStorage.getItem("redirectToCleanSlate")) {
                sessionStorage.setItem("redirectToCleanSlate", false);
                //forceCleanSlate();
            }
        }

        // Update pages visited count
        //var pagesVisited = sessionStorage.getItem("pagesVisited");
        //sessionStorage.setItem("pagesVisited", pagesVisited + 1);
    }

    // When logged out and no user info available --> need to refer to sessionStorage
    else if (sessionStorage.getItem("TIMUSER")) {
        var h1Style = "text-align: center;";
        var styles = ["display: flex;",
                    "flex-direction: column;",
                    "justify-content: center;",
                    "height: 100vh;"];
        document.body.innerHTML = '<h1 style="' + h1Style + '">Voit sulkea ikkunan.</h1>';
        document.body.style = styles.join(" ");

        // Reset sessionStorage
        sessionStorage.clear();
    }

</script>
