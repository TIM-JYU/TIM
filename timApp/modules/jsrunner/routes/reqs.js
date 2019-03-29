var express = require('express');
var router = express.Router();

const templates = [`
\`\`\` {#pistelasku plugin="jsrunner"}
fields:
 - 
program:
header: Laske pisteet
stem: Laske sivun pisteet.
\`\`\` `, `
\`\`\`{#pistelasku1 plugin="pali"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
\`\`\``]


/* GET users listing. */
router.get('/', function (req, res, next) {
    res.send({
        js: ["javascripts/build/jsrunner.js"],
        "multihtml": true,
        "css": ["stylesheets/jsrunner.css"],
                'editor_tabs': [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'Jsrunner',
                        'items': [
                            {
                                'data': templates[0].trim(),
                                'text': '5 letters',
                                'expl': 'Add a 5-letter palindrome task',
                            }
                        ],
                    },
                ],
            },
        ],
    });
});

module.exports = router;
