var express = require('express');
var router = express.Router();

const backTicks = "```";

const templates = [`
${backTicks} {#pistelasku plugin="jsrunner"}
groups:
 -
fields:
 - 
gradingScale:
  1: 10
  2: 20
  3: 30
  4: 40
  5: 50
failGrade: hyl
gradeField: arvosana
creditField: opintopisteet
defaultPoints: 5
program: |!!

!!
${backTicks} `, `
${backTicks}{#pistelasku1 plugin="pali"}
header: Kirjoita palindromi
stem: Kirjoita palindromi, jossa on 7 kirjainta.
-points_array: [[0, 0.1], [0.6, 1]]
inputstem: "Palindromisi:"
needed_len: 7
answerLimit: 4
initword: muikku
cols: 20
${backTicks}`]


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
                                'text': 'JavaScript runner',
                                'expl': 'Add JavaScript runner task',
                            }
                        ],
                    },
                ],
            },
        ],
    });
});

module.exports = router;
