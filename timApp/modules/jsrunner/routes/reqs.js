var express = require('express');
var router = express.Router();

const backTicks = "```";

const templates = [`
${backTicks} {#runner plugin="jsrunner"}
groups:
 -
fields:
 - 
program: |!!

!!
${backTicks} `, `
${backTicks}{#runner1 plugin="pali"}
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
                                'expl': 'Add basic JavaScript runner task',
                            },
                            {
                                'data': templates[1].trim(),
                                'text': 'Extended JavaScript runner',
                                'expl': 'Add extended JavaScript runner task',
                            },
                        ],
                    },
                ],
            },
        ],
    });
});

module.exports = router;
