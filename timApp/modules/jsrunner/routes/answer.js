var express = require('express');
var router = express.Router();

/* GET users listing. */
router.put('/', function (req, res, next) {

    const uAndF = req.body.input.data;
    // poista doc id:t uAndF
    const program = req.body.markup.program;

    const {NodeVM} = require('vm2');
    const vm = new NodeVM({
        console: 'inherit',
        sandbox: {data: uAndF},
        require: {
            external: true, // TODO allow only tools.js
        },
        timeout: 1000,
    });
    const result = vm.run(
        `
        const Tools = require('./tools');
        const tools = new Tools(data);
        const s = tools.sayHi();
        console.log(s);
        console.log(tools.setDouble("pisteet", 4));
        function runProgram() {
            ${program}
        }
        runProgram();
        module.exports = {result: tools.getResult()};
        `,
        "/jsrunner/routes/vm.js",
        );
    console.log(result);
    res.send({'save': result.result, 'web': {'result': 'points saved'}});

});

module.exports = router;
