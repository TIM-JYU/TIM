var express = require('express');
var router = express.Router();

/* GET users listing. */
router.put('/', function (req, res, next) {

    const uAndF = req.body.input.data;
    console.log(uAndF);
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
    let result = vm.run(
        `
        const Tools = require('./tools');
        const s = JSON.stringify(data)
        const usersAndFields = JSON.parse(s);
        let r = [];
        for (let user of usersAndFields) {
            const tools = new Tools(user);
            function runProgram() {
                ${program}
            }
            runProgram(); 
            // module.exports = {result: tools.getResult()};
            // console.log("hei");
            r.push(tools.getResult());
        }
        module.exports = r;
        `,
        "/jsrunner/routes/vm.js",
        );
    console.log(result);
    res.send({'save': result, 'web': {'result': 'points saved'}});

});

module.exports = router;
