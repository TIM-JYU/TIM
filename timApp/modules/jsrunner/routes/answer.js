var express = require('express');
var router = express.Router();

router.put('/', function (req, res, next) {

    const uAndF = req.body.input.data;
    console.log(uAndF);
    const doc = req.body.taskID;
    const regex = /[0-9]+\./;
    const currDoc = doc.match(regex);
    const program = req.body.markup.program;
    const markup = req.body.markup;
    console.log(markup);

    const {NodeVM} = require('vm2');
    const vm = new NodeVM({
        console: 'inherit',
        sandbox: {data: uAndF, currDoc, markup},
        require: {
            external: true, // TODO allow only tools.js
        },
        timeout: 1000,
    });
    let result = vm.run(
        `
        const Tools = require('./tools');
        let saveUsersFields = [];
        let output = "";
        let error = "";
        for (const user of data) {
            const tools = new Tools(user, currDoc[0], markup);
            function runProgram(saveUsersFields, output, error) {
                ${program}
            }
            // TODO: try for errors
            runProgram(); 
            saveUsersFields.push(tools.getResult());
            output += tools.getPrint();
            error = tools.getError();
        }
        module.exports = {"res": saveUsersFields, "pr": output, "err": error};
        `,
        "/jsrunner/routes/vm.js",
        );
    console.log(result);
    res.send({'save': result.res, 'web': {'result': 'points saved', 'print': result.pr, 'error': result.err}});

});

module.exports = router;
