var express = require('express');
var router = express.Router();

/* GET home page. */
router.post('/', function (req, res, next) {
    const htmls = [];
    for (const j of req.body) {
        //console.log(j);
        const s = JSON.stringify(j);
        const base64 = Buffer.from(s).toString('base64');
        htmls.push(`<js-runner json="${base64}"></js-runner>`);
    }
    res.send(htmls);
});

module.exports = router;
