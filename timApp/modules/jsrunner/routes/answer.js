var express = require('express');
var router = express.Router();

/* GET users listing. */
router.put('/', function(req, res, next) {
  console.log(req.body.input.data);
  console.log(req.body.markup.program);
  res.send('respond with a resource');
});

module.exports = router;
