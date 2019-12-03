/* eslint-disable @typescript-eslint/tslint/config */
import express from "express";
console.log("multi");
const router = express.Router();

router.post("/", (req, res, next) => {
    const htmls = [];
    for (const j of req.body) {
        const s = JSON.stringify(j);
        // TODO: Why is 's' encoded with latin1?
        const base64 = Buffer.from(s, "latin1").toString("base64");
        htmls.push(`<js-runner json="${base64}"></js-runner>`);
    }
    res.json(htmls);
});

export default router;
