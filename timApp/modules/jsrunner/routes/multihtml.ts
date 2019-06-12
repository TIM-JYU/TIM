import express from "express";

const router = express.Router();

router.post("/", (req, res, next) => {
    const htmls = [];
    for (const j of req.body) {
        const s = JSON.stringify(j);
        const base64 = Buffer.from(s).toString("base64");
        htmls.push(`<js-runner json="${base64}"></js-runner>`);
    }
    res.send(htmls);
});

export default router;
