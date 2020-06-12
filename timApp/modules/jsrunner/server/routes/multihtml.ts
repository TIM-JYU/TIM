/* eslint-disable @typescript-eslint/tslint/config */
import express from "express";

console.log("multi");
const router = express.Router();

router.post("/", (req, res, next) => {
    const htmls = [];
    for (const j of req.body) {
        // TODO: Lift this up to pluginify / render_plugin
        if (j['viewmode']) {
            const siw = j['markup']['showInView'] || false;
            if (!siw) {
                htmls.push(``);
                continue;
            }
        }
        let s = JSON.stringify(j);
        // Escape all non-ascii characters. The base64 string will eventually get passed to "atob" function in browser,
        // which does not handle UTF-8. Solution from: https://stackoverflow.com/a/4901205
        s = s.replace(/[\u007f-\uffff]/g,
            function (c) {
                return '\\u' + ('0000' + c.charCodeAt(0).toString(16)).slice(-4);
            }
        );
        const base64 = Buffer.from(s).toString("base64");
        htmls.push(`<js-runner json="${base64}"></js-runner>`);
    }
    res.json(htmls);
});

export default router;
