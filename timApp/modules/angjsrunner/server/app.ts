import path from "path";
import cookieParser from "cookie-parser";
import type {ErrorRequestHandler} from "express";
import express from "express";
import createError from "http-errors";
import logger from "morgan";
import answerRouter from "./routes/answer";
import multihtmlRouter from "./routes/multihtml";
import routes from "./routes/reqs";
import runScriptRouter from "./routes/runscript";

const app = express();

function escapeHtml(s: unknown): string {
    // eslint-disable-next-line @typescript-eslint/no-base-to-string
    return String(s ?? "")
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#39;");
}

app.use(logger("dev"));
app.use(express.json({limit: "100mb"}));
app.use(express.urlencoded({limit: "100mb", extended: false}));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, "public")));

app.use("/multihtml", multihtmlRouter);
app.use("/answer", answerRouter);
app.use("/runScript", runScriptRouter);
app.use("/reqs", routes);

// catch 404 and forward to error handler
app.use((req, res, next) => {
    next(createError(404));
});

// error handler
app.use(((err, req, res, next) => {
    // set locals, only providing error in development

    /* eslint-disable @typescript-eslint/no-unsafe-member-access */
    res.locals.message = err.message;
    res.locals.error = req.app.get("env") === "development" ? err : {};

    // render the error page
    // eslint-disable-next-line @typescript-eslint/prefer-nullish-coalescing
    res.status(err.status || 500);
    /* eslint-enable @typescript-eslint/no-unsafe-member-access */

    const error = res.locals.error as {status?: unknown; stack?: unknown};
    res.type("html").send(
        `<!DOCTYPE html><html><head><title></title></head><body><h1>${escapeHtml(
            res.locals.message
        )}</h1><h2>${escapeHtml(error.status)}</h2><pre>${escapeHtml(
            error.stack
        )}</pre></body></html>`
    );
}) as ErrorRequestHandler);

export default app;
