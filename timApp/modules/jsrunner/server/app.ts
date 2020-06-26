import path from "path";
import cookieParser from "cookie-parser";
import express, {ErrorRequestHandler} from "express";
import createError from "http-errors";
import logger from "morgan";
import answerRouter from "./routes/answer";
import multihtmlRouter from "./routes/multihtml";
import routes from "./routes/reqs";
import runScriptRouter from "./routes/runscript";

const app = express();

// view engine setup
app.set("views", path.join(__dirname, "views"));
app.set("view engine", "jade");

app.use(logger("dev"));
app.use(express.json({limit: "50mb"}));
app.use(express.urlencoded({extended: false}));
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

  /* eslint-disable @typescript-eslint/tslint/config */
  res.locals.message = err.message;
  res.locals.error = req.app.get("env") === "development" ? err : {};

  // render the error page
  res.status(err.status || 500);
  /* eslint-enable @typescript-eslint/tslint/config */

  res.render("error");
}) as ErrorRequestHandler);

export default app;
