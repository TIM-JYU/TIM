DROP TABLE IF EXISTS Question;

DROP TABLE IF EXISTS AskedQuestion;

DROP TABLE IF EXISTS AskedJson;

CREATE TABLE Question (
  question_id    INTEGER NOT NULL PRIMARY KEY,
  doc_id         INTEGER NOT NULL,
  par_id         TEXT    NOT NULL,
  question_title TEXT    NOT NULL,
  answer         TEXT,
  questionjson   TEXT,
  points         TEXT
);

CREATE TABLE AskedQuestion (
  asked_id       INTEGER NOT NULL PRIMARY KEY,
  lecture_id     INTEGER NOT NULL,
  doc_id         INTEGER,
  par_id         INTEGER,
  asked_time     TEXT    NOT NULL,
  points         TEXT    NOT NULL,
  asked_json_id  INTEGER NOT NULL
);

CREATE TABLE AskedJson (
  asked_json_id  INTEGER NOT NULL PRIMARY KEY,
  json           TEXT    NOT NULL,
  hash           TEXT    NOT NULL
);