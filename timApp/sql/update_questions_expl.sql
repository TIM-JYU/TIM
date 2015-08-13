ALTER TABLE Question RENAME TO QuestionOld;

CREATE TABLE Question (
  question_id    INTEGER NOT NULL PRIMARY KEY,
  doc_id         INTEGER NOT NULL,
  par_id         TEXT    NOT NULL,
  question_title TEXT    NOT NULL,
  answer         TEXT,
  questionJson   TEXT,
  points         TEXT,
  expl           TEXT
);

INSERT INTO Question (question_id, doc_id, par_id, question_title, answer, questionJson, points)
SELECT question_id, doc_id, par_id, question_title, answer, questionJson, points FROM QuestionOld;

DROP TABLE QuestionOld;

ALTER TABLE AskedQuestion RENAME TO AskedQuestionOld;

CREATE TABLE AskedQuestion (
  asked_id       INTEGER NOT NULL PRIMARY KEY,
  lecture_id     INTEGER NOT NULL,
  doc_id         INTEGER,
  par_id         INTEGER,
  asked_time     TEXT    NOT NULL,
  points         TEXT    NOT NULL,
  asked_json_id  INTEGER NOT NULL,
  expl           TEXT
);

INSERT INTO AskedQuestion (asked_id, lecture_id, doc_id, par_id, asked_time, points, asked_json_id)
SELECT asked_id, lecture_id, doc_id, par_id, asked_time, points, asked_json_id FROM AskedQuestionOld;

DROP TABLE AskedQuestionOld;

UPDATE Question
SET expl = '{}';
UPDATE AskedQuestion
SET expl = '{}';