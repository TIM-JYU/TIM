ALTER TABLE Question RENAME TO QuestionOld;

CREATE TABLE Question (
  question_id    INTEGER NOT NULL PRIMARY KEY,
  doc_id         INTEGER NOT NULL,
  par_id         TEXT    NOT NULL,
  question_title TEXT    NOT NULL,
  answer         TEXT,
  questionjson   TEXT,
  points         TEXT
);

INSERT INTO Question (question_id, doc_id, par_id, question_title, answer, questionjson)
SELECT question_id, doc_id, par_id, question_title, answer, questionjson FROM QuestionOld;

DROP TABLE QuestionOld;
