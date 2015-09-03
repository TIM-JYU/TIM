ALTER TABLE Question RENAME TO QuestionOld;

CREATE TABLE Question (
  question_id    INTEGER NOT NULL PRIMARY KEY,
  doc_id         INTEGER NOT NULL,
  par_id         TEXT    NOT NULL,
  question_title TEXT    NOT NULL,
  answer         TEXT,
  questionJson   TEXT,
  points         TEXT
);

INSERT INTO Question (question_id, doc_id, par_id, question_title, answer, questionJson)
SELECT question_id, doc_id, par_id, question_title, answer, questionJson FROM QuestionOld;

DROP TABLE QuestionOld;
