ALTER TABLE Lecture RENAME TO LectureOld;

CREATE TABLE Lecture (
  lecture_id   INTEGER,
  lecture_code TEXT,
  doc_id       INTEGER NOT NULL,
  lecturer     INTEGER NOT NULL,
  start_time   TEXT    NOT NULL,
  end_time     TEXT,
  password     TEXT,
  options      TEXT,

  PRIMARY KEY (lecture_id)
);

INSERT INTO Lecture (lecture_id, lecture_code, doc_id, lecturer, start_time, end_time, password)
SELECT lecture_id, lecture_code, doc_id, lecturer, start_time, end_time, password FROM LectureOld;

DROP TABLE LectureOld;

UPDATE Lecture
SET options = '{}';