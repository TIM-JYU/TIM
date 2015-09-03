DROP TABLE IF EXISTS Question;

DROP TABLE IF EXISTS AskedQuestion;

DROP TABLE IF EXISTS AskedJson;

DROP TABLE IF EXISTS Lecture;

DROP TABLE IF EXISTS LectureUsers;

DROP TABLE IF EXISTS LectureAnswer;

DROP TABLE IF EXISTS Message;

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

CREATE TABLE AskedQuestion (
  asked_id       INTEGER NOT NULL PRIMARY KEY,
  lecture_id     INTEGER NOT NULL,
  doc_id         INTEGER,
  par_id         TEXT,
  asked_time     TEXT    NOT NULL,
  points         TEXT    NOT NULL,
  asked_json_id  INTEGER NOT NULL,
  expl           TEXT
);

CREATE TABLE AskedJson (
  asked_json_id  INTEGER NOT NULL PRIMARY KEY,
  json           TEXT    NOT NULL,
  hash           TEXT    NOT NULL
);

CREATE TABLE Lecture (
  lecture_id   INTEGER,
  lecture_code TEXT,
  doc_id       INTEGER NOT NULL,
  lecturer     INTEGER NOT NULL,
  start_time   TEXT    NOT NULL,
  end_time     TEXT,
  password     TEXT,

  PRIMARY KEY (lecture_id)
);

CREATE TABLE LectureUsers (
  lecture_id INTEGER,
  user_id    INTEGER,

  FOREIGN KEY (lecture_id)
  REFERENCES Lecture (lecture_id)
  ON DELETE CASCADE,

  FOREIGN KEY (user_id)
  REFERENCES User (user_id)
  ON DELETE CASCADE
);

CREATE TABLE `Message` (
  msg_id     INTEGER PRIMARY KEY,
  lecture_id INTEGER NOT NULL,
  user_id    INTEGER NOT NULL,
  message    TEXT    NOT NULL,
  timestamp  TEXT    NOT NULL,

  FOREIGN KEY (lecture_id)
  REFERENCES Lecture (lecture_id)
  ON DELETE CASCADE,

  FOREIGN KEY (user_id)
  REFERENCES User (user_id)
  ON DELETE CASCADE
);

CREATE TABLE LectureAnswer (
  answer_id   INTEGER,
  user_id     INTEGER NOT NULL,
  question_id INTEGER NOT NULL,
  lecture_id  INTEGER NOT NULL,
  answer      TEXT    NOT NULL,
  answered_on TEXT    NOT NULL,
  points      REAL,
  PRIMARY KEY (answer_id)
);