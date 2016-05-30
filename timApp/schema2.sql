DROP TABLE IF EXISTS BlockEditAccess;

DROP TABLE IF EXISTS BlockViewAccess;

DROP TABLE IF EXISTS UserGroupMember;

DROP TABLE IF EXISTS BlockRelation;

DROP TABLE IF EXISTS Block;

DROP TABLE IF EXISTS DocEntry;

DROP TABLE IF EXISTS Translation;

DROP TABLE IF EXISTS Folder;

DROP TABLE IF EXISTS NewUser;

DROP TABLE IF EXISTS User;

DROP TABLE IF EXISTS UserGroup;

DROP TABLE IF EXISTS Answer;

DROP TABLE IF EXISTS UserAnswer;

DROP TABLE IF EXISTS AnswerTag;

DROP TABLE IF EXISTS UserNotes;

DROP TABLE IF EXISTS ReadParagraphs;

DROP TABLE IF EXISTS Question;

DROP TABLE IF EXISTS AskedQuestion;

DROP TABLE IF EXISTS AskedJson;

DROP TABLE IF EXISTS Lecture;

DROP TABLE IF EXISTS LectureUsers;

DROP TABLE IF EXISTS LectureAnswer;

DROP TABLE IF EXISTS Message;

DROP TABLE IF EXISTS Notification;

DROP TABLE IF EXISTS Version;

CREATE TABLE Answer (
  id          INTEGER      NOT NULL,
  task_id     VARCHAR(255) NOT NULL,
  content     VARCHAR(255) NOT NULL,
  points      VARCHAR(255), -- TODO: should this be of type REAL?
  answered_on TIMESTAMP    NOT NULL,
  valid       BOOLEAN,

  CONSTRAINT Answer_PK
  PRIMARY KEY (id)
);

CREATE TABLE UserAnswer (
  id        INTEGER NOT NULL,
  answer_id INTEGER NOT NULL,
  user_id   INTEGER NOT NULL,

  CONSTRAINT UserAnswer_PK
  PRIMARY KEY (id),
  CONSTRAINT UserAnswer_id
  FOREIGN KEY (answer_id)
  REFERENCES Answer (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE AnswerTag (
  id        INTEGER      NOT NULL,
  answer_id INTEGER      NOT NULL,
  tag       VARCHAR(255) NOT NULL,

  CONSTRAINT AnswerTag_PK
  PRIMARY KEY (id),
  CONSTRAINT AnswerTag_id
  FOREIGN KEY (answer_id)
  REFERENCES Answer (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE UserGroup (
  id   INTEGER      NOT NULL,
  name VARCHAR(100) NOT NULL,

  CONSTRAINT UserGroup_PK
  PRIMARY KEY (id)
);


CREATE TABLE User (
  id        INTEGER      NOT NULL,
  name      VARCHAR(100) NOT NULL,
  real_name VARCHAR(100),
  email     VARCHAR(100),
  prefs     TEXT,
  pass      VARCHAR(128),
  yubikey   VARCHAR(12),

  CONSTRAINT User_PK
  PRIMARY KEY (id)
);

CREATE TABLE NewUser (
  email   VARCHAR(100),
  pass    VARCHAR(128),
  created TIMESTAMP,

  CONSTRAINT NewUser_PK
  PRIMARY KEY (email)
);

CREATE TABLE Block (
  id                 INTEGER NOT NULL,
  latest_revision_id INTEGER,
  type_id            INTEGER NOT NULL,
  description        VARCHAR(100),
  created            TIMESTAMP,
  modified           TIMESTAMP,
  UserGroup_id       INTEGER NOT NULL,

  CONSTRAINT Block_PK
  PRIMARY KEY (id),
  CONSTRAINT Block_id
  FOREIGN KEY (UserGroup_id)
  REFERENCES UserGroup (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE DocEntry (
  id     INTEGER      NOT NULL,
  name   VARCHAR(512) NOT NULL,
  public INTEGER      NOT NULL DEFAULT 1,

  CONSTRAINT DocEntry_PK
  PRIMARY KEY (name),
  CONSTRAINT DocEntry_id
  FOREIGN KEY (id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

CREATE TABLE Translation (
  doc_id      INTEGER      NOT NULL,
  src_docid   INTEGER      NOT NULL,
  lang_id     INTEGER      NOT NULL,
  doc_title   VARCHAR(50),

  CONSTRAINT Translation_PK
  PRIMARY KEY (doc_id),

  CONSTRAINT Translation_id
  FOREIGN KEY (doc_id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Translation_src_docid
  FOREIGN KEY (src_docid)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

CREATE TABLE Folder (
  id        INTEGER       NOT NULL,
  name      VARCHAR(50)   NOT NULL,
  location  VARCHAR(512)  NOT NULL,

  CONSTRAINT Folder_PK
  PRIMARY KEY (id),
  CONSTRAINT Folder_id
  FOREIGN KEY (id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

CREATE TABLE BlockRelation (
  parent_block_specifier   INTEGER NOT NULL,
  parent_block_revision_id INTEGER,
  parent_block_id          INTEGER NOT NULL,
  Block_id                 INTEGER NOT NULL,

  CONSTRAINT BlockRelation_PK
  PRIMARY KEY (Block_id),
  CONSTRAINT BlockRelation_id
  FOREIGN KEY (Block_id)
  REFERENCES Block (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE UserGroupMember (
  UserGroup_id INTEGER NOT NULL,
  User_id      INTEGER NOT NULL,

  CONSTRAINT UserGroupMember_PK
  PRIMARY KEY (UserGroup_id, User_id),
  CONSTRAINT UserGroupMember_id
  FOREIGN KEY (UserGroup_id)
  REFERENCES UserGroup (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE,
  CONSTRAINT UserGroupMember_id
  FOREIGN KEY (User_id)
  REFERENCES User (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);


CREATE TABLE BlockAccess (
  accessible_from TIMESTAMP NOT NULL,
  accessible_to   TIMESTAMP,
  Block_id      INTEGER   NOT NULL,
  UserGroup_id  INTEGER   NOT NULL,
  type INTEGER NOT NULL,

  CONSTRAINT BlockAccess_PK
  PRIMARY KEY (Block_id, UserGroup_id, type),

  CONSTRAINT BlockAccess_id
  FOREIGN KEY (Block_id)
  REFERENCES Block (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE,

  CONSTRAINT BlockAccess_id
  FOREIGN KEY (UserGroup_id)
  REFERENCES UserGroup (id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE,

  FOREIGN KEY (type)
  REFERENCES AccessType(id)
  ON DELETE NO ACTION
  ON UPDATE CASCADE
);

CREATE TABLE AccessType (
  id INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

INSERT INTO AccessType(id, name) VALUES (1, 'view');
INSERT INTO AccessType(id, name) VALUES (2, 'edit');
INSERT INTO AccessType(id, name) VALUES (3, 'teacher');
INSERT INTO AccessType(id, name) VALUES (4, 'manage');
INSERT INTO AccessType(id, name) VALUES (5, 'see answers');

CREATE TABLE UserNotes (
  id           INTEGER      NOT NULL,
  UserGroup_id INTEGER      NOT NULL,
  doc_id       INTEGER      NOT NULL,
  par_id       TEXT         NOT NULL,
  par_hash     TEXT         NOT NULL,
  content      VARCHAR(255) NOT NULL,
  created      TIMESTAMP    NOT NULL,
  modified     TIMESTAMP,
  access       VARCHAR(20)  NOT NULL,
  tags         VARCHAR(20)  NOT NULL,
  html         TEXT,

  CONSTRAINT UserNotes_PK
  PRIMARY KEY (id)
);


CREATE TABLE ReadParagraphs (
  UserGroup_id INTEGER   NOT NULL,
  doc_id       INTEGER   NOT NULL,
  par_id       TEXT      NOT NULL,
  par_hash     TEXT      NOT NULL,
  timestamp    TIMESTAMP NOT NULL,

  CONSTRAINT ReadParagraphs_PK
  PRIMARY KEY (UserGroup_id, doc_id, par_id)
);

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
  options      TEXT,

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

CREATE TABLE Notification (
  user_id   INTEGER NOT NULL,
  doc_id    INTEGER NOT NULL,

  email_doc_modify      BOOLEAN NOT NULL DEFAULT FALSE,
  email_comment_add     BOOLEAN NOT NULL DEFAULT FALSE,
  email_comment_modify  BOOLEAN NOT NULL DEFAULT FALSE,

  CONSTRAINT Notification_PK
  PRIMARY KEY (user_id, doc_id),

  CONSTRAINT Notification_docid
  FOREIGN KEY (doc_id)
  REFERENCES Block (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
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

CREATE TABLE Version (
  id INTEGER NOT NULL PRIMARY KEY,
  updated_on TIMESTAMP
);

INSERT INTO Version(id, updated_on) VALUES (7, CURRENT_TIMESTAMP);
