DROP TABLE IF EXISTS RunningQuestion;

DROP TABLE IF EXISTS UserShown;

DROP TABLE IF EXISTS UserAnswered;

DROP TABLE IF EXISTS UserExtended;

DROP TABLE IF EXISTS ShowPoints;

DROP TABLE IF EXISTS NewAnswer;

DROP TABLE IF EXISTS UserActivity;

DROP TABLE IF EXISTS PointsShown;

DROP TABLE IF EXISTS PointsClosed;

CREATE TABLE RunningQuestion (
  asked_id    INTEGER NOT NULL PRIMARY KEY,
  lecture_id  INTEGER NOT NULL,
  ask_time    BIGINT NOT NULL,
  end_time    BIGINT
);

CREATE TABLE UserShown (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT UserShown_PK
  PRIMARY KEY (asked_id, user_id)
);

CREATE TABLE UserAnswered (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT UserAnswered_PK
  PRIMARY KEY (asked_id, user_id)
);

CREATE TABLE UserExtended (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT UserExtended_PK
  PRIMARY KEY (asked_id, user_id)
);

CREATE TABLE ShowPoints (
  asked_id    INTEGER NOT NULL PRIMARY KEY,
  lecture_id  INTEGER NOT NULL
);

CREATE TABLE NewAnswer (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT NewAnswer_PK
  PRIMARY KEY (asked_id, user_id)
);

CREATE TABLE UserActivity (
  lecture_id  INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,
  active      TEXT,

  CONSTRAINT UserActivity_PK
  PRIMARY KEY (lecture_id, user_id)
);

CREATE TABLE PointsShown (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT PointsShown_PK
  PRIMARY KEY (asked_id, user_id)
);

CREATE TABLE PointsClosed (
  lecture_id  INTEGER NOT NULL,
  asked_id    INTEGER NOT NULL,
  user_id     INTEGER NOT NULL,

  CONSTRAINT PointsClosed_PK
  PRIMARY KEY (asked_id, user_id)
);