DROP TABLE UserNotes;
DROP TABLE ReadParagraphs;
DROP TABLE ParMappings;
DROP TABLE ReadRevision;

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
