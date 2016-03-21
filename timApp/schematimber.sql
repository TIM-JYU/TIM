DROP TABLE IF EXISTS Velp;

DROP TABLE IF EXISTS VelpInAnnotationSetting;

DROP TABLE IF EXISTS Icon;

DROP TABLE IF EXISTS Label;

DROP TABLE IF EXISTS LabelInVelp;

DROP TABLE IF EXISTS VelpVersion;

DROP TABLE IF EXISTS VelpContent;

DROP TABLE IF EXISTS Annotation;

DROP TABLE  IF EXISTS AnnotationVisibility;

DROP TABLE IF EXISTS Comment;

DROP TABLE IF EXISTS VelpGroup;

DROP TABLE IF EXISTS VelpInGroup;

DROP TABLE IF EXISTS VelpGroupInDocument;

DROP TABLE IF EXISTS VelpGroupInParagraph;

DROP TABLE IF EXISTS VelpGroupInArea;

DROP TABLE IF EXISTS VelpGroupInFolder;

DROP TABLE IF EXISTS VelpGroupLabel;

DROP TABLE IF EXISTS LabelInVelpGroup;


CREATE TABLE IF NOT EXISTS Velp (
  id              INTEGER       NOT NULL,
  creator_id      INTEGER       NOT NULL,
  creation_time   TIMESTAMP     NOT NULL,
  default_points  INTEGER       NOT NULL  DEFAULT 0,      -- change to some better type?
  icon_id         INTEGER       NOT NULL,
  valid_until     TIMESTAMP,

  CONSTRAINT Velp_PK
  PRIMARY KEY (id),

  CONSTRAINT Icon_id
  FOREIGN KEY (icon_id)
  REFERENCES Icon(id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);

CREATE TABLE VelpInAnnotationSetting (
  id              INTEGER NOT NULL,
  velp_id         INTEGER NOT NULL,
  default_points  INTEGER NOT NULL,                       -- change to some better type?
  velp_hidden     BOOLEAN NOT NULL,

  CONSTRAINT VelpInAnnotationSetting_PK
  PRIMARY KEY (id),

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

/*
Add annotation setting locations here pls plox
 */


CREATE TABLE Icon (
  id        INT NOT NULL,
  filename  TEXT,

  CONSTRAINT Icon_PK
  PRIMARY KEY (id)
);


CREATE TABLE Label (
  id          INTEGER     NOT NULL,
  language_id VARCHAR(2)  NOT NULL,
  content     Text,

  CONSTRAINT Label_PK
  PRIMARY KEY (id, language_id)
);


CREATE TABLE LabelInVelp (
  label_id  INTEGER NOT NULL,
  velp_id   INTEGER NOT NULL,

  CONSTRAINT LabelInVelp_PK
  PRIMARY KEY (label_id, velp_id),

  CONSTRAINT Label_id
  FOREIGN KEY (label_id)
  REFERENCES Label(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpVersion (
  id          INTEGER   NOT NULL,
  velp_id     INTEGER   NOT NULL,
  version     INTEGER   NOT NULL,
  modify_time TIMESTAMP NOT NULL,

  CONSTRAINT VelpVersion_PK
  PRIMARY KEY (id),

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES  Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpContent (
  version_id  INTEGER     NOT NULL,
  language_id VARCHAR(2)  NOT NULL,
  content     TEXT,

  CONSTRAINT VelpContent_PK
  PRIMARY KEY (version_id, language_id),

  CONSTRAINT Version_id
  FOREIGN KEY (version_id)
  REFERENCES VelpVersion(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE Annotation (
  id            INTEGER   NOT NULL,
  version_id    INTEGER   NOT NULL,
  points        INTEGER,                  -- change to some better type?
  time          TIMESTAMP NOT NULL,
  icon_id       INTEGER,
  annotator_id  INTEGER   NOT NULL,
  answer_id     INTEGER,
  document_id   INTEGER,
  paragraph_id  TEXT,
  place_start   INTEGER   NOT NULL,
  place_end     INTEGER   NOT NULL,

  CONSTRAINT Annotation_PK
  PRIMARY KEY (id),

  CONSTRAINT Version_id
  FOREIGN KEY (version_id)
  REFERENCES VelpVersion(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Icon_id
  FOREIGN KEY (icon_id)
  REFERENCES Icon(id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);


-- NOT FINISHED THIS ONE DO SOMETHING ANYTHING I DON'T EVEN KNOW MY WORLD RIGHT NOW
CREATE TABLE AnnotationVisibility (
  annotation_id INTEGER NOT NULL,

  CONSTRAINT Annotation_id
  FOREIGN KEY (annotation_id)
  REFERENCES Annotation(id)
);
-- READ ABOVE THEN YOU CAN READ BELOW OR RIGHT OF LEFT OR WHEREVER YOU WANT UP TO YOU


CREATE TABLE Comment (
  id            INTEGER   NOT NULL,
  annotation_id INTEGER   NOT NULL,
  comment_time  TIMESTAMP NOT NULL,
  commenter_id  INTEGER   NOT NULL,
  content       TEXT,

  CONSTRAINT Comment_PK
  PRIMARY KEY (id),

  CONSTRAINT Annotation_id
  FOREIGN KEY (annotation_id)
  REFERENCES Annotation(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroup (
  id            INTEGER   NOT NULL,
  name          TEXT      NOT NULL,
  block_id      INTEGER   NOT NULL,
  creation_time TIMESTAMP NOT NULL,
  valid_until   TIMESTAMP,

  CONSTRAINT VelpGroup_PK
  PRIMARY KEY (id)
);


CREATE TABLE VelpInGroup (
  velp_group_id INTEGER NOT NULL,
  velp_id       INTEGER NOT NULL,

  CONSTRAINT VelpInGroup_PK
  PRIMARY KEY (velp_group_id, velp_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupInDocument (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,

  CONSTRAINT VelpGroupInDocument_PK
  PRIMARY KEY (velp_group_id, document_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupInParagraph (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,
  paragraph_id  TEXT    NOT NULL,

  CONSTRAINT VelpGroupInParagraph_PK
  PRIMARY KEY (velp_group_id, document_id, paragraph_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupInArea (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,
  area_id       TEXT    NOT NULL,

  CONSTRAINT VelpGroupInArea_PK
  PRIMARY KEY (velp_group_id, document_id, area_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupInFolder (
  velp_group_id INTEGER NOT NULL,
  folder_id     INTEGER NOT NULL,

  CONSTRAINT VelpGroupInFolder_PK
  PRIMARY KEY (velp_group_id, folder_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupLabel (
  id      INTEGER NOT NULL,
  content TEXT    NOT NULL,

  CONSTRAINT VelpGroupLabel_PK
  PRIMARY KEY (id)
);


CREATE TABLE LabelInVelpGroup (
  velp_group_id   INTEGER NOT NULL,
  group_label_id  INTEGER NOT NULL,

  CONSTRAINT LabelInVelpGroup_PK
  PRIMARY KEY (velp_group_id, group_label_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT LabelGroup_id
  FOREIGN KEY (group_label_id)
  REFERENCES VelpGroupLabel(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);