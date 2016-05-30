DROP TABLE IF EXISTS Velp;

DROP TABLE IF EXISTS Icon;

DROP TABLE IF EXISTS VelpLabel;

DROP TABLE IF EXISTS LabelInVelp;

DROP TABLE IF EXISTS VelpVersion;

DROP TABLE IF EXISTS VelpContent;

DROP TABLE IF EXISTS Annotation;

DROP TABLE IF EXISTS AnnotationVisibility;

DROP TABLE IF EXISTS AnnotationComment;

DROP TABLE IF EXISTS VelpGroup;

DROP TABLE IF EXISTS VelpInGroup;

DROP TABLE IF EXISTS VelpGroupSelection;

DROP TABLE IF EXISTS ImportedVelpGroups;

DROP TABLE IF EXISTS VelpGroupDefaults;

DROP TABLE IF EXISTS VelpGroupLabel;

DROP TABLE IF EXISTS LabelInVelpGroup;

DROP VIEW IF EXISTS VelpInformation;

DROP VIEW IF EXISTS VelpGroupInAssessmentArea;


CREATE TABLE Velp (
  id             INTEGER  NOT NULL,
  creator_id     INTEGER  NOT NULL,
  creation_time  DATETIME NOT NULL  DEFAULT CURRENT_TIMESTAMP,
  default_points REAL,
  icon_id        INTEGER,
  valid_from     DATETIME DEFAULT CURRENT_TIMESTAMP,
  valid_until    DATETIME,

  CONSTRAINT Velp_PK
  PRIMARY KEY (id),

  CONSTRAINT Icon_id
  FOREIGN KEY (icon_id)
  REFERENCES Icon (id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);

/* These might not needed. Remember to check delete/update before use.
CREATE TABLE VelpInVelpView (
  velp_view_id   INTEGER NOT NULL,
  velp_id        INTEGER NOT NULL,
  default_points INTEGER NOT NULL, -- change to some better type?
  velp_hidden    BOOLEAN NOT NULL,

  CONSTRAINT VelpInVelpView_PK
  PRIMARY KEY (velp_view_id),

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp (velp_view_id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Velp_view_id
  FOREIGN KEY (velp_view_id)
  REFERENCES VelpView (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

-- At the moment only supports views in a paragraph.
CREATE TABLE VelpView (
  id           INTEGER NOT NULL,
  document_id  INTEGER NOT NULL,
  paragraph_id TEXT    NOT NULL
);
*/

CREATE TABLE Icon (
  id       INT NOT NULL,
  filename TEXT,

  CONSTRAINT Icon_PK
  PRIMARY KEY (id)
);


CREATE TABLE VelpLabel (
  id          INTEGER    NOT NULL,
  language_id VARCHAR(2) NOT NULL,
  content     TEXT,

  CONSTRAINT VelpLabel_PK
  PRIMARY KEY (id, language_id)
);


CREATE TABLE LabelInVelp (
  label_id INTEGER NOT NULL,
  velp_id  INTEGER NOT NULL,

  CONSTRAINT LabelInVelp_PK
  PRIMARY KEY (label_id, velp_id),

  CONSTRAINT Label_id
  FOREIGN KEY (label_id)
  REFERENCES VelpLabel (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpVersion (
  id          INTEGER  NOT NULL,
  velp_id     INTEGER  NOT NULL,
  modify_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT VelpVersion_PK
  PRIMARY KEY (id),

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpContent (
  version_id  INTEGER    NOT NULL,
  language_id VARCHAR(2) NOT NULL,
  content     TEXT,

  CONSTRAINT VelpContent_PK
  PRIMARY KEY (version_id, language_id),

  CONSTRAINT Version_id
  FOREIGN KEY (version_id)
  REFERENCES VelpVersion (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE Annotation (
  id                 INTEGER  NOT NULL,
  velp_version_id    INTEGER  NOT NULL,
  points             REAL,
  creation_time      DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  valid_from         DATETIME DEFAULT CURRENT_TIMESTAMP,
  valid_until        DATETIME,
  icon_id            INTEGER,
  annotator_id       INTEGER  NOT NULL,
  visible_to         INTEGER  NOT NULL,
  document_id        INTEGER  NOT NULL,
  answer_id          INTEGER,
  paragraph_id_start TEXT,
  paragraph_id_end   TEXT,
  offset_start       INTEGER NOT NULL,
  node_start         INTEGER NOT NULL,
  depth_start        INTEGER NOT NULL,
  offset_end         INTEGER NOT NULL,
  node_end           INTEGER NOT NULL,
  depth_end          INTEGER NOT NULL,
  hash_start         TEXT,
  hash_end           TEXT,
  element_path_start TEXT,
  element_path_end   TEXT,


  CONSTRAINT Annotation_PK
  PRIMARY KEY (id),

  CONSTRAINT Velp_version_id
  FOREIGN KEY (velp_version_id)
  REFERENCES VelpVersion (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Icon_id
  FOREIGN KEY (icon_id)
  REFERENCES Icon (id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);


CREATE TABLE AnnotationComment (
  id            INTEGER  NOT NULL,
  annotation_id INTEGER  NOT NULL,
  comment_time  DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  commenter_id  INTEGER  NOT NULL,
  content       TEXT,

  CONSTRAINT AnnotationComment_PK
  PRIMARY KEY (id),

  CONSTRAINT Annotation_id
  FOREIGN KEY (annotation_id)
  REFERENCES Annotation (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroup (
  id            INTEGER  NOT NULL,
  name          TEXT     NOT NULL,
  creation_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  valid_from    DATETIME DEFAULT CURRENT_TIMESTAMP,
  valid_until   DATETIME,
  document_def  BOOLEAN  NOT NULL DEFAULT 0, -- sqlite doesn't have true/false boolean

  CONSTRAINT VelpGroup_PK
  PRIMARY KEY (id)
);


CREATE TABLE VelpInGroup (
  velp_group_id INTEGER NOT NULL,
  velp_id       INTEGER NOT NULL,
  points        REAL,

  CONSTRAINT VelpInGroup_PK
  PRIMARY KEY (velp_group_id, velp_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT Velp_id
  FOREIGN KEY (velp_id)
  REFERENCES Velp (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


CREATE TABLE VelpGroupSelection (
  user_id       INTEGER NOT NULL,
  doc_id        INTEGER NOT NULL,
  target_type   INTEGER NOT NULL,   -- 0 = document, 1 = paragraph, 2 = area
  target_id     TEXT NOT NULL,
  selected      BOOLEAN DEFAULT FALSE,
  velp_group_id INTEGER NOT NULL,

  CONSTRAINT VelpGroupSelection_PK
  PRIMARY KEY (user_id, doc_id, target_id, velp_group_id)
);


CREATE TABLE ImportedVelpGroups (
  user_group    INTEGER NOT NULL,
  doc_id        INTEGER NOT NULL,
  target_type   INTEGER NOT NULL,   -- 0 = document, 1 = paragraph, 2 = area
  target_id     TEXT NOT NULL,
  velp_group_id INTEGER NOT NULL,

  CONSTRAINT ImportedVelpGroups_PK
  PRIMARY KEY (user_group, doc_id, target_id, velp_group_id)
);


CREATE TABLE VelpGroupDefaults (
  doc_id        INTEGER NOT NULL,
  target_type   INTEGER NOT NULL,   -- 0 = document, 1 = paragraph, 2 = area
  target_id     TEXT NOT NULL,
  velp_group_id INTEGER NOT NULL,
  selected      BOOLEAN DEFAULT FALSE,

  CONSTRAINT ImportedVelpGroups_PK
  PRIMARY KEY (doc_id, target_id, velp_group_id)
);


CREATE TABLE LabelInVelpGroup (
  velp_group_id  INTEGER NOT NULL,
  group_label_id INTEGER NOT NULL,

  CONSTRAINT LabelInVelpGroup_PK
  PRIMARY KEY (velp_group_id, group_label_id),

  CONSTRAINT VelpGroup_id
  FOREIGN KEY (velp_group_id)
  REFERENCES VelpGroup (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT LabelGroup_id
  FOREIGN KEY (group_label_id)
  REFERENCES VelpGroupLabel (id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

-- Next up, some views!
CREATE VIEW VelpInformation AS
  SELECT
    VelpVersion.id,
    VelpVersion.velp_id,
    VelpContent.language_id,
    VelpContent.content,
    VelpVersion.modify_time
  FROM VelpVersion
    INNER JOIN VelpContent ON VelpVersion.id = VelpContent.version_id;

-- IMPORTANT! THIS IS EXAMPLE DATA. YOU SHOULD PROBABLY DELETE IT BEFORE RUNNING IN PRODUCTION.
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (1, 1, -2, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (2, 1, -1, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (3, 1, -0.5, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (4, 1, -1, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (5, 1, -2, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (6, 1, 0, NULL, NULL);
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until) VALUES (7, 1, 1, NULL, NULL);
/*A velp that is now longer valid.*/
INSERT INTO Velp (id, creator_id, default_points, icon_id, valid_until)
VALUES (8, 1, -1.5, NULL, datetime('now', '-1 month'));

INSERT INTO VelpVersion (id, velp_id) VALUES (1, 1);
INSERT INTO VelpVersion (id, velp_id) VALUES (2, 1);
INSERT INTO VelpVersion (id, velp_id) VALUES (3, 2);
INSERT INTO VelpVersion (id, velp_id) VALUES (4, 3);
INSERT INTO VelpVersion (id, velp_id) VALUES (5, 4);
INSERT INTO VelpVersion (id, velp_id) VALUES (6, 5);
INSERT INTO VelpVersion (id, velp_id) VALUES (7, 6);
INSERT INTO VelpVersion (id, velp_id) VALUES (8, 7);
INSERT INTO VelpVersion (id, velp_id) VALUES (9, 8);

/*This one will be hidden by default, because there is a newer version.*/
INSERT INTO VelpContent (version_id, language_id, content) VALUES (1, "FI", "Virheellinen alue");
/*This is the latest english version, but there is a newer in finish.*/
INSERT INTO VelpContent (version_id, language_id, content) VALUES (1, "EN", "Erroneus region");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (2, "FI", "Virheellinen ajankohta");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (3, "FI", "Virheellinen henkilö");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (4, "FI", "Kirjoitusvirhe");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (5, "FI", "Kielioppivirhe");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (5, "EN", "Grammatical error");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (6, "FI", "Virheellinen alue");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (7, "FI", "Epäolennaista");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (8, "FI", "Hyvin muistettu!");
INSERT INTO VelpContent (version_id, language_id, content) VALUES (9, "FI", "Vanhentunut velppi");

INSERT INTO VelpGroup (id, name, valid_until) VALUES (1, "Paljon velppejä", NULL);
INSERT INTO VelpGroup (id, name, valid_until) VALUES (2, "Kehuja", NULL);
INSERT INTO VelpGroup (id, name, valid_until) VALUES (3, "Haukkuja (vuh vuh)", NULL);

INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 1);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 2);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 3);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 4);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 5);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 6);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 7);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (1, 8);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (2, 2);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (2, 3);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (2, 5);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (3, 4);
INSERT INTO VelpInGroup (velp_group_id, velp_id) VALUES (3, 6);

INSERT INTO ImportedVelpGroups (user_group, doc_id, target_type, target_id, velp_group_id) VALUES (3, 7, 0, 0, 1);
INSERT INTO ImportedVelpGroups (user_group, doc_id, target_type, target_id, velp_group_id) VALUES (3, 162, 0, 0, 1);

INSERT INTO VelpLabel (id, language_id, content) VALUES (1, "FI", "Historia");
INSERT INTO VelpLabel (id, language_id, content) VALUES (2, "FI", "Waterloo");
INSERT INTO VelpLabel (id, language_id, content) VALUES (3, "FI", "Kielenhuolto");
INSERT INTO VelpLabel (id, language_id, content) VALUES (4, "FI", "Kehut");

INSERT INTO LabelInVelp (label_id, velp_id) VALUES (1, 1);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (1, 2);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (1, 5);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (2, 1);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (2, 2);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (2, 5);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (3, 3);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (3, 4);
INSERT INTO LabelInVelp (label_id, velp_id) VALUES (4, 7);