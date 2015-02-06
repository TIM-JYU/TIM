
DROP TABLE IF EXISTS BlockEditAccess;

DROP TABLE IF EXISTS BlockViewAccess;

DROP TABLE IF EXISTS UserGroupMember;

DROP TABLE IF EXISTS ReadRevision;

DROP TABLE IF EXISTS BlockRelation;

DROP TABLE IF EXISTS Block;

DROP TABLE IF EXISTS NewUser;

DROP TABLE IF EXISTS User;

DROP TABLE IF EXISTS UserGroup;

DROP TABLE IF EXISTS Answer;

DROP TABLE IF EXISTS UserAnswer;

DROP TABLE IF EXISTS AnswerTag;

DROP TABLE IF EXISTS ParMappings;

DROP TABLE IF EXISTS UserNotes;

DROP TABLE IF EXISTS ReadParagraphs;

CREATE TABLE Answer (
id INTEGER NOT NULL,
task_id VARCHAR(255) NOT NULL,
content VARCHAR(255) NOT NULL,
points VARCHAR(255),      -- TODO: should this be of type REAL?
answered_on TIMESTAMP NOT NULL,

CONSTRAINT Answer_PK
	PRIMARY KEY (id)
)
;

CREATE TABLE UserAnswer (
id INTEGER NOT NULL,
answer_id INTEGER NOT NULL,
user_id INTEGER NOT NULL,

CONSTRAINT UserAnswer_PK
	PRIMARY KEY (id),
CONSTRAINT UserAnswer_id 
	FOREIGN KEY (answer_id)
	REFERENCES Answer (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;

CREATE TABLE AnswerTag (
id INTEGER NOT NULL,
answer_id INTEGER NOT NULL,
tag VARCHAR(255) NOT NULL,

CONSTRAINT AnswerTag_PK
    PRIMARY KEY (id),
CONSTRAINT AnswerTag_id
	FOREIGN KEY (answer_id)
	REFERENCES Answer (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;

CREATE TABLE UserGroup (
id INTEGER NOT NULL,
name VARCHAR(100) NOT NULL,

CONSTRAINT UserGroup_PK 
	PRIMARY KEY (id)
)
;


CREATE TABLE User (
id INTEGER NOT NULL,
name VARCHAR(100) NOT NULL,
real_name VARCHAR(100),
email VARCHAR(100),
prefs TEXT,
pass VARCHAR(128),

CONSTRAINT User_PK 
	PRIMARY KEY (id)
)
;

CREATE TABLE NewUser (
email VARCHAR(100),
pass VARCHAR(128),
created TIMESTAMP,

CONSTRAINT NewUser_PK
	PRIMARY KEY (email)
)
;

CREATE TABLE Block (
id INTEGER NOT NULL,
latest_revision_id INTEGER,
type_id INTEGER NOT NULL,
description VARCHAR(100), -- better name would be: tag
created TIMESTAMP,
modified TIMESTAMP,
UserGroup_id INTEGER NOT NULL,

CONSTRAINT Block_PK 
	PRIMARY KEY (id),
CONSTRAINT Block_id 
	FOREIGN KEY (UserGroup_id)
	REFERENCES UserGroup (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;


CREATE TABLE BlockRelation (
parent_block_specifier INTEGER NOT NULL,
parent_block_revision_id INTEGER,
parent_block_id INTEGER NOT NULL,
Block_id INTEGER NOT NULL,

CONSTRAINT BlockRelation_PK
	PRIMARY KEY (Block_id),
CONSTRAINT BlockRelation_id 
	FOREIGN KEY (Block_id)
	REFERENCES Block (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;


CREATE TABLE ReadRevision (
revision_id INTEGER NOT NULL,
Block_id INTEGER NOT NULL,
User_id INTEGER NOT NULL,

CONSTRAINT ReadRevision_PK
	PRIMARY KEY (Block_id,User_id),
CONSTRAINT ReadRevision_id 
	FOREIGN KEY (Block_id)
	REFERENCES Block (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE,
CONSTRAINT ReadRevision_id 
	FOREIGN KEY (User_id)
	REFERENCES User (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;


CREATE TABLE UserGroupMember (
UserGroup_id INTEGER NOT NULL,
User_id INTEGER NOT NULL,

CONSTRAINT UserGroupMember_PK
	PRIMARY KEY (UserGroup_id,User_id),
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
)
;


CREATE TABLE BlockViewAccess (
visible_from TIMESTAMP NOT NULL,
visible_to TIMESTAMP,
Block_id INTEGER NOT NULL,
UserGroup_id INTEGER NOT NULL,

CONSTRAINT BlockViewAccess_PK
	PRIMARY KEY (Block_id,UserGroup_id),
CONSTRAINT BlockViewAccess_id 
	FOREIGN KEY (Block_id)
	REFERENCES Block (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE,
CONSTRAINT BlockViewAccess_id 
	FOREIGN KEY (UserGroup_id)
	REFERENCES UserGroup (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;


CREATE TABLE BlockEditAccess (
editable_from TIMESTAMP NOT NULL,
editable_to TIMESTAMP,
Block_id INTEGER NOT NULL,
UserGroup_id INTEGER NOT NULL,

CONSTRAINT BlockEditAccess_PK
	PRIMARY KEY (Block_id,UserGroup_id),
CONSTRAINT BlockEditAccess_id 
	FOREIGN KEY (Block_id)
	REFERENCES Block (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE,
CONSTRAINT BlockEditAccess_id 
	FOREIGN KEY (UserGroup_id)
	REFERENCES UserGroup (id)
		ON DELETE NO ACTION
		ON UPDATE CASCADE
)
;

CREATE TABLE ParMappings(
doc_id INTEGER NOT NULL,
doc_ver INTEGER NOT NULL,
par_index INTEGER NOT NULL,
new_ver INTEGER NULL,
new_index INTEGER NULL,
modified BOOLEAN NULL,

CONSTRAINT ParMappings_PK
	PRIMARY KEY (doc_id, doc_ver, par_index)
)
;


CREATE TABLE UserNotes(
UserGroup_id	INTEGER NOT NULL,
doc_id INTEGER NOT NULL,
doc_ver INTEGER NOT NULL,
par_index INTEGER NOT NULL,
note_index INTEGER NOT NULL,
content VARCHAR(255) NOT NULL,
created TIMESTAMP NOT NULL,
modified TIMESTAMP NULL,
access VARCHAR(20) NOT NULL,
tags VARCHAR(20) NOT NULL,
deprecated BOOLEAN,

CONSTRAINT UserNotes_PK
	PRIMARY KEY (UserGroup_id, doc_id, doc_ver, par_index, note_index),

CONSTRAINT UserNotes_id
	FOREIGN KEY (doc_id, doc_ver, par_index)
	REFERENCES ParMappings (doc_id, doc_ver, par_index)
		ON DELETE CASCADE
		ON UPDATE RESTRICT
);


CREATE TABLE ReadParagraphs(
UserGroup_id	INTEGER NOT NULL,
doc_id INTEGER NOT NULL,
doc_ver INTEGER NOT NULL,
par_index INTEGER NOT NULL,
timestamp TIMESTAMP NOT NULL,
deprecated BOOLEAN,

CONSTRAINT ReadParagraphs_PK
	PRIMARY KEY (UserGroup_id, doc_id, doc_ver, par_index),

CONSTRAINT ReadParagraphs_id
	FOREIGN KEY (doc_id, doc_ver, par_index)
	REFERENCES ParMappings (doc_id, doc_ver, par_index)
		ON DELETE CASCADE
		ON UPDATE RESTRICT
);
