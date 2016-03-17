DROP TABLE IF EXISTS Velp;
CREATE TABLE IF NOT EXISTS Velp (
  id              INTEGER       NOT NULL,
  creator_id      INTEGER       NOT NULL,
  creation_time   TIMESTAMP     NOT NULL,
  default_points  INTEGER       NOT NULL  DEFAULT 0,      -- change to some better type?
  icon_id         INTEGER       NOT NULL,
  valid_until     TIMESTAMP,

  CONSTRAINT Velp_PK
  PRIMARY KEY (id),

  CONSTRAINT icon_id_FK
  FOREIGN KEY (icon_id)
  REFERENCES icon (id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);

DROP TABLE IF EXISTS Velp_in_annotation_setting;
CREATE TABLE Velp_in_annotation_setting (
  id              INTEGER NOT NULL,
  velp_id         INTEGER NOT NULL,
  default_points  INTEGER NOT NULL,                       -- change to some better type?
  velp_hidden     BOOLEAN NOT NULL,

  CONSTRAINT Velp_in_annotation_setting_PK
  PRIMARY KEY (id),

  CONSTRAINT velp_id_FK
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

/*
Add annotation setting locations here pls plox
 */


DROP TABLE IF EXISTS Icon;
CREATE TABLE IF NOT EXISTS Icon (
  id        INT NOT NULL,
  filename  TEXT,

  CONSTRAINT Icon_PK
  PRIMARY KEY (id)
);


DROP TABLE IF EXISTS Label;
CREATE TABLE IF NOT EXISTS Label (
  id          INTEGER     NOT NULL,
  language_id VARCHAR(2)  NOT NULL,
  content     Text,

  CONSTRAINT Label_PK
  PRIMARY KEY (id, language_id)
);


DROP TABLE IF EXISTS Label_in_velp;
CREATE TABLE IF NOT EXISTS Label_in_velp (
  label_id  INTEGER NOT NULL,
  velp_id   INTEGER NOT NULL,

  CONSTRAINT Label_in_velp_PK
  PRIMARY KEY (label_id, velp_id),

  CONSTRAINT label_id_FK
  FOREIGN KEY (label_id)
  REFERENCES Label(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT velp_id_FK
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_version;
CREATE TABLE IF NOT EXISTS Velp_version (
  id          INTEGER   NOT NULL,
  velp_id     INTEGER   NOT NULL,
  version     INTEGER   NOT NULL,
  modify_time TIMESTAMP NOT NULL,

  CONSTRAINT Velp_version_PK
  PRIMARY KEY (id),

  CONSTRAINT velp_id_FK
  FOREIGN KEY (velp_id)
  REFERENCES  Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_content;
CREATE TABLE IF NOT EXISTS Velp_version (
  id          INTEGER     NOT NULL,
  language_id VARCHAR(2)  NOT NULL,
  version_id  INTEGER     NOT NULL,
  content     TEXT,
  PRIMARY KEY (id),

  CONSTRAINT Velp_content_id_PK
  PRIMARY KEY (id),

  CONSTRAINT version_id_FK
  FOREIGN KEY (version_id)
  REFERENCES Velp_version(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Annotation;
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

  CONSTRAINT version_id_FK
  FOREIGN KEY (version_id)
  REFERENCES Velp_version(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT icon_id_FK
  FOREIGN KEY (icon_id)
  REFERENCES Icon(id)
  ON DELETE SET NULL
  ON UPDATE CASCADE
);


-- NOT FINISHED THIS ONE DO SOMETHING ANYTHING I DON'T EVEN KNOW MY WORLD RIGHT NOW
DROP TABLE  IF EXISTS Annotation_visibility;
CREATE TABLE Annotation_visibility (
  annotation_id INTEGER NOT NULL,

  CONSTRAINT annoation_id_FK
  FOREIGN KEY (annotation_id)
  REFERENCES Annotation(id)
);
-- READ ABOVE THEN YOU CAN READ BELOW OR RIGHT OF LEFT OR WHEREVER YOU WANT UP TO YOU


DROP TABLE IF EXISTS Comment;
CREATE TABLE Comment (
  id            INTEGER   NOT NULL,
  annotation_id INTEGER   NOT NULL,
  comment_time  TIMESTAMP NOT NULL,
  commenter_id  INTEGER   NOT NULL,
  content       TEXT,

  CONSTRAINT Comment_PK
  PRIMARY KEY (id),

  CONSTRAINT annotation_id_FK
  FOREIGN KEY (annotation_id)
  REFERENCES Annotation(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_group;
CREATE TABLE Velp_group (
  id            INTEGER   NOT NULL,
  name          TEXT      NOT NULL,
  block_id      INTEGER   NOT NULL,
  creation_time TIMESTAMP NOT NULL,
  valid_until   TIMESTAMP,

  CONSTRAINT Velp_group_PK
  PRIMARY KEY (id)
);


DROP TABLE IF EXISTS Velp_in_group;
CREATE TABLE Velp_in_group (
  velp_group_id INTEGER NOT NULL,
  velp_id       INTEGER NOT NULL,

  CONSTRAINT Velp_in_group_PK
  PRIMARY KEY (velp_group_id, velp_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT velp_id_FK
  FOREIGN KEY (velp_id)
  REFERENCES Velp(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_group_in_document;
CREATE TABLE Velp_group_in_document (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,

  CONSTRAINT Velp_group_in_document_PK
  PRIMARY KEY (velp_group_id, document_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_group_in_paragraph;
CREATE TABLE Velp_group_in_paragraph (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,
  paragraph_id  TEXT    NOT NULL,

  CONSTRAINT Velp_group_in_paragraph_PK
  PRIMARY KEY (velp_group_id, document_id, paragraph_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_group_in_area;
CREATE TABLE Velp_group_in_area (
  velp_group_id INTEGER NOT NULL,
  document_id   INTEGER NOT NULL,
  area_id       TEXT    NOT NULL,

  CONSTRAINT Velp_group_in_area_PK
  PRIMARY KEY (velp_group_id, document_id, area_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


DROP TABLE IF EXISTS Velp_group_in_folder;
CREATE TABLE Velp_group_in_folder (
  velp_group_id INTEGER NOT NULL,
  folder_id     INTEGER NOT NULL,

  CONSTRAINT Velp_group_in_folder_PK
  PRIMARY KEY (velp_group_id, folder_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);

DROP TABLE IF EXISTS Velp_group_label;
CREATE TABLE Velp_group_label (
  id      INTEGER NOT NULL,
  content TEXT    NOT NULL,

  CONSTRAINT Velp_group_label_id_PK
  PRIMARY KEY (id)
);

DROP TABLE IF EXISTS Label_in_velp_group;
CREATE TABLE Label_in_velp_group (
  velp_group_id   INTEGER NOT NULL,
  group_label_id  INTEGER NOT NULL,

  CONSTRAINT Label_in_velp_group_PK
  PRIMARY KEY (velp_group_id, group_label_id),

  CONSTRAINT velp_group_id_FK
  FOREIGN KEY (velp_group_id)
  REFERENCES Velp_group(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE,

  CONSTRAINT label_group_id_FK
  FOREIGN KEY (group_label_id)
  REFERENCES Velp_group_label(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);


  /*
    CONSTRAINT `fk_phrase_in_group_phrase_group1`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE RESTRICT
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_phrase_in_group_phrase1`
    FOREIGN KEY (`phrase_id`)
    REFERENCES `mydb`.`phrase` (`id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION)
   */




/*
Raw sql generated from the plan in workbench. Recreate and repaste as necessary.
-- MySQL Script generated by MySQL Workbench
-- 03/17/16 16:40:42
-- Model: New Model    Version: 1.0
-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `mydb` DEFAULT CHARACTER SET utf8 ;
USE `mydb` ;

-- -----------------------------------------------------
-- Table `mydb`.`icon`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`icon` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `filename` TEXT NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `creator_id` INT NOT NULL,
  `creation_time` TIMESTAMP NOT NULL,
  `default_points` INT NOT NULL DEFAULT 0,
  `icon_id` INT NOT NULL,
  `valid_until` TIMESTAMP NULL,
  PRIMARY KEY (`id`),
  INDEX `icon_id_idx` (`icon_id` ASC),
  CONSTRAINT `icon_id`
    FOREIGN KEY (`icon_id`)
    REFERENCES `mydb`.`icon` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);


-- -----------------------------------------------------
-- Table `mydb`.`phrase_version`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_version` (
  `id` INT NOT NULL,
  `phrase_id` INT NOT NULL,
  `version` INT NOT NULL,
  `modify_time` TIMESTAMP NOT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_phrase_version_phrase1_idx` (`phrase_id` ASC),
  CONSTRAINT `fk_phrase_version_phrase1`
    FOREIGN KEY (`phrase_id`)
    REFERENCES `mydb`.`phrase` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_content`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_content` (
  `id` INT NOT NULL,
  `language_id` VARCHAR(2) NOT NULL,
  `version_id` INT NULL,
  `content` TEXT NULL,
  PRIMARY KEY (`id`),
  INDEX `version_language_index` (`language_id` ASC, `version_id` ASC),
  CONSTRAINT `fk_phrase_content_phrase_version1`
    FOREIGN KEY (`version_id`)
    REFERENCES `mydb`.`phrase_version` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`label`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`label` (
  `id` INT NOT NULL,
  `language_id` VARCHAR(2) NOT NULL,
  `content` TEXT NULL,
  PRIMARY KEY (`id`, `language_id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`label_in_phrase`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`label_in_phrase` (
  `label_id` INT NOT NULL,
  `phrase_id` INT NOT NULL,
  PRIMARY KEY (`label_id`, `phrase_id`),
  INDEX `fk_labels_in_phrase_phrase1_idx` (`phrase_id` ASC),
  CONSTRAINT `fk_labels_label_content1`
    FOREIGN KEY (`label_id`)
    REFERENCES `mydb`.`label` (`id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_labels_in_phrase_phrase1`
    FOREIGN KEY (`phrase_id`)
    REFERENCES `mydb`.`phrase` (`id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group` (
  `id` INT NOT NULL,
  `name` TEXT NULL,
  `block_id` INT NOT NULL,
  `creation_time` TIMESTAMP NULL,
  `valid_until` TIMESTAMP NULL,
  PRIMARY KEY (`id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_in_group`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_in_group` (
  `phrase_group_id` INT NOT NULL,
  `phrase_id` INT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `phrase_id`),
  INDEX `fk_phrase_in_group_phrase1_idx` (`phrase_id` ASC),
  CONSTRAINT `fk_phrase_in_group_phrase_group1`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE RESTRICT
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_phrase_in_group_phrase1`
    FOREIGN KEY (`phrase_id`)
    REFERENCES `mydb`.`phrase` (`id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group_labels`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group_labels` (
  `group_label_id` INT NOT NULL,
  `content` TEXT NULL,
  PRIMARY KEY (`group_label_id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`label_in_phrase_group`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`label_in_phrase_group` (
  `phrase_group_id` INT NOT NULL,
  `group_label_id` INT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `group_label_id`),
  INDEX `fk_labels_in_phrase_group_phrase_group_labels1_idx` (`group_label_id` ASC),
  CONSTRAINT `fk_labels_in_phrase_group_phrase_group1`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_labels_in_phrase_group_phrase_group_labels1`
    FOREIGN KEY (`group_label_id`)
    REFERENCES `mydb`.`phrase_group_labels` (`group_label_id`)
    ON DELETE CASCADE
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group_in_document`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group_in_document` (
  `phrase_group_id` INT NOT NULL,
  `document_id` INT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `document_id`),
  CONSTRAINT `fk_phrase_group_in_document_phrase_group1`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group_in_area`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group_in_area` (
  `phrase_group_id` INT NOT NULL,
  `document_id` INT NOT NULL,
  `area_id` TEXT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `document_id`, `area_id`),
  CONSTRAINT `fk_phrase_group_in_document_phrase_group10`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group_in_paragraph`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group_in_paragraph` (
  `phrase_group_id` INT NOT NULL,
  `document_id` INT NOT NULL,
  `paragraph_id` TEXT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `document_id`, `paragraph_id`),
  CONSTRAINT `fk_phrase_group_in_document_phrase_group11`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrase_group_in_folder`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrase_group_in_folder` (
  `phrase_group_id` INT NOT NULL,
  `folder_id` INT NOT NULL,
  PRIMARY KEY (`phrase_group_id`, `folder_id`),
  CONSTRAINT `fk_phrase_group_in_document_phrase_group12`
    FOREIGN KEY (`phrase_group_id`)
    REFERENCES `mydb`.`phrase_group` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`annotation`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`annotation` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `version_id` INT NOT NULL,
  `points` INT NULL,
  `time` TIMESTAMP NULL,
  `icon_id` INT NULL,
  `annotator_id` INT NULL,
  `answer_id` INT NULL,
  `document_id` INT NULL,
  `paragraph_id` TEXT NULL,
  `place_start` INT NULL,
  `place_end` INT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_annotation_phrase_version1_idx` (`version_id` ASC),
  INDEX `fk_annotation_icon1_idx` (`icon_id` ASC),
  CONSTRAINT `fk_annotation_phrase_version1`
    FOREIGN KEY (`version_id`)
    REFERENCES `mydb`.`phrase_version` (`id`)
    ON DELETE RESTRICT
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_annotation_icon1`
    FOREIGN KEY (`icon_id`)
    REFERENCES `mydb`.`icon` (`id`)
    ON DELETE SET NULL
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`comment`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`comment` (
  `id` INT NOT NULL,
  `annotation_id` INT NOT NULL,
  `comment_time` TIMESTAMP NOT NULL,
  `commenter_id` INT NULL,
  `content` TEXT NULL,
  PRIMARY KEY (`id`, `annotation_id`),
  INDEX `fk_comment_annotation1_idx` (`annotation_id` ASC),
  CONSTRAINT `fk_comment_annotation1`
    FOREIGN KEY (`annotation_id`)
    REFERENCES `mydb`.`annotation` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`phrases_in_annotation_setting`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`phrases_in_annotation_setting` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `phrase_id` INT NOT NULL,
  `default_points` INT NULL,
  `phrase_hidden` TINYINT(1) NULL,
  PRIMARY KEY (`id`, `phrase_id`),
  INDEX `fk_annotation_setting_phrase1_idx` (`phrase_id` ASC),
  CONSTRAINT `fk_annotation_setting_phrase1`
    FOREIGN KEY (`phrase_id`)
    REFERENCES `mydb`.`phrase` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`annotation_setting_in_document`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`annotation_setting_in_document` (
  `annotation_setting_id` INT NOT NULL,
  `document_id` INT NOT NULL,
  PRIMARY KEY (`document_id`, `annotation_setting_id`),
  CONSTRAINT `fk_annotation_setting_in_document_annotation_setting1`
    FOREIGN KEY (`annotation_setting_id`)
    REFERENCES `mydb`.`phrases_in_annotation_setting` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `mydb`.`annotation_visibility`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `mydb`.`annotation_visibility` (
  `annotation_id` INT NOT NULL,
  `usergroup_id` INT NOT NULL,
  PRIMARY KEY (`annotation_id`),
  CONSTRAINT `fk_annotation_visibility_annotation1`
    FOREIGN KEY (`annotation_id`)
    REFERENCES `mydb`.`annotation` (`id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
*/