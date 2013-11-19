
CREATE TABLE openehr_archetype
(
   archetypeid varchar(255) NOT NULL,
   rmname VARCHAR(50) NOT NULL,
   archetype TEXT NOT NULL,
   aom bytea NOT NULL,
   aobcvo bytea NOT NULL,
   PRIMARY KEY (archetypeid)
)  extent size 512 next size 1024 lock mode row;
revoke all on openehr_archetype from "public";

CREATE TABLE openehr_template
(
    templateid VARCHAR(255) NOT NULL,
    archetypeid VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    description TEXT NOT NULL,
	rmname VARCHAR(50) NOT NULL,
    archetype TEXT NOT NULL,
    aom bytea NOT NULL,
    tobcvo  bytea NOT NULL,
    PRIMARY KEY (templateid)
)  extent size 512 next size 1024 lock mode row;
revoke all on openehr_template from "public";

CREATE TABLE openehr_terminology
(
    terminologyid VARCHAR(255) NOT NULL,
    src bytea NOT NULL,
    PRIMARY KEY (terminologyid)
)  extent size 1024 next size 2048 lock mode row;
revoke all on openehr_terminology from "public";

ALTER TABLE openehr_template ADD CONSTRAINT FOREIGN KEY (archetypeid)
REFERENCES openehr_archetype (archetypeid) CONSTRAINT fk_temlate_archetypeid;