
CREATE TABLE cds_archetype
(
   archetypeid varchar(255) NOT NULL,
   rmname VARCHAR(50) NOT NULL,
   archetype TEXT NOT NULL,
   aom blob,
   PRIMARY KEY (archetypeid)
)  extent size 512 next size 1024 lock mode row;
revoke all on cds_archetype from "public";

CREATE TABLE cds_template
(
    templateid VARCHAR(255) NOT NULL,
    archetypeid VARCHAR(255) NOT NULL,
	rmname VARCHAR(50) NOT NULL,
    archetype TEXT NOT NULL,
    aom blob NOT NULL,
    PRIMARY KEY (templateid)
)  extent size 512 next size 1024 lock mode row;
revoke all on cds_template from "public";

ALTER TABLE cds_template ADD CONSTRAINT FOREIGN KEY (archetypeid) 
REFERENCES cds_archetype (archetypeid) CONSTRAINT fk_temlate_archetypeid;