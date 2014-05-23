
CREATE TABLE openehr_archetype
(
   archetypeid varchar(255) NOT NULL,
   rmname VARCHAR(50) NOT NULL,
   archetype TEXT NOT NULL,
   aom bytea NOT NULL,
   aobcvo bytea NOT NULL,
   PRIMARY KEY (archetypeid)
);

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
);

CREATE TABLE openehr_terminology
(
    terminologyid VARCHAR(255) NOT NULL,
    src bytea NOT NULL,
    PRIMARY KEY (terminologyid)
);

ALTER TABLE openehr_template ADD CONSTRAINT fk_temlate_archetypeid FOREIGN KEY (archetypeid)
REFERENCES openehr_archetype (archetypeid);
