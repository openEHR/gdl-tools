CREATE TABLE cds_guide
(
    guideid VARCHAR(255) NOT NULL,
    guidesrc TEXT NOT NULL,
    guideObject BYTEA NOT NULL,
    compiledguide BYTEA NOT NULL,
    active SMALLINT NOT NULL,
    lastUpdate TIMESTAMP WITHOUT TIME ZONE,
    PRIMARY KEY (guideid)
)  extent size 2048 next size 2048 lock mode row;
revoke all on cds_guide from "public";

CREATE TABLE cds_overview
(
    overviewid VARCHAR(255) NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    src TEXT NOT NULL,
    PRIMARY KEY (overviewid)
)  extent size 2048 next size 2048 lock mode row;
revoke all on cds_guide from "public";