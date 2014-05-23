CREATE TABLE cds_guide
(
    guideid VARCHAR(255) NOT NULL,
    guidesrc TEXT NOT NULL,
    guideObject BYTEA NOT NULL,
    compiledguide BYTEA NOT NULL,
    active SMALLINT NOT NULL,
    lastUpdate TIMESTAMP WITHOUT TIME ZONE,
    PRIMARY KEY (guideid)
);

CREATE TABLE cds_overview
(
    overviewid VARCHAR(255) NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    src TEXT NOT NULL,
    PRIMARY KEY (overviewid)
);
