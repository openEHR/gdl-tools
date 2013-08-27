CREATE TABLE cds_guide
(
    guideid VARCHAR(255) NOT NULL,
    guidesrc TEXT NOT NULL,
    guideObject blob NOT NULL,
    compiledguide blob NOT NULL,
    active SMALLINT NOT NULL,
    PRIMARY KEY (guideid)
)  extent size 2048 next size 2048 lock mode row;
revoke all on cds_guide from "public";