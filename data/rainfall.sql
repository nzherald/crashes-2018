BEGIN;

DROP TABLE IF EXISTS rainfall;
CREATE TABLE rainfall (day date, rain double precision, site text);

\copy rainfall from pstdin with csv header null 'NA'

COMMIT;