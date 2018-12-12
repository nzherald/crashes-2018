BEGIN;

DROP TABLE IF EXISTS crashes;

CREATE TEMP TABLE _load (
    day date not null,
    hour text not null,
    severity text not null,
    count int not null,
    _x1 text,
    _x2 text,
    _x3 text,
    _x4 text,
    _x5 text
);

\copy _load from pstdin with csv header

CREATE TABLE crashes AS
SELECT day, nullif(hour, 'Unknown')::integer / 100 as hour, severity, count
FROM _load
;

COMMIT;