CREATE TABLE cache (
key text NOT NULL,
value bytea NOT NULL
);

CREATE UNIQUE INDEX cache_unq ON cache (key);
