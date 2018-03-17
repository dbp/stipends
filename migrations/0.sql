CREATE TABLE reporters (
    id serial PRIMARY KEY,
    created_at timestamptz NOT NULL DEFAULT now(),
    fingerprint text NOT NULL,
    token text NOT NULL default md5(random()::text),
    name text,
    trusted_at timestamptz,
    curator_at timestamptz
);

CREATE TABLE stipends (
    id serial PRIMARY KEY,
    created_at timestamptz NOT NULL DEFAULT now(),
    amount integer NOT NULL,
    academic_year text NOT NULL,
    period text NOT NULL,
    summer_guarantee text NOT NULL,
    year_in_program int,
    department text NOT NULL,
    reporter_id integer NOT NULL references reporters(id),
    saw_document boolean NOT NULL,
    notes text NOT NULL
);

CREATE TABLE documents (
    id serial PRIMARY KEY,
    created_at timestamptz NOT NULL DEFAULT now(),
    url text NOT NULL,
    file_type text NOT NULL,
    stipend_id integer NOT NULL references stipends(id),
    verified_at timestamptz
);
