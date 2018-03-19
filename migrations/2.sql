ALTER TABLE stipends RENAME COLUMN summer_guarantee TO summer_typical;
ALTER TABLE stipends ADD COLUMN verified_at timestamptz;
