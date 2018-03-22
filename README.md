# stipends

# database setup for development

This requires postgresql. Then, as a postgresql super user (usually the user "postgres"), run these (connected to the database "template1"):

    create user stipends with password '111';
    create database stipends_devel;
    grant all on database stipends_devel to stipends;
    create database stipends_test;
    grant all on database stipends_test to stipends;

To create the user & databases for testing & development.

# some notes about the crypto

This is hopefully totally standard RSA + AES public key stuff. For the latter,
relying on the `crypto-simple` library for sane defaults. The former didn't seem
to have any similar wrapper around `cryptonite`, but also didn't seem to have
options, so hopefully it's fine.

- RSA private key is 256 bytes (i.e., 2048 bit). It's a long integer, which gets
  copied into a text field at some point to stick it in the session (this isn't
  "never touch the server" crypto, but it is "only in memory on the server"
  crypto).
- If there is no "secret_key" session key for a curator, we will redirect them
  to a page prompting them to enter it and then redirect back.
- The public key for curators will be an env var (i.e., there is only one
  encryption key, it's not tied to a given curator)
- For each upload, we generate a 32 byte (i.e., 256 bit) AES key.
- We can encrypt the symmetric w/ public key, as the asymmetric key needs to be
  11 bytes longer (and we are way more than that) and store in DB for document.
- We then encrypt the file with symmetric key using crypto-simple.
- To display, we look up the "secret_key" in the session and use that to decrypt
  the symmetric key, and then use that to decrypt the file.

