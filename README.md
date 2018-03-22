# About

This is a database-backed (postgresql) web application (written in Haskell).

# Getting Started

To get it running, install [stack](http://haskellstack.org/), then run `stack
build`, set up the database as follows (technically if you don't want to run the
automated tests, you only need create the `stipends_devel` database):

    create user stipends with password '111';
    create database stipends_devel;
    grant all on database stipends_devel to stipends;
    create database stipends_test;
    grant all on database stipends_test to stipends;

Now create a file `.env`. It should have four lines in it:

    BUCKET_NAME=some.bucket.name.com
    AWS_ACCESS_KEY_ID=AK....
    AWS_SECRET_ACCESS_KEY=....
    PUBLIC_KEY=1234....
    
The first is an AWS S3 bucket, which is where uploaded documents (once
encrypted) will be stored. The AWS credentials that are the next two variables
are for a user that can read & write to that bucket (you should create a bucket,
create a user, and give the user access to just that bucket. This file is
ignored by git, so it shouldn't get committed, and hopefully there won't be any
other way those credentials get leaked, but it's good practice to limit the
scope of what accounts can do so you don't have people spinning up servers to
mine bitcoin). 

The last is a very long integer that is a public RSA key. You can generate this in haskell. Run: `stack ghci` to start an interpreter. Then run the following:

    :set prompt "> "
    import qualified Crypto.PubKey.RSA as RSA
    (pub, priv) <- RSA.generate 256 0x10001
    
This will generate a 256 byte (i.e., 2048bit) RSA key with the exponent 0x10001
(which apparently is a good one). To get the integer for the public key, run:

    RSA.public_n pub
    
And to get the private one, which you should store somewhere (if you want, you
can put a `PRIVATE_KEY`` variable in the same `.env` file -- the application
won't use it, but it's a decent place to keep it), run:

    RSA.private_d priv

Now you are ready to start the application, which you can do with `stack exec
stipends`. It will listen, by default, on port 3000, so open a browser to
`http://localhost:3000`.

Many actions require a `curator`, and currently people can only be made curators
directly via the database. If you first submit a stipend, a new `reporter` will
be created (an anonymous one) for you. You can connect to the database with
`psql stipends_devel` (as the `postgres` superuser or using the credentials set
up above) and then set your user to be trusted and a curator (and give yourself
a name while you're at it): 

    UPDATE reporters SET name = 'My Name', trusted_at = now(), curator_at = now();
    
Now grab the login token for your account:

    SELECT token FROM reporters;
    
And visit `http://localhost:3000/reporter/login/TOKEN`

This will log you in.

You can now view a couple interesting pages:

`/curator/review` -- this shows stipends that have been submitted for you to
review. The intention for this is to filter out duplicates (the very rudimentary
way of detecting these is that IP&User-Agents are used to correlate separate
entries, so you can see the systems best guess of multiple entries by the same
person), obviously silly entries (like $1/year or $1,000,000/year), spam (any
form without a CAPTCHA on the internet will get spam), etc. Nothing gets posted
publicly until it gets approved on this page. The second thing it shows are
stipends with unverified documents. This is important because only curators can
view the documents (as they've been encrypted with the public key you set up
before). Once you've viewed a document, you can click "verify" next to it. This
will mark the document as verified, which will then translate to the entire
stipend being more trusted (the idea of verifying the document is confirming
that it supports what was claimed in the stipend. If it doesn't say what the
stipend says, don't verify it! Or reach out to the person -- likely, the
document includes a way to contact them, even if the stipend is anonymous).

`/curator/organizers` -- this page lists reporters who are trusted, and has a
way to add new ones. It also has their login links. You can then send the link
to them so they can click that. Once they are logged in, any stipends they post
will appear on the site immediately, and they get a boost to their trusted
rating.


# Design

- Anyone can upload, both their own or other peoples
- Public view anyone can see
- What is publicly viewable shows level of confidence

# Challenges

- How to build trust over anonymous data?
   Probably, we want to increase trust as we get correlation.
   
   So if we get many stipends within the same department, that gives us confidence.

   Even without having many in the same dept, having many in the same college gets something.
   
- How to avoid duplicate data?
  
  If our main tactic to deal with anonymity is rely on correlation, what about duplicates?

i.e., if someone just puts in the same entry 10 times. How do we tell that
     apart from 10 people in the same department entering in their data? We
     could do some client fingerprinting (IP address), drop a cookie on their
     browser, etc. Hash that and store that along with the data. Ideally we
     would just ask for a name, but people wouldn't do that. We could ask what
     year they entered the program, maybe that would be sufficiently anonymous
     to be acceptable?

- How is this different from http://www.phdstipends.com
  It should be more accurate! (Requires more work too...)

# Who is uploading

- Random person with own documents
- Random person with own stipend, no documents
- Organizer who asked someone their stipend
- Organizer who saw someone's documents


# some notes about the crypto

This is used to store the supporting documents that people upload. We want them
to only be viewable by curators, and don't want the decryption keys stored on
the server.

This should be totally standard RSA + AES public key stuff. For the latter,
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

