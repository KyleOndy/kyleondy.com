---
title: PGP signature verification
published: 2016-01-06 12:00:00
updated: 2016-01-25 12:00:00
subtitle: A primer on how to verify a detached pgp signature is valid
tags: pgp
---

## Quick Version ##

    gpg --verify Kyle.Ondy-Resume.pdf.sig
    gpg --verify Kyle.Ondy-Cover.Letter.pdf.sig

## Details ##

You may have been brought to this page by my resume or cover letter, which I distribute with a detached signature file.
By verifying the signature you confirm that not only that you have received the document exactly as I intended.

Unfortunately not many people are familiar with how [pgp](https://en.wikipedia.org/wiki/Pretty_Good_Privacy) and [gnupg](https://www.gnupg.org) work.
This is a short tutorial on how to verify detached signatures.

### Install GPG ###

Use your package manager or build from source.

### My Public Key ##

Based on your thoroughness, this could be the most involved step.
To quickly digress for a moment, PGP works on public + private key combo that are mathematically tied together.
Cryptographically verifying a document's signatures means only that the signature was created by the associated private key.
The private key could be created by anyone, pretending to be anyone. You need to verify that a person is actually associated with the public key.
Ideally you want to compare the key you get from various sources that you trust to be me.

* [Hosted on my website, named after me](https://www.kyleondy.com/pgp.txt)
* [My keybase.io profile](https://keybase.io/kyleondy)
* [MIT keyserver](https://pgp.mit.edu/)
* Directly contacting my to verify the fingerprint of my key

By comparing key from multipole sources you protect yourself against importing a key that is not truly mine.

### Import My Public Key ###

Once your are sufficiently satisfied the key is valid, import it.

    gpg --import KyleOndy.pgp

### Verifying the Document ###

Make sure the document and signature file, .asc or .sig commonly, are in the same directory.

    gpg --verify <signature file>

### What Now? ###

If the signature has validated, you can be assured you are reading the document as I intended.

If the signature fails, the document has been modified by something at some point.
You can always find a copy of my [most recent and fully detailed resume](/files/Kyle.Ondy-Resume.pdf) and its [signature](/files/Kyle.Ondy-Resume.pdf.sig)
