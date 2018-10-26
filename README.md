## Introduction

This repository provides a basic demonstration of how to use the Haskell
`docusign-client` package. In particular, it demonstrates how to:

1. Upload an unsigned document to DocuSign.
2. Initiate an embedded signing ceremony.
3. Download a signed document from DocuSign.

To run the sample code, you'll need an account with DocuSign. In particular,
you should have the following information to hand:

* Your account ID (an integer)
* Your account key (a UUID)
* Your account username (normally an email address)
* Your account password

## Building

With cabal:

```
cabal new-build
```

With stack:

```
stack setup
stack build
```

## Running

Run the following, substituting in your own DocuSign account information:

```
<cabal new-exec|stack exec> docusign-example --                       \
  --host      demo.docusign.net                      \
  --port      443                                    \
  --account   1234567                                \
  --username  test@example.com                       \
  --password  mypassword                             \
  --key       01234567-0123-0123-0123-0123456789abc  \
  --input     example-document.pdf                   \
  --output    example-document-signed.pdf            \
  --anchor    signinglocation
```

Note that all of the above arguments are required.

