#!/bin/sh

set -xe

sbcl --non-interactive --load init.lisp --eval "(asdf:make :ledger-cl)"
