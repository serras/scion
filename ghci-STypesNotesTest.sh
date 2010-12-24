#!/bin/sh

cd lib
ghc --interactive -package ghc -cpp STypesNotesTest.hs
