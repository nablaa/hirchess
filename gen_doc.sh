#! /bin/bash
haddock -o doc -h *.hs
hlint . --report
