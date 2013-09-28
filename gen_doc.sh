#! /bin/bash
haddock -o doc -h $(ls *.hs | grep -v Setup.hs)
hlint . --report
