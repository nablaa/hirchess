#!/bin/bash
ghc --make TestBoard.hs
./TestBoard
hpc report TestBoard --exclude=Main --exclude=QC
hpc markup TestBoard --exclude=Main --exclude=QC

