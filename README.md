# Project Title
MainframeIBM_COBOL_MATCH_FILES

## Main Functionality

Example of a COBOL program to perform a file matching between 2 files. 

## Input data

- file ACCT-IN is a list of bank accouts and their balance. It is the master file.
- file MOVREC contains a list of movements on these bank accounts

## Process

For each movement found in the mvt input file, the matching bank account balance is updated 

Rules:
- not all the bank accounts have movements
- some movements have no related bank accounts: in this case we list an error for these movements

## Output data
- file ACCT-OUT is the input updated bank accounts list with updated balance

## Test
See JCL and input files 

## Author
https://github.com/johnmarcc


