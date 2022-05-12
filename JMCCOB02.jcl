//TSOBA17A JOB 0000,'COBJMC01',MSGCLASS=Q,MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,CLASS=A,REGION=0M
//*******************************************************************
//*                                                                 *
//*******************************************************************
//COMPILE EXEC IGYWCL,PARM=(OFFSET,NOLIST,ADV),
//     PGMLIB='&&GOSET',GOPGM=COBJMC01
//COBOL.SYSIN  DD  DSN=TSOBA17.COBOL.LABS(COBJMC01),DISP=SHR
//COBOL.SYSLIB DD  DSN=TSOBA17.ONLINE.LOADLIB,DISP=SHR
//*
//STEP1   EXEC PGM=COBJMC01
//STEPLIB  DD DSN=*.COMPILE.LKED.SYSLMOD,DISP=(OLD,PASS)
//ACCTIN   DD DSN=TSOBA17.FILE.ACCTIN,DISP=SHR
//MOVREC   DD DSN=TSOBA17.FILE.MOVREC,DISP=SHR
//ACCTOUT  DD SYSOUT=*
//SYSOUT  DD SYSOUT=*
//*
