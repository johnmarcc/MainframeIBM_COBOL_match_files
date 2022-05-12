      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    JMCCOB02.
       AUTHOR.        JEAN M C.
      *>  - EXAMPLE OF FILE MATCHING BETWEEN 2 FILES
      *>  - BOTH FILES MUST BE SORTED BY KEY (ACCOUNT NUMBER) IN THE JCL
      *>    BEFORE THIS PROG
      *>  - FILE "ACCT IN" IS THE MASTER FILE
      *>  - KEYS IN "MOVEMENTS" ARE NOT UNIQUE [YES CAN BE DUPLICATES]
      *>  - THIS PROGRAM CAN CHECK IF KEYS ARE MISSING
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       CONFIGURATION SECTION.
       OBJECT-COMPUTER.
       SOURCE-COMPUTER.
      D                IBM-370 WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-IN ASSIGN TO ACCTIN
            ORGANIZATION IS  SEQUENTIAL
            FILE STATUS IS WS-F01-FS.

           SELECT MOV-REC  ASSIGN TO MOVREC
            ORGANIZATION IS  SEQUENTIAL
            FILE STATUS IS WS-F02-FS.

           SELECT ACCT-OUT ASSIGN TO ACCTOUT
            ORGANIZATION IS  SEQUENTIAL
            FILE STATUS IS WS-F03-FS.

      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.

       FD  ACCT-IN RECORDING MODE F.
       01  ACCT-IN-DATA                 PIC X(80).

       FD  MOV-REC RECORDING MODE F.
       01  MOUV-FIELDS                  PIC X(80).

       FD  ACCT-OUT RECORDING MODE F.
       01  ACCT-OUT-DATA                 PIC X(80).

      *-------------------------
       WORKING-STORAGE SECTION.
      *-------------------------
       01 WS-COUNTERS.
           05 WS-NO-READ-F01               PIC 9(8).
           05 WS-NO-READ-F02               PIC 9(8).
           05 WS-NO-RECORD-MATCH           PIC 9(8).
           05 WS-NO-WRITE-REC              PIC 9(8).

       01 WS-FILE-STATUS.
           05 WS-F01-FS                 PIC X(2).
           05 WS-F02-FS                 PIC X(2).
           05 WS-F03-FS                 PIC X(2).

       01 WS-F01-END-OF-FILE           PIC X(5) VALUE 'FALSE'.
          88 WS-F01-EOF                VALUE 'TRUE'.
          88 WS-F01-NOT-EOF            VALUE 'FALSE'.

       01 WS-F02-END-OF-FILE           PIC X(5) VALUE 'FALSE'.
           88 WS-F02-EOF                VALUE 'TRUE'.
           88 WS-F02-NOT-EOF            VALUE 'FALSE'.

      * THIS FILE REPRESENTS THE BANK ACCOUNTS LIST WE WANT TO UPDATE
      * THEIR BALANCE
       01 WS-REC-F01.
           05  F01-ACCT-NO               PIC X(07).
           05  F01-CUSTOMER-NAME         PIC X(20).
           05  F01-BALANCE               PIC 9(3).

      * THIS FILE REPRESENTS THE MVT FILE WHICH CONTAINS TRANSACTIONS
      * TO UPDATE / MANY TRANSACTIONS CAN BE PRESENT IN THIS FILE
      * FOR THE SAME BANK ACCOUNT
       01 WS-REC-F02.
         05  F02-MVT-ACCT-NO             PIC X(07).
         05  F02-MVT                     PIC 9(3).

       01  WS-DISPLAY-BALANCE            PIC  $ZZ,ZZ9.99 VALUE ZERO.

       01  WS-SAVE-ACCT-NO               PIC X(07).
       01  WS-SAVE-MVT-ACCT-NO           PIC X(07).
       01  WS-TOTAL-SUM                  PIC 9(3) VALUE ZERO.
.
       01   WS-USER-ABEND-CODE      PIC S9(04)   COMP.

       01  WS-CURRENT-DATE.
           05 WS-CC  PIC 9(2).
           05 WS-YY  PIC 9(2).
           05 WS-MM  PIC 9(2).
           05 WS-DD  PIC 9(2).
           05 WS-HH  PIC 9(2).
           05 WS-MI  PIC 9(2).
           05 WS-SS  PIC 9(2).

      *------------------
       PROCEDURE DIVISION.
      *------------------

           PERFORM 1000-INIT
              THRU 1000-INIT-END.

           PERFORM 2000-MAIN-PROCESS
              THRU 2000-MAIN-PROCESS-END
               UNTIL WS-F01-EOF
                 AND WS-F02-EOF

           PERFORM 9000-END-PROCESS
              THRU 9000-END-PROCESS-END.

           STOP RUN.

      *-------------------
       1000-INIT.
      *-------------------

           DISPLAY "***** INIT PROCESS *****".

           MOVE SPACE TO WS-FILE-STATUS.
           MOVE ZEROES TO WS-COUNTERS.

           OPEN INPUT  ACCT-IN.

           IF WS-F01-FS NOT = "00"

      D      DISPLAY "ERROR OPEN FILE ACCT-IN: " WS-F01-FS

             PERFORM 9999-ABEND
                THRU 9999-ABEND-END

           END-IF.

           OPEN INPUT  MOV-REC.

           IF WS-F02-FS NOT = "00"

      D      DISPLAY "ERROR OPEN FILE MOV-REC: " WS-F02-FS

             PERFORM 9999-ABEND
                THRU 9999-ABEND-END

           END-IF.

           OPEN OUTPUT ACCT-OUT.

           IF WS-F03-FS NOT = "00"

      D      DISPLAY "ERROR OPEN FILE ACCT-OUT: " WS-F03-FS

             PERFORM 9999-ABEND
                 THRU 9999-ABEND-END

           END-IF.

      *    INITIAL READ OF EACH INPUT FILE

           PERFORM 8000-READ-ACCT-IN
              THRU 8000-READ-ACCT-IN-END.
      *    FIRST ACCOUNT NUMBER BEING TREATED
           MOVE F01-ACCT-NO TO WS-SAVE-ACCT-NO.

           PERFORM 8000-READ-MVT
              THRU 8000-READ-MVT-END.
      *    FIRST MVT FROM THE FILE
           MOVE F02-MVT-ACCT-NO TO WS-SAVE-MVT-ACCT-NO.

           INITIALIZE WS-TOTAL-SUM.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.               TE.

      *-------------------
       1000-INIT-END.
      *-------------------
           EXIT.

      *-------------------
       2000-MAIN-PROCESS.
      *-------------------

           EVALUATE TRUE

           WHEN  F01-ACCT-NO < F02-MVT-ACCT-NO

      D       DISPLAY "<<<MASTER LOWER MVT: " F02-MVT-ACCT-NO

               PERFORM UNTIL F01-ACCT-NO >= WS-SAVE-MVT-ACCT-NO
                          OR WS-F01-EOF

                   PERFORM 8000-WRITE-F03
                      THRU 8000-WRITE-F03-END

      *            IN THIS CASE THE MASTER FILE KEY IS LOWER SO SE HAVE TO
      *            READ TO FILE TO BE AT SAME LEVEL OF MVT FILE
                   PERFORM 8000-READ-ACCT-IN
                      THRU 8000-READ-ACCT-IN-END

               END-PERFORM

           WHEN  F01-ACCT-NO = F02-MVT-ACCT-NO

      D         DISPLAY "=== MASTER = MVT: " F02-MVT-ACCT-NO

      *         IN THIS CASE WE HAVE TO READ THE MVT FILE SEVERAL TIMES
      *         BECAUSE THERE MIGHT BE SEVERAL MVTS FOR THE SAME ACCOUNT
                PERFORM 3000-CALCUL-SUM
                   THRU 3000-CALCUL-SUM-END
                    UNTIL F02-MVT-ACCT-NO > WS-SAVE-MVT-ACCT-NO
                       OR WS-F02-EOF

                COMPUTE F01-BALANCE = F01-BALANCE + WS-TOTAL-SUM

               PERFORM 8000-WRITE-F03
                  THRU 8000-WRITE-F03-END

               INITIALIZE WS-TOTAL-SUM

               MOVE F02-MVT-ACCT-NO TO WS-SAVE-MVT-ACCT-NO

                 PERFORM 8000-READ-ACCT-IN
                    THRU 8000-READ-ACCT-IN-END

           WHEN OTHER

      *       WE ARE IN THE CASE: F01-ACCT-NO > F02-MVT-ACCT-NO

      D       DISPLAY ">>> MASTER GREATER MVT " F02-MVT-ACCT-NO

      *        IN THIS CASE WE HAVE TO READ THE MVT FILE SEVERAL TIMES
      *        BECAUSE IT CONTAINS KEY THAT ARE NOT PRESENT IN THE MASTER
      *        WHICH IS AN ERROR
               PERFORM UNTIL F02-MVT-ACCT-NO >= F01-ACCT-NO
                   OR WS-F02-EOF

      D            DISPLAY "__MVT NOT FOUND IN MASTER: " F02-MVT-ACCT-NO

                    PERFORM 8000-READ-MVT
                       THRU 8000-READ-MVT-END

                    MOVE F02-MVT-ACCT-NO TO WS-SAVE-MVT-ACCT-NO

                END-PERFORM

           END-EVALUATE.

      *-------------------
       2000-MAIN-PROCESS-END.
      *-------------------
           EXIT.

      *-------------------
       3000-CALCUL-SUM.
      *-------------------

           COMPUTE WS-TOTAL-SUM = WS-TOTAL-SUM + F02-MVT.

           ADD 1 TO WS-NO-RECORD-MATCH.

           PERFORM 8000-READ-MVT
               THRU 8000-READ-MVT-END.

      *-------------------
       3000-CALCUL-SUM-END.
      *-------------------
           EXIT.
      *-------------------
       8000-READ-ACCT-IN.
      *-------------------

           INITIALIZE WS-REC-F01.

           READ ACCT-IN INTO WS-REC-F01

           END-READ.

           EVALUATE TRUE

             WHEN WS-F01-FS = '00'
      D        DISPLAY "F01-ACCT-NO " F01-ACCT-NO
               ADD 1 TO WS-NO-READ-F01

               CONTINUE

             WHEN WS-F01-FS = '10'
               SET WS-F01-EOF TO TRUE
               MOVE HIGH-VALUE TO WS-SAVE-ACCT-NO
      D        DISPLAY "WS-F01-END-OF-FILE " WS-F01-END-OF-FILE

             WHEN OTHER
      D        DISPLAY "ERROR READ FILE F01 !!!: " WS-F01-FS

               PERFORM 9999-ABEND
                   THRU 9999-ABEND-END

           END-EVALUATE.

           MOVE F01-ACCT-NO TO WS-SAVE-ACCT-NO.

      *-------------------
       8000-READ-ACCT-IN-END.
      *-------------------
           EXIT.
      *-------------------
       8000-READ-MVT.
      *-------------------

           INITIALIZE WS-REC-F02.

           READ MOV-REC INTO WS-REC-F02

           END-READ.

           EVALUATE TRUE

             WHEN WS-F02-FS = '00'
      D          DISPLAY "F02-MVT-ACCT-NO " F02-MVT-ACCT-NO
                 ADD 1 TO WS-NO-READ-F02

             WHEN WS-F02-FS = '10'
               SET WS-F02-EOF TO TRUE

               MOVE HIGH-VALUE TO WS-SAVE-MVT-ACCT-NO

      D        DISPLAY "WS-F02-END-OF-FILE " WS-F02-END-OF-FILE
               MOVE HIGH-VALUE TO F02-MVT-ACCT-NO

             WHEN OTHER
      D        DISPLAY "ERROR READ FILE F02 !!!: " WS-F02-FS
               PERFORM 9999-ABEND
                   THRU 9999-ABEND-END

           END-EVALUATE.

      *-------------------
       8000-READ-MVT-END.
      *-------------------
           EXIT.

      *-------------------
       8000-WRITE-F03.
      *-------------------

           INITIALIZE ACCT-OUT-DATA.
      D    DISPLAY "WRITE WS-REC-F01 " WS-REC-F01.
           WRITE ACCT-OUT-DATA FROM WS-REC-F01.

           EVALUATE TRUE

             WHEN WS-F03-FS = '00'
                 COMPUTE WS-NO-WRITE-REC = WS-NO-WRITE-REC + 1

             WHEN OTHER
      D        DISPLAY "ERROR WRITE FILE F03 !!!: " WS-F03-FS
               PERFORM 9999-ABEND
                   THRU 9999-ABEND-END

           END-EVALUATE.

      *-------------------
       8000-WRITE-F03-END.
      *-------------------
           EXIT.

      *-------------------
       9000-END-PROCESS.
      *-------------------

           DISPLAY "***** END PROCESS *****".

           DISPLAY '*************************************************'.
           DISPLAY "PROCESS DATE: " WS-CURRENT-DATE(5:2) "/"
                    WS-CURRENT-DATE(7:2) "/" WS-CURRENT-DATE(1:4) "-"
                    WS-CURRENT-DATE(9:2) ":" WS-CURRENT-DATE(11:2)
                    ":" WS-CURRENT-DATE(13:2)
           DISPLAY '*************************************************'.
           DISPLAY "NO REC READ F01 : " WS-NO-READ-F01.
           DISPLAY "NO REC READ F02 : " WS-NO-READ-F02.
           DISPLAY "NO REC MATCH    : " WS-NO-RECORD-MATCH.
           DISPLAY "NO REC WRIT F03 : " WS-NO-WRITE-REC.
           DISPLAY '*************************************************'.

           CLOSE ACCT-IN.
           CLOSE MOV-REC.
           CLOSE ACCT-OUT.

      *-------------------
       9000-END-PROCESS-END.
      *-------------------
           EXIT.
      *-------------------
       9999-ABEND.
      *-------------------

      D    DISPLAY "WE ARE IN ABEND".

           PERFORM 9000-END-PROCESS
              THRU 9000-END-PROCESS-END.

      *    WE FORCE AN ABEND
      *>      MOVE +40                TO WS-USER-ABEND-CODE
      *>      CALL 'ILBOABN0'      USING WS-USER-ABEND-CODE

           GOBACK.

      *-------------------
       9999-ABEND-END.
      *-------------------
           EXIT.
