       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2023-12-01.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE1 ASSIGN
            TO "C:\Users\310344706\Documents\AOC20231201.input.txt"
           FILE STATUS IS FILE1-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE1.
         01 INPUTRECORD1 PIC X(200).
       WORKING-STORAGE SECTION.

      * FILE STATUS
       01 FILE1-STATUS PIC 9(02).
       88 FILE1-STATUS-OK  VALUE 00.
       88 FILE1-STATUS-EOF VALUE 10.

      * WORKING VARIABLE
       01 ETALONNAGE PIC 9(2).
       01 ETALONNAGE-FINAL PIC 9(10).
       01 I PIC 9(3).
       01 CAR-REC PIC X(1).
       01 PRE-CHI PIC S9(1).
       01 DER-CHI PIC S9(1).
       
       01 one pic x(03) value 'one'.
       01 two pic x(03) value 'two'.
       01 three pic x(05) value 'three'.
       01 four pic x(04) value 'four'.
       01 five pic x(05) value 'five'.
       01 six pic x(03) value 'six'.
       01 seven pic x(05) value 'seven'.
       01 eight pic x(05) value 'eight'.
       01 nine pic x(04) value 'nine'.

       PROCEDURE DIVISION.


           display 'ETALONNAGE'
           PERFORM MAIN-PROCESSING
           STOP RUN
           .

       MAIN-PROCESSING.
           PERFORM OPEN-FILE1

           PERFORM READ-FILE1

           PERFORM UNTIL FILE1-STATUS-EOF
           OR NOT FILE1-STATUS-OK
           
                       display INPUTRECORD1

                     PERFORM VARYING I FROM 1 BY 1 UNTIL
                   (I > 195)                  

                       MOVE INPUTRECORD1(I:1) TO CAR-REC

                       IF CAR-REC IS NUMERIC
                         IF PRE-CHI < 1
                             MOVE CAR-REC TO PRE-CHI
                         ELSE
                           MOVE CAR-REC TO DER-CHI
                           
                         END-IF
                       else
                         IF PRE-CHI < 1
                           evaluate TRUE
                           when INPUTRECORD1(I:3) = one
                               move 1 to PRE-CHI
                           when INPUTRECORD1(I:3) = two
                               move 2 to PRE-CHI
                           when INPUTRECORD1(I:5) = three
                               move 3 to PRE-CHI
                           when INPUTRECORD1(I:4) = four
                               move 4 to PRE-CHI
                           when INPUTRECORD1(I:4) = five
                               move 5 to PRE-CHI
                           when INPUTRECORD1(I:3) = six
                               move 6 to PRE-CHI
                           when INPUTRECORD1(I:5) = seven
                               move 7 to PRE-CHI
                           when INPUTRECORD1(I:5) = eight
                               move 8 to PRE-CHI
                           when INPUTRECORD1(I:4) = nine
                               move 9 to PRE-CHI
                            when other 
                               CONTINUE
                           END-EVALUATE
                          ELSE
                              
                           evaluate TRUE
                           when INPUTRECORD1(I:3) = one
                               move 1 to DER-CHI
                           when INPUTRECORD1(I:3) = two
                               move 2 to DER-CHI
                           when INPUTRECORD1(I:5) = three
                               move 3 to DER-CHI
                           when INPUTRECORD1(I:4) = four
                               move 4 to DER-CHI
                           when INPUTRECORD1(I:4) = five
                               move 5 to DER-CHI
                           when INPUTRECORD1(I:3) = six
                               move 6 to DER-CHI
                           when INPUTRECORD1(I:5) = seven
                               move 7 to DER-CHI
                           when INPUTRECORD1(I:5) = eight
                               move 8 to DER-CHI
                           when INPUTRECORD1(I:4) = nine
                               move 9 to DER-CHI
                            when other 
                               CONTINUE
                           END-EVALUATE
                         END-IF
                           
                       END-IF
                     END-PERFORM
                     
                     IF DER-CHI = 0 
                         move PRE-CHI to DER-CHI
                     end-if
                     
                       DISPLAY PRE-CHI
                       DISPLAY DER-CHI
                           COMPUTE ETALONNAGE = (PRE-CHI * 10) + DER-CHI
                       DISPLAY ETALONNAGE
                     
                           ADD ETALONNAGE TO ETALONNAGE-FINAL
                           
                           MOVE 0 TO PRE-CHI DER-CHI

                PERFORM READ-FILE1

           END-PERFORM

           PERFORM CLOSE-FILE1

           display 'ETALONNAGE :' ETALONNAGE-FINAL
           .

       OPEN-FILE1.
           OPEN INPUT INPUTFILE1
           IF NOT FILE1-STATUS-OK
               DISPLAY 'OPEN ERROR ON FILE 1'
               DISPLAY 'FILE STATUS : ' FILE1-STATUS
               STOP RUN
           END-IF
           .

       READ-FILE1.
           READ INPUTFILE1
               AT END
                   CONTINUE
                NOT AT END
                   IF NOT FILE1-STATUS-OK
                       DISPLAY 'READ ERROR ON FILE 1'
                       DISPLAY 'FILE STATUS : ' FILE1-STATUS
                       STOP RUN
                   END-IF
           END-READ
           .

       CLOSE-FILE1.
           CLOSE INPUTFILE1
           IF NOT FILE1-STATUS-OK
               DISPLAY 'CLOSE ERROR ON FILE 1'
               DISPLAY 'FILE STATUS : ' FILE1-STATUS
               STOP RUN
           END-IF
           .
