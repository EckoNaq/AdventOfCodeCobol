       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2023-12-01.
       AUTHOR. AURELIO FLORE.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE1 ASSIGN 
            TO "C:\Users\310344706\Documents\
      -        "AOC20231201.input.txt"
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
	  
		01 ETALONNAGE PIC 99.
		01 ETALONNAGE-FINAL PIC 999.
		01 INDEX PIC 99.
		01 CAR-REC PIC X.
		01 PRE-CHI PIC 9.
		01 DER-CHI PIC 9.

       PROCEDURE DIVISION.
       
           PERFORM MAIN-PROCESSING
           STOP RUN
           .
       
       MAIN-PROCESSING.
           PERFORM OPEN-FILE1
           
           PERFORM READ-FILE1
           
           PERFORM UNTIL FILE1-STATUS-EOF 
           OR NOT FILE1-STATUS-OK
				  
				PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > LENGTH OF INPUTRECORD1
				
					MOVE INPUTRECORD1(INDEX:1) TO CAR-REC
					IF CAR-REC IS NUMERIC
						IF PRE-CHI NOT NUMERIC
							MOVE CAR-REC TO PRE-CHI
						ELSE
							MOVE CAR-REC TO DER-CHI
							COMPUTE ETALONNAGE = (PRE-CHI * 10) + DER-CHI
							ADD ETALONNAGE TO ETALONNAGE-FINAL
							MOVE SPACES TO PRE-CHI 
							MOVE SPACES TO DER-CHI
						END-IF
					END-IF
				END-PERFORM
				  
                PERFORM READ-FILE1
              
           END-PERFORM
           
           PERFORM CLOSE-FILE1            
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
