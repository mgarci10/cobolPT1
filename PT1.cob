       IDENTIFICATION DIVISION.
       PROGRAM-ID. PT1.
       AUTHOR. MARIO GARCIA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE   ASSIGN TO 'COB1-EMPLOYEE'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNT-FILE    ASSIGN TO 'UR-S-PRNT'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  INPUT-FILE.
       01 INPUT-REC.
        88 EOF VALUE HIGH-VALUES.
        02 EMP-ID PIC 9(7).
        02 EMP-NAME.
         03 EMP-L-NAME PIC X(15).
         03 EMP-F-NAME PIC X(15).
        02 EMP-TYPE PIC 99.
        02 EMP-TITLE PIC X(17).
        02 EMP-SSN.
         03 EMP-SSN-1 PIC 9(3).
         03 EMP-SSN-2 PIC 9(2).
         03 EMP-SSN-3 PIC 9(4).
        02 FILLER PIC X(24) VALUE SPACES.
        02 EMP-DATE.
         03 EMP-DATE-M PIC 9(2).
         03 EMP-DATE-D PIC 9(2).
         03 EMP-DATE-Y PIC 9(4).

       FD PRNT-FILE.
       01 PRNT-REC PIC X(150).

       WORKING-STORAGE SECTION.
      * Header hard-coded to be applied to the first line
       01 PRNT-HEADING.
        02 FILLER PIC X(2) VALUE SPACES.
        02 FILLER PIC X(3) VALUE "SSN".
        02 FILLER PIC X(11) VALUE SPACES.
        02 FILLER PIC X(6) VALUE "EMP ID".
        02 FILLER PIC X(4) VALUE SPACES.
        02 FILLER PIC X(4) VALUES "LAST".
        02 FILLER PIC X(22) VALUE SPACES.
        02 FILLER PIC X(5) VALUE "FIRST".
        02 FILLER PIC X(21) VALUE SPACES.
        02 FILLER PIC X(6) VALUE "TITLE".
        02 FILLER PIC X(17) VALUE SPACES.
        02 FILLER PIC X(4) VALUE "TYPE".
        02 FILLER PIC X(4) VALUE SPACES.
        02 FILLER PIC X(4) VALUE "DATE".

      * Template/layout for the values that shall
      * be moved  over from each record
       01 PRNT-DATA.
        02 FILLER PIC X(2) VALUE SPACES.
        02 PRN-EMP-SSN.
         03 PRN-EMP-SSN-1 PIC 9(3).
         03 FILLER PIC X VALUE "-".
         03 PRN-EMP-SSN-2 PIC 9(2).
         03 FILLER PIC X VALUE "-".
         03 PRN-EMP-SSN-3 PIC 9(4).
        02 FILLER PIC X(3) VALUE SPACES.
        02 PRN-EMP-ID PIC 9(7).
        02 FILLER PIC X(3) VALUE SPACES.
        02 PRN-EMP-NAME.
         03 PRN-EMP-L-NAME PIC X(15).
         03 FILLER PIC X(11) VALUE SPACES.
         03 PRN-EMP-F-NAME PIC X(15).
        02 FILLER PIC X(11) VALUE SPACES.
        02 PRN-EMP-TITLE PIC X(17).
        02 FILLER PIC X(6) VALUE SPACES.
        02 PRN-EMP-TYPE PIC 99.
        02 FILLER PIC X(6) VALUE SPACES.
        02 PRN-EMP-DATE.
         03 PRN-EMP-DATE-M PIC 9(2).
         03 FILLER PIC X VALUE "/".
         03 PRN-EMP-DATE-D PIC 9(2).
         03 FILLER PIC X VALUE "/".
         03 PRN-EMP-DATE-Y PIC 9(4).

       PROCEDURE DIVISION.
       MAIN.
       OPEN INPUT INPUT-FILE
       OUTPUT PRNT-FILE.

        READ INPUT-FILE INTO INPUT-REC
               AT END SET EOF TO TRUE
               END-READ
        PERFORM PRINT-HEADING

       PERFORM UNTIL EOF
          PERFORM PRINT-RESULTS
          READ INPUT-FILE INTO INPUT-REC
            AT END SET EOF TO TRUE
          END-READ
       END-PERFORM


        CLOSE INPUT-FILE, PRNT-FILE
        STOP RUN.

      * Write headers directly to file through the file buffer line
      * then adds spaces to the next line
        PRINT-HEADING.
          WRITE PRNT-REC FROM PRNT-HEADING
           AFTER ADVANCING PAGE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC AFTER ADVANCING 1 LINE.

      * Moves values from current record buffer to new layout
      * and writes data to file buffer line
        PRINT-RESULTS.
          MOVE EMP-SSN-1 TO PRN-EMP-SSN-1
          MOVE EMP-SSN-2 TO PRN-EMP-SSN-2
          MOVE EMP-SSN-3 TO PRN-EMP-SSN-3
          MOVE EMP-ID TO PRN-EMP-ID
          MOVE EMP-L-NAME TO PRN-EMP-L-NAME
          MOVE EMP-F-NAME TO PRN-EMP-F-NAME
          MOVE EMP-TITLE TO PRN-EMP-TITLE
          MOVE EMP-TYPE TO PRN-EMP-TYPE
          MOVE EMP-DATE-M TO PRN-EMP-DATE-M
          MOVE EMP-DATE-D TO PRN-EMP-DATE-D
          MOVE EMP-DATE-Y TO PRN-EMP-DATE-Y
          WRITE PRNT-REC FROM PRNT-DATA
          AFTER ADVANCING 2 LINES.
