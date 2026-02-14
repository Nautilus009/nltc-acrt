*******************************************************************************
IDENTIFICATION DIVISION.
*******************************************************************************
PROGRAM-ID.    DEMO_ICA_CHK_RMS_V24.
AUTHOR.        TP_ONISSA.
DATE-WRITTEN.  08-Mar-2016.
 
* In TEST we saw a failures (in TRB212 and SVJ604) where SYS_CHK_FILE_AVAIL_SPACE returned status 20 on a write operation and caused file-status 20,
*  even though there was no real disk-full situation.
 
* The root cause was that, during a WRITE operation, CALextfh was passing the wrong (stale) filename into SYS_CHK_FILE_AVAIL_SPACE,
*  so the disk check was being done on an unrelated path instead of the current application file.
 
*******************************************************************************
ENVIRONMENT DIVISION.
*******************************************************************************
 
*--------------------------------------------------------------------
INPUT-OUTPUT                            SECTION.
*--------------------------------------------------------------------
FILE-CONTROL.
 
*   Output RMS file
    SELECT P_RMS_FILE
           ASSIGN                 TO           "SYSzDISK"
           ORGANIZATION           IS           SEQUENTIAL
           ACCESS                 IS           SEQUENTIAL
           FILE STATUS            IS           SPz_COBRMS_FILE_STATUS.
 
*   Output RMS file
    SELECT P_RMS_SORT_FILE
           ASSIGN                 TO           "SYSzDISK"
           ORGANIZATION           IS           SEQUENTIAL
           ACCESS                 IS           SEQUENTIAL
           FILE STATUS            IS           SPz_COBRMS_FILE_STATUS.
 
    SELECT S_RMS_FILE   ASSIGN TO "SORTFILE".
 
*******************************************************************************
DATA DIVISION.
*******************************************************************************
*------------------------------------------------------------------------------
FILE SECTION.
*------------------------------------------------------------------------------
FD P_RMS_FILE
    VALUE OF ID     IS Lz_P_RMS_FILE_NAME.
    01  P_RMS_FILE_REC         PIC X(20).
 
FD P_RMS_SORT_FILE
    VALUE OF ID     IS Lz_P_RMS_SORT_FILE_NAME.
    01  P_RMS_SORT_FILE_REC    PIC X(20).
 
SD S_RMS_FILE.
    01  S_RMS_FILE_REC         PIC X(20).
 
*------------------------------------------------------------------------------
WORKING-STORAGE SECTION.
*------------------------------------------------------------------------------
01 Lz_FILE_NAMES.
    03 Lz_P_RMS_FILE_NAME            PIC X(70).
    03 Lz_P_RMS_SORT_FILE_NAME       PIC X(70).
 
01 Lz_CT01_COUNTERS.
    03 Lz_CT01_REC_WRITTEN           PIC 9(09)       VALUE 0.
 
* DBA copies.
**************
 
* ICA copies.
******************
COPY "ICA_CDD_WKSP:ICA_RMS_MSG_WKSP"                              .
* UTL copies.
***************
COPY "UTL_CDD_WKSP:UTL_EXIT_ROUTINE_WKSP"                              .
COPY "UTL_SOURCE:UTL_MESSAGE.INC".
COPY "UTL_CDD_WKSP:UTL_COBRMS_VALUE_WKSP"                        .
COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"                           .
 
* ICD copies.
***************
 
* INC files.
*****************
 
*******************************************************************************
PROCEDURE DIVISION.
*******************************************************************************
DECLARATIVES.
*--------------------------------------------------------------------
001-I-O-PROBLEM                                    SECTION.
*--------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_RMS_FILE.
001.
****
    IF NOT SPz_COBRMS_SUCCESS IN SPz_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " Lz_P_RMS_FILE_NAME
       DISPLAY "** SPz_COBRMS_FILE_STATUS: " SPz_COBRMS_FILE_STATUS
 
       SET SPz_EXIT_STATUS_FAILURE TO TRUE
 
       MOVE "Sec:DECLARATIVES 001-I-O-PROBLEM"  TO SPz_ACW_FREE_TEXT
 
*# 13-NOV-2012 18:57:01.85 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       EXIT PROGRAM
    END-IF
    .
*--------------------------------------------------------------------
002-I-O-PROBLEM                                    SECTION.
*--------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_RMS_SORT_FILE.
002.
****
    IF NOT SPz_COBRMS_SUCCESS IN SPz_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " Lz_P_RMS_SORT_FILE_NAME
       DISPLAY "** SPz_COBRMS_FILE_STATUS: " SPz_COBRMS_FILE_STATUS
 
       SET SPz_EXIT_STATUS_FAILURE TO TRUE
 
       MOVE SPz_MSG_ERROR                       TO SPz_ACW_PROC_AUX_STATUS
       MOVE "Sec:DECLARATIVES 002-I-O-PROBLEM"  TO SPz_ACW_FREE_TEXT
 
*# 13-NOV-2012 18:57:01.85 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       EXIT PROGRAM
    END-IF
    .
END DECLARATIVES.
*----------------------------------------------------------------------
A-MAIN         SECTION.
*----------------------------------------------------------------------
A-00.
 
    PERFORM B-INIT
 
    PERFORM C-PROCESS
 
    PERFORM Z-FINISH
    .
A-EXIT.     EXIT.
*----------------------------------------------------------------------
B-INIT         SECTION.
*----------------------------------------------------------------------
B-00.
 
    INITIALIZE Lz_CT01_COUNTERS
 
    DISPLAY FUNCTION CURRENT-DATE
    .
B-EXIT.     EXIT.
*----------------------------------------------------------------------
C-PROCESS              SECTION.
*----------------------------------------------------------------------
C-00.
 
*   Open files
    DISPLAY "----------------------------------------------------"
 
    MOVE "ICA_PR_DAT_DIR:DEMO_ICA_CHK_RMS_V24.DAT"
      TO Lz_P_RMS_FILE_NAME
 
    OPEN OUTPUT P_RMS_FILE
    DISPLAY "Open Output File: " Lz_P_RMS_FILE_NAME
 
    MOVE "ICA_DATA:DEMO_ICA_CHK_RMS_V24.DAT"
      TO Lz_P_RMS_SORT_FILE_NAME
 
    OPEN OUTPUT P_RMS_SORT_FILE
    DISPLAY "Open Output File: " Lz_P_RMS_SORT_FILE_NAME
    .
C-01.
 
*   Write to files
    DISPLAY "----------------------------------------------------"
 
    CLOSE P_RMS_SORT_FILE
    DISPLAY "Close Output File: " Lz_P_RMS_SORT_FILE_NAME
 
    PERFORM CB-SORT-OUTPUT-FILE
 
    MOVE "91234567890123456789"
      TO P_RMS_FILE_REC
 
    WRITE P_RMS_FILE_REC
    DISPLAY "Record written to File: " Lz_P_RMS_FILE_NAME
 
    ADD 1
     TO Lz_CT01_REC_WRITTEN
   .
C-02.
 
*   Close files
    DISPLAY "----------------------------------------------------"
 
    CLOSE P_RMS_FILE
    DISPLAY "Close Output File: " Lz_P_RMS_FILE_NAME
    .
C-EXIT.     EXIT.
*----------------------------------------------------------------------
CB-SORT-OUTPUT-FILE              SECTION.
*----------------------------------------------------------------------
CB-00.
 
    DISPLAY "----------------------------------------------------"
 
    DISPLAY "Sort input file ", Lz_P_RMS_SORT_FILE_NAME," Giving output file ",Lz_P_RMS_SORT_FILE_NAME
 
    SORT S_RMS_FILE ON ASCENDING KEY
            S_RMS_FILE_REC
       USING  P_RMS_SORT_FILE
       GIVING P_RMS_SORT_FILE
    .
CB-EXIT.     EXIT.
*----------------------------------------------------------------------
X-ERRORS               SECTION.
*----------------------------------------------------------------------
X-00.
 
    DISPLAY "======================================================="
    DISPLAY "Attention!!! Program ended with error"
    DISPLAY "======================================================="
 
    CALL 'UTL_EXIT_ROUTINE' USING BY REFERENCE UTL_EXIT_ROUTINE_WKSP
 
    STOP RUN
    .
X-EXIT.     EXIT.
*----------------------------------------------------------------------
Z-FINISH               SECTION.
*----------------------------------------------------------------------
Z-00.
 
    DISPLAY FUNCTION CURRENT-DATE
 
    DISPLAY "======================================================="
    DISPLAY "Program ended successfully"
    DISPLAY "======================================================="
 
    CALL 'UTL_EXIT_ROUTINE' USING BY REFERENCE UTL_EXIT_ROUTINE_WKSP
 
    STOP RUN
    .
Z-EXIT.     EXIT.