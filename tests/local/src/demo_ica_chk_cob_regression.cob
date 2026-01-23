*****************************************************************
**************** PROGRAM_NAME: [DEMO_ICA_CHK_COB_REGRESSION.COB] **
*                                                               *
*                 COBOL-IT Regression Test Suite                 *
*      Computational tests (RHEL7 vs RHEL9 / gcc differences)     *
*****************************************************************

*******************************************************************************
IDENTIFICATION DIVISION.
*******************************************************************************
PROGRAM-ID.  REGTEST-COMP.
AUTHOR.      MIGRATION-TEST.

*******************************************************************************
ENVIRONMENT DIVISION.
*******************************************************************************
CONFIGURATION SECTION.
SOURCE-COMPUTER. RHEL-LINUX.
OBJECT-COMPUTER. RHEL-LINUX.

*******************************************************************************
DATA DIVISION.
*******************************************************************************
WORKING-STORAGE SECTION.

*------------------------------------------------------------------------------
* Test counters and identifiers
*------------------------------------------------------------------------------
01 WS-TEST-ID                PIC X(10).
01 WS-TEST-COUNT             PIC 9(5) VALUE 0.
01 WS-SEPARATOR              PIC X(70) VALUE ALL "=".

*------------------------------------------------------------------------------
* Numeric test variables - different storage types
*------------------------------------------------------------------------------
01 WS-DISPLAY-NUM-1          PIC 9(9)V99 VALUE 123456789.12.
01 WS-DISPLAY-NUM-2          PIC 9(9)V99 VALUE 987654321.98.
01 WS-DISPLAY-RESULT         PIC S9(10)V99.

01 WS-COMP-NUM-1             PIC 9(9)V99 COMP VALUE 123456789.12.
01 WS-COMP-NUM-2             PIC 9(9)V99 COMP VALUE 987654321.98.
01 WS-COMP-RESULT            PIC S9(10)V99 COMP.

01 WS-COMP3-NUM-1            PIC 9(9)V99 COMP-3 VALUE 123456789.12.
01 WS-COMP3-NUM-2            PIC 9(9)V99 COMP-3 VALUE 987654321.98.
01 WS-COMP3-RESULT           PIC S9(10)V99 COMP-3.

01 WS-FLOAT-1                COMP-1 VALUE 1.23456789E+08.
01 WS-FLOAT-2                COMP-1 VALUE 9.87654321E+08.
01 WS-FLOAT-RESULT           COMP-1.

01 WS-DOUBLE-1               COMP-2 VALUE 1.23456789012345E+08.
01 WS-DOUBLE-2               COMP-2 VALUE 9.87654321098765E+08.
01 WS-DOUBLE-RESULT          COMP-2.

*------------------------------------------------------------------------------
* Signed numeric fields
*------------------------------------------------------------------------------
01 WS-SIGNED-DISP            PIC S9(7)V99 VALUE -1234567.89.
01 WS-SIGNED-COMP            PIC S9(7)V99 COMP VALUE -1234567.89.
01 WS-SIGNED-COMP3           PIC S9(7)V99 COMP-3 VALUE -1234567.89.
01 WS-SIGNED-RESULT          PIC S9(8)V99.

*------------------------------------------------------------------------------
* Edge case values
*------------------------------------------------------------------------------
01 WS-MAX-VALUE              PIC 9(18) VALUE 999999999999999999.
01 WS-MIN-VALUE              PIC 9(18) VALUE 1.
01 WS-ZERO-VALUE             PIC 9(9)V99 VALUE 0.
01 WS-EDGE-RESULT            PIC 9(18).

*------------------------------------------------------------------------------
* String manipulation variables
*------------------------------------------------------------------------------
01 WS-STRING-1               PIC X(50) VALUE "Hello World".
01 WS-STRING-2               PIC X(50) VALUE "COBOL-IT Test".
01 WS-STRING-RESULT          PIC X(100).
01 WS-STRING-COUNT           PIC 99.
01 WS-STRING-PTR             PIC 99.

*------------------------------------------------------------------------------
* Date and time fields
*------------------------------------------------------------------------------
01 WS-CURRENT-DATE.
   05 WS-CURR-YEAR           PIC 9(4).
   05 WS-CURR-MONTH          PIC 99.
   05 WS-CURR-DAY            PIC 99.
01 WS-CURRENT-TIME.
   05 WS-CURR-HOUR           PIC 99.
   05 WS-CURR-MIN            PIC 99.
   05 WS-CURR-SEC            PIC 99.

*------------------------------------------------------------------------------
* Intrinsic function test variables
*------------------------------------------------------------------------------
01 WS-FUNC-INPUT             PIC S9(9)V99 VALUE -12345.67.
01 WS-FUNC-RESULT            PIC S9(9)V99.
01 WS-FUNC-RESULT-DISP       PIC -(9)9.99.

*------------------------------------------------------------------------------
* Table for subscript tests
*------------------------------------------------------------------------------
01 WS-TABLE-DATA.
   05 WS-TABLE-ENTRY OCCURS 10 TIMES
                     INDEXED BY WS-IDX.
      10 WS-ENTRY-NUM        PIC 9(5).
      10 WS-ENTRY-NAME       PIC X(20).

*------------------------------------------------------------------------------
* Counter for loops
*------------------------------------------------------------------------------
01 WS-COUNTER                PIC 9(5).
01 WS-LOOP-RESULT            PIC 9(10).

*------------------------------------------------------------------------------
* Division test variables
*------------------------------------------------------------------------------
01 WS-DIVIDEND               PIC 9(9)V99 VALUE 1000000.00.
01 WS-DIVISOR                PIC 9(5)V99 VALUE 3.00.
01 WS-QUOTIENT               PIC 9(9)V9(6).
01 WS-REMAINDER              PIC 9(5)V99.

*------------------------------------------------------------------------------
* Reference modification test
*------------------------------------------------------------------------------
01 WS-REF-STRING             PIC X(50) VALUE
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".
01 WS-REF-RESULT             PIC X(20).
01 WS-REF-START              PIC 99 VALUE 5.
01 WS-REF-LENGTH             PIC 99 VALUE 10.

*------------------------------------------------------------------------------
* Evaluate test variable
*------------------------------------------------------------------------------
01 WS-EVAL-CODE              PIC X(2).
01 WS-EVAL-RESULT            PIC X(30).

*------------------------------------------------------------------------------
* Call parameter test (kept for future extension)
*------------------------------------------------------------------------------
01 WS-CALL-PARAM-1           PIC 9(5) VALUE 100.
01 WS-CALL-PARAM-2           PIC 9(5) VALUE 200.
01 WS-CALL-RESULT            PIC 9(6).

*------------------------------------------------------------------------------
* Binary field alignment test
*------------------------------------------------------------------------------
01 WS-ALIGNMENT-TEST.
   05 WS-ALIGN-CHAR          PIC X.
   05 WS-ALIGN-COMP          PIC 9(4) COMP.
   05 WS-ALIGN-CHAR2         PIC X.
   05 WS-ALIGN-COMP2         PIC 9(9) COMP.

*------------------------------------------------------------------------------
* Overflow test variables
*------------------------------------------------------------------------------
01 WS-OVERFLOW-TEST          PIC 9(5) VALUE 99999.
01 WS-OVERFLOW-ADD           PIC 9(5) VALUE 1.
01 WS-OVERFLOW-RESULT        PIC 9(5).

*------------------------------------------------------------------------------
* Mixed sign test
*------------------------------------------------------------------------------
01 WS-POS-NUM                PIC 9(5) VALUE 12345.
01 WS-NEG-NUM                PIC S9(5) VALUE -6789.
01 WS-MIXED-RESULT           PIC S9(6).

*******************************************************************************
PROCEDURE DIVISION.
*******************************************************************************
*----------------------------------------------------------------------
A-MAIN                 SECTION.
*----------------------------------------------------------------------
A-00.

    PERFORM B-INIT

    PERFORM C-PROCESS

    PERFORM Z-FINISH
    .
A-EXIT. EXIT.

*----------------------------------------------------------------------
B-INIT                 SECTION.
*----------------------------------------------------------------------
B-00.

    DISPLAY WS-SEPARATOR
    DISPLAY "COBOL-IT REGRESSION TEST - COMPUTATIONAL"
    DISPLAY "Test execution started"
    DISPLAY WS-SEPARATOR
    DISPLAY " "
    .
B-EXIT. EXIT.

*----------------------------------------------------------------------
C-PROCESS              SECTION.
*----------------------------------------------------------------------
C-00.

    PERFORM T-SYSTEM-INFO

    PERFORM T-ARITHMETIC-DISPLAY
    PERFORM T-ARITHMETIC-COMP
    PERFORM T-ARITHMETIC-COMP3

    PERFORM T-FLOATING-POINT
    PERFORM T-SIGNED-ARITHMETIC
    PERFORM T-EDGE-CASES
    PERFORM T-DIVISION-REMAINDER

    PERFORM T-STRING-OPERATIONS
    PERFORM T-INTRINSIC-FUNCTIONS
    PERFORM T-DATE-TIME
    PERFORM T-TABLE-SUBSCRIPT
    PERFORM T-REFERENCE-MOD
    PERFORM T-EVALUATE
    PERFORM T-PERFORM-VARIATIONS
    PERFORM T-BINARY-ALIGNMENT
    PERFORM T-OVERFLOW-HANDLING
    PERFORM T-MIXED-SIGNS
    PERFORM T-TYPE-CONVERSIONS
    .
C-EXIT. EXIT.

*******************************************************************************
* TESTS
*******************************************************************************

*----------------------------------------------------------------------
T-SYSTEM-INFO           SECTION.
*----------------------------------------------------------------------
T-SYSTEM-INFO-00.

    MOVE "SYS-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] System Information Test"
    ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
    ACCEPT WS-CURRENT-TIME FROM TIME

    DISPLAY "  Current Date: " WS-CURR-YEAR "-" WS-CURR-MONTH "-" WS-CURR-DAY
    DISPLAY "  Current Time: " WS-CURR-HOUR ":" WS-CURR-MIN ":" WS-CURR-SEC
    DISPLAY " "
    .
T-SYSTEM-INFO-EXIT. EXIT.

*----------------------------------------------------------------------
T-ARITHMETIC-DISPLAY    SECTION.
*----------------------------------------------------------------------
T-ARITHMETIC-DISPLAY-00.

    MOVE "ART-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Display Format Arithmetic"
    DISPLAY "  Input 1: " WS-DISPLAY-NUM-1
    DISPLAY "  Input 2: " WS-DISPLAY-NUM-2

*   Addition
    ADD WS-DISPLAY-NUM-1 TO WS-DISPLAY-NUM-2
        GIVING WS-DISPLAY-RESULT
    DISPLAY "  Addition Result: " WS-DISPLAY-RESULT

*   Subtraction
    SUBTRACT WS-DISPLAY-NUM-1 FROM WS-DISPLAY-NUM-2
        GIVING WS-DISPLAY-RESULT
    DISPLAY "  Subtraction Result: " WS-DISPLAY-RESULT

*   Multiplication
    MULTIPLY WS-DISPLAY-NUM-1 BY 2 GIVING WS-DISPLAY-RESULT
    DISPLAY "  Multiplication (*2) Result: " WS-DISPLAY-RESULT

*   Division
    DIVIDE WS-DISPLAY-NUM-1 BY 3 GIVING WS-DISPLAY-RESULT
    DISPLAY "  Division (/3) Result: " WS-DISPLAY-RESULT
    DISPLAY " "
    .
T-ARITHMETIC-DISPLAY-EXIT. EXIT.

*----------------------------------------------------------------------
T-ARITHMETIC-COMP       SECTION.
*----------------------------------------------------------------------
T-ARITHMETIC-COMP-00.

    MOVE "ART-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] COMP (Binary) Format Arithmetic"
    DISPLAY "  Input 1: " WS-COMP-NUM-1
    DISPLAY "  Input 2: " WS-COMP-NUM-2

*   Addition
    ADD WS-COMP-NUM-1 TO WS-COMP-NUM-2 GIVING WS-COMP-RESULT
    DISPLAY "  Addition Result: " WS-COMP-RESULT

*   Subtraction
    SUBTRACT WS-COMP-NUM-1 FROM WS-COMP-NUM-2
        GIVING WS-COMP-RESULT
    DISPLAY "  Subtraction Result: " WS-COMP-RESULT

*   Multiplication
    MULTIPLY WS-COMP-NUM-1 BY 2 GIVING WS-COMP-RESULT
    DISPLAY "  Multiplication (*2) Result: " WS-COMP-RESULT

*   Division
    DIVIDE WS-COMP-NUM-1 BY 3 GIVING WS-COMP-RESULT
    DISPLAY "  Division (/3) Result: " WS-COMP-RESULT
    DISPLAY " "
    .
T-ARITHMETIC-COMP-EXIT. EXIT.

*----------------------------------------------------------------------
T-ARITHMETIC-COMP3      SECTION.
*----------------------------------------------------------------------
T-ARITHMETIC-COMP3-00.

    MOVE "ART-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] COMP-3 (Packed) Format Arithmetic"
    DISPLAY "  Input 1: " WS-COMP3-NUM-1
    DISPLAY "  Input 2: " WS-COMP3-NUM-2

*   Addition
    ADD WS-COMP3-NUM-1 TO WS-COMP3-NUM-2 GIVING WS-COMP3-RESULT
    DISPLAY "  Addition Result: " WS-COMP3-RESULT

*   Subtraction
    SUBTRACT WS-COMP3-NUM-1 FROM WS-COMP3-NUM-2
        GIVING WS-COMP3-RESULT
    DISPLAY "  Subtraction Result: " WS-COMP3-RESULT

*   Multiplication
    MULTIPLY WS-COMP3-NUM-1 BY 2 GIVING WS-COMP3-RESULT
    DISPLAY "  Multiplication (*2) Result: " WS-COMP3-RESULT

*   Division
    DIVIDE WS-COMP3-NUM-1 BY 3 GIVING WS-COMP3-RESULT
    DISPLAY "  Division (/3) Result: " WS-COMP3-RESULT
    DISPLAY " "
    .
T-ARITHMETIC-COMP3-EXIT. EXIT.

*----------------------------------------------------------------------
T-FLOATING-POINT        SECTION.
*----------------------------------------------------------------------
T-FLOATING-POINT-00.

    MOVE "FLT-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Floating Point Arithmetic (COMP-1)"
    DISPLAY "  Input 1: " WS-FLOAT-1
    DISPLAY "  Input 2: " WS-FLOAT-2

*   Addition
    ADD WS-FLOAT-1 TO WS-FLOAT-2 GIVING WS-FLOAT-RESULT
    DISPLAY "  Addition Result: " WS-FLOAT-RESULT

*   Multiplication
    MULTIPLY WS-FLOAT-1 BY 2 GIVING WS-FLOAT-RESULT
    DISPLAY "  Multiplication (*2) Result: " WS-FLOAT-RESULT

*   Division
    DIVIDE WS-FLOAT-2 BY 7 GIVING WS-FLOAT-RESULT
    DISPLAY "  Division (/7) Result: " WS-FLOAT-RESULT

    MOVE "FLT-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Double Precision (COMP-2)"
    DISPLAY "  Input 1: " WS-DOUBLE-1
    DISPLAY "  Input 2: " WS-DOUBLE-2

*   Addition
    ADD WS-DOUBLE-1 TO WS-DOUBLE-2 GIVING WS-DOUBLE-RESULT
    DISPLAY "  Addition Result: " WS-DOUBLE-RESULT

*   Division with high precision
    DIVIDE WS-DOUBLE-2 BY 7 GIVING WS-DOUBLE-RESULT
    DISPLAY "  Division (/7) Result: " WS-DOUBLE-RESULT
    DISPLAY " "
    .
T-FLOATING-POINT-EXIT. EXIT.

*----------------------------------------------------------------------
T-SIGNED-ARITHMETIC     SECTION.
*----------------------------------------------------------------------
T-SIGNED-ARITHMETIC-00.

    MOVE "SGN-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Signed Arithmetic - Display"
    DISPLAY "  Input (negative): " WS-SIGNED-DISP

    MULTIPLY WS-SIGNED-DISP BY -1 GIVING WS-SIGNED-RESULT
    DISPLAY "  After *(-1): " WS-SIGNED-RESULT

    ADD 1000000 TO WS-SIGNED-DISP GIVING WS-SIGNED-RESULT
    DISPLAY "  After +1000000: " WS-SIGNED-RESULT

    MOVE "SGN-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Signed Arithmetic - COMP"
    DISPLAY "  Input (negative): " WS-SIGNED-COMP

    MULTIPLY WS-SIGNED-COMP BY -1 GIVING WS-SIGNED-RESULT
    DISPLAY "  After *(-1): " WS-SIGNED-RESULT

    MOVE "SGN-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Signed Arithmetic - COMP-3"
    DISPLAY "  Input (negative): " WS-SIGNED-COMP3

    MULTIPLY WS-SIGNED-COMP3 BY -1 GIVING WS-SIGNED-RESULT
    DISPLAY "  After *(-1): " WS-SIGNED-RESULT
    DISPLAY " "
    .
T-SIGNED-ARITHMETIC-EXIT. EXIT.

*----------------------------------------------------------------------
T-EDGE-CASES            SECTION.
*----------------------------------------------------------------------
T-EDGE-CASES-00.

    MOVE "EDG-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Edge Cases - Maximum Values"
    DISPLAY "  Max Value: " WS-MAX-VALUE

    SUBTRACT 1 FROM WS-MAX-VALUE GIVING WS-EDGE-RESULT
    DISPLAY "  Max - 1: " WS-EDGE-RESULT

    MOVE "EDG-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Edge Cases - Minimum Values"
    DISPLAY "  Min Value: " WS-MIN-VALUE

    ADD 1 TO WS-MIN-VALUE GIVING WS-EDGE-RESULT
    DISPLAY "  Min + 1: " WS-EDGE-RESULT

    MOVE "EDG-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Edge Cases - Zero Handling"
    DISPLAY "  Zero Value: " WS-ZERO-VALUE

    ADD 100 TO WS-ZERO-VALUE GIVING WS-DISPLAY-RESULT
    DISPLAY "  Zero + 100: " WS-DISPLAY-RESULT

    MULTIPLY WS-ZERO-VALUE BY 12345 GIVING WS-DISPLAY-RESULT
    DISPLAY "  Zero * 12345: " WS-DISPLAY-RESULT
    DISPLAY " "
    .
T-EDGE-CASES-EXIT. EXIT.

*----------------------------------------------------------------------
T-DIVISION-REMAINDER     SECTION.
*----------------------------------------------------------------------
T-DIVISION-REMAINDER-00.

    MOVE "DIV-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Division with Remainder"
    DISPLAY "  Dividend: " WS-DIVIDEND
    DISPLAY "  Divisor: " WS-DIVISOR

    DIVIDE WS-DIVIDEND BY WS-DIVISOR
        GIVING WS-QUOTIENT REMAINDER WS-REMAINDER
    DISPLAY "  Quotient: " WS-QUOTIENT
    DISPLAY "  Remainder: " WS-REMAINDER
    DISPLAY " "
    .
T-DIVISION-REMAINDER-EXIT. EXIT.

*----------------------------------------------------------------------
T-STRING-OPERATIONS     SECTION.
*----------------------------------------------------------------------
T-STRING-OPERATIONS-00.

    MOVE "STR-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] STRING Operation"
    DISPLAY "  String 1: '" WS-STRING-1 "'"
    DISPLAY "  String 2: '" WS-STRING-2 "'"

    STRING WS-STRING-1 DELIMITED BY SPACE
           " - "       DELIMITED BY SIZE
           WS-STRING-2 DELIMITED BY SIZE
           INTO WS-STRING-RESULT
           WITH POINTER WS-STRING-PTR
    END-STRING

    DISPLAY "  Result: '" WS-STRING-RESULT "'"
    DISPLAY "  Final Pointer: " WS-STRING-PTR

    MOVE "STR-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] INSPECT Operation"
    MOVE "HELLO WORLD HELLO" TO WS-STRING-RESULT
    DISPLAY "  Before: '" WS-STRING-RESULT "'"

    INSPECT WS-STRING-RESULT TALLYING WS-STRING-COUNT
        FOR ALL "HELLO"
    DISPLAY "  Count of 'HELLO': " WS-STRING-COUNT

    INSPECT WS-STRING-RESULT REPLACING ALL "HELLO" BY "HALLO"
    DISPLAY "  After Replace: '" WS-STRING-RESULT "'"

    MOVE "STR-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] UNSTRING Operation"
    MOVE "FIELD1:FIELD2:FIELD3" TO WS-STRING-1
    DISPLAY "  Input: '" WS-STRING-1 "'"

    UNSTRING WS-STRING-1 DELIMITED BY ":"
        INTO WS-STRING-RESULT WS-STRING-2
    END-UNSTRING

    DISPLAY "  First Field: '" WS-STRING-RESULT "'"
    DISPLAY "  Second Field: '" WS-STRING-2 "'"
    DISPLAY " "
    .
T-STRING-OPERATIONS-EXIT. EXIT.

*----------------------------------------------------------------------
T-INTRINSIC-FUNCTIONS   SECTION.
*----------------------------------------------------------------------
T-INTRINSIC-FUNCTIONS-00.

    MOVE "FUN-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - ABS"
    DISPLAY "  Input: " WS-FUNC-INPUT
    COMPUTE WS-FUNC-RESULT = FUNCTION ABS(WS-FUNC-INPUT)
    DISPLAY "  ABS Result: " WS-FUNC-RESULT

    MOVE "FUN-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - MAX/MIN"
    COMPUTE WS-FUNC-RESULT =
        FUNCTION MAX(WS-FUNC-INPUT, 0, 5000)
    DISPLAY "  MAX(-12345.67, 0, 5000): " WS-FUNC-RESULT

    COMPUTE WS-FUNC-RESULT =
        FUNCTION MIN(WS-FUNC-INPUT, 0, 5000)
    DISPLAY "  MIN(-12345.67, 0, 5000): " WS-FUNC-RESULT

    MOVE "FUN-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - LENGTH"
    MOVE "Test String" TO WS-STRING-1
    DISPLAY "  String: '" WS-STRING-1 "'"
    DISPLAY "  Length: " FUNCTION LENGTH(WS-STRING-1)

    MOVE "FUN-004" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - MOD"
    COMPUTE WS-FUNC-RESULT = FUNCTION MOD(17, 5)
    DISPLAY "  MOD(17, 5): " WS-FUNC-RESULT

    MOVE "FUN-005" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - INTEGER"
    MOVE 12345.789 TO WS-FUNC-INPUT
    COMPUTE WS-FUNC-RESULT = FUNCTION INTEGER(WS-FUNC-INPUT)
    DISPLAY "  INTEGER(12345.789): " WS-FUNC-RESULT

    MOVE "FUN-006" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Intrinsic Function - SQRT"
    COMPUTE WS-FUNC-RESULT = FUNCTION SQRT(2500)
    DISPLAY "  SQRT(2500): " WS-FUNC-RESULT
    DISPLAY " "
    .
T-INTRINSIC-FUNCTIONS-EXIT. EXIT.

*----------------------------------------------------------------------
T-DATE-TIME             SECTION.
*----------------------------------------------------------------------
T-DATE-TIME-00.

    MOVE "DTE-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Date/Time - Current Date"
    ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
    DISPLAY "  YYYYMMDD: " WS-CURR-YEAR WS-CURR-MONTH WS-CURR-DAY

    MOVE "DTE-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Date/Time - Current Time"
    ACCEPT WS-CURRENT-TIME FROM TIME
    DISPLAY "  HHMMSS: " WS-CURR-HOUR WS-CURR-MIN WS-CURR-SEC

    MOVE "DTE-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Date/Time - Current-Date Function"
    DISPLAY "  Current-Date: " FUNCTION CURRENT-DATE
    DISPLAY " "
    .
T-DATE-TIME-EXIT. EXIT.

*----------------------------------------------------------------------
T-TABLE-SUBSCRIPT       SECTION.
*----------------------------------------------------------------------
T-TABLE-SUBSCRIPT-00.

    MOVE "TBL-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Table Operations - Initialize"

*   Initialize table
    PERFORM VARYING WS-COUNTER FROM 1 BY 1
        UNTIL WS-COUNTER > 10
       COMPUTE WS-ENTRY-NUM(WS-COUNTER) = WS-COUNTER * 100
       STRING "ENTRY-" WS-COUNTER DELIMITED BY SIZE
              INTO WS-ENTRY-NAME(WS-COUNTER)
       END-STRING
    END-PERFORM

    DISPLAY "  Table initialized with 10 entries"

    MOVE "TBL-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Table Operations - Access by Index"
    SET WS-IDX TO 5
    DISPLAY "  Entry 5 Number: " WS-ENTRY-NUM(WS-IDX)
    DISPLAY "  Entry 5 Name: " WS-ENTRY-NAME(WS-IDX)

    MOVE "TBL-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Table Operations - Search"
    MOVE 0 TO WS-LOOP-RESULT

    PERFORM VARYING WS-COUNTER FROM 1 BY 1
        UNTIL WS-COUNTER > 10
       IF WS-ENTRY-NUM(WS-COUNTER) = 700
          MOVE WS-COUNTER TO WS-LOOP-RESULT
       END-IF
    END-PERFORM

      IF WS-ENTRY-NUM(WS-COUNTER) = 700
          MOVE WS-COUNTER TO WS-LOOP-RESULT.

    DISPLAY "  Found 700 at position: " WS-LOOP-RESULT
    DISPLAY " "
    .
T-TABLE-SUBSCRIPT-EXIT. EXIT.

*----------------------------------------------------------------------
T-REFERENCE-MOD         SECTION.
*----------------------------------------------------------------------
T-REFERENCE-MOD-00.

    MOVE "REF-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Reference Modification"
    DISPLAY "  Source String: '" WS-REF-STRING "'"
    DISPLAY "  Start Position: " WS-REF-START
    DISPLAY "  Length: " WS-REF-LENGTH

    MOVE WS-REF-STRING(WS-REF-START:WS-REF-LENGTH)
        TO WS-REF-RESULT
    DISPLAY "  Extracted: '" WS-REF-RESULT "'"

    MOVE "REF-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Reference Modification - Update"
    MOVE "ABCDEFGHIJ" TO WS-REF-RESULT
    DISPLAY "  Before: '" WS-REF-RESULT "'"
    MOVE "12345" TO WS-REF-RESULT(3:5)
    DISPLAY "  After (3:5)='12345': '" WS-REF-RESULT "'"
    DISPLAY " "
    .
T-REFERENCE-MOD-EXIT. EXIT.

*----------------------------------------------------------------------
T-EVALUATE              SECTION.
*----------------------------------------------------------------------
T-EVALUATE-00.

    MOVE "EVL-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] EVALUATE Statement Tests"

*   Test case 1
    MOVE "AA" TO WS-EVAL-CODE
    DISPLAY "  Test Code: " WS-EVAL-CODE

    EVALUATE TRUE
       WHEN "AA"
          MOVE "Code is AA" TO WS-EVAL-RESULT
       WHEN "BB"
          MOVE "Code is BB" TO WS-EVAL-RESULT
       WHEN "CC"
          MOVE "Code is CC" TO WS-EVAL-RESULT
*       WHEN OTHER
*          MOVE "Code is Other" TO WS-EVAL-RESULT
    END-EVALUATE

    DISPLAY "  Result: " WS-EVAL-RESULT

*   Test case 2 - Range
    MOVE 5 TO WS-COUNTER
    DISPLAY "  Test Counter: " WS-COUNTER

    EVALUATE WS-COUNTER
       WHEN 1 THRU 3
          MOVE "Range 1-3" TO WS-EVAL-RESULT
       WHEN 4 THRU 6
          MOVE "Range 4-6" TO WS-EVAL-RESULT
       WHEN 7 THRU 9
          MOVE "Range 7-9" TO WS-EVAL-RESULT
       WHEN OTHER
          MOVE "Out of Range" TO WS-EVAL-RESULT
    END-EVALUATE

    DISPLAY "  Result: " WS-EVAL-RESULT

*   Test case 3 - TRUE/FALSE
    EVALUATE TRUE
       WHEN WS-COUNTER > 10
          MOVE "Greater than 10" TO WS-EVAL-RESULT
       WHEN WS-COUNTER > 5
          MOVE "Greater than 5" TO WS-EVAL-RESULT
       WHEN WS-COUNTER > 0
          MOVE "Greater than 0" TO WS-EVAL-RESULT
       WHEN OTHER
          MOVE "Not Greater than 0" TO WS-EVAL-RESULT
    END-EVALUATE

    DISPLAY "  Boolean Result: " WS-EVAL-RESULT
    DISPLAY " "
    .
T-EVALUATE-EXIT. EXIT.

*----------------------------------------------------------------------
T-PERFORM-VARIATIONS    SECTION.
*----------------------------------------------------------------------
T-PERFORM-VARIATIONS-00.

    MOVE "PRF-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] PERFORM Variations - Simple Loop"
    MOVE 0 TO WS-LOOP-RESULT

    PERFORM 5 TIMES
       ADD 10 TO WS-LOOP-RESULT
    END-PERFORM

    DISPLAY "  Result after 5 times: " WS-LOOP-RESULT

    MOVE "PRF-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] PERFORM Variations - VARYING"
    MOVE 0 TO WS-LOOP-RESULT

    PERFORM VARYING WS-COUNTER FROM 1 BY 2
        UNTIL WS-COUNTER > 10
       ADD WS-COUNTER TO WS-LOOP-RESULT
    END-PERFORM

    DISPLAY "  Sum of 1,3,5,7,9: " WS-LOOP-RESULT

    MOVE "PRF-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] PERFORM Variations - UNTIL"
    MOVE 0 TO WS-COUNTER
    MOVE 0 TO WS-LOOP-RESULT

    PERFORM UNTIL WS-COUNTER = 100
       ADD 1 TO WS-COUNTER
       ADD WS-COUNTER TO WS-LOOP-RESULT
    END-PERFORM

    DISPLAY "  Sum 1 to 100: " WS-LOOP-RESULT
    DISPLAY " "
    .
T-PERFORM-VARIATIONS-EXIT. EXIT.

*----------------------------------------------------------------------
T-BINARY-ALIGNMENT      SECTION.
*----------------------------------------------------------------------
T-BINARY-ALIGNMENT-00.

    MOVE "ALN-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Binary Field Alignment Test"

    MOVE "A" TO WS-ALIGN-CHAR
    MOVE 1234 TO WS-ALIGN-COMP
    MOVE "B" TO WS-ALIGN-CHAR2
    MOVE 567890123 TO WS-ALIGN-COMP2

    DISPLAY "  Char 1: " WS-ALIGN-CHAR
    DISPLAY "  COMP 1: " WS-ALIGN-COMP
    DISPLAY "  Char 2: " WS-ALIGN-CHAR2
    DISPLAY "  COMP 2: " WS-ALIGN-COMP2
    DISPLAY " "
    .
T-BINARY-ALIGNMENT-EXIT. EXIT.

*----------------------------------------------------------------------
T-OVERFLOW-HANDLING      SECTION.
*----------------------------------------------------------------------
T-OVERFLOW-HANDLING-00.

    MOVE "OVF-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Overflow Handling"
    DISPLAY "  Input 1: " WS-OVERFLOW-TEST
    DISPLAY "  Input 2: " WS-OVERFLOW-ADD

    ADD WS-OVERFLOW-ADD TO WS-OVERFLOW-TEST
        GIVING WS-OVERFLOW-RESULT
        ON SIZE ERROR
           DISPLAY "  SIZE ERROR detected on addition"
        NOT ON SIZE ERROR
           DISPLAY "  Addition Result: " WS-OVERFLOW-RESULT
    END-ADD

*   Test multiplication overflow
    MOVE 9999 TO WS-OVERFLOW-TEST
    MULTIPLY WS-OVERFLOW-TEST BY 100
        GIVING WS-OVERFLOW-RESULT
        ON SIZE ERROR
           DISPLAY "  SIZE ERROR detected on multiplication"
        NOT ON SIZE ERROR
           DISPLAY "  Multiplication Result: " WS-OVERFLOW-RESULT
    END-MULTIPLY
    DISPLAY " "
    .
T-OVERFLOW-HANDLING-EXIT. EXIT.

*----------------------------------------------------------------------
T-MIXED-SIGNS            SECTION.
*----------------------------------------------------------------------
T-MIXED-SIGNS-00.

    MOVE "MIX-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Mixed Sign Operations"
    DISPLAY "  Positive: " WS-POS-NUM
    DISPLAY "  Negative: " WS-NEG-NUM

    ADD WS-POS-NUM TO WS-NEG-NUM GIVING WS-MIXED-RESULT
    DISPLAY "  Addition Result: " WS-MIXED-RESULT

    SUBTRACT WS-NEG-NUM FROM WS-POS-NUM GIVING WS-MIXED-RESULT
    DISPLAY "  Subtraction Result: " WS-MIXED-RESULT

    MULTIPLY WS-POS-NUM BY WS-NEG-NUM GIVING WS-MIXED-RESULT
    DISPLAY "  Multiplication Result: " WS-MIXED-RESULT
    DISPLAY " "
    .
T-MIXED-SIGNS-EXIT. EXIT.

*----------------------------------------------------------------------
T-TYPE-CONVERSIONS       SECTION.
*----------------------------------------------------------------------
T-TYPE-CONVERSIONS-00.

    MOVE "CVT-001" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Type Conversion - DISPLAY to COMP"
    MOVE 12345.67 TO WS-DISPLAY-NUM-1
    MOVE WS-DISPLAY-NUM-1 TO WS-COMP-NUM-1
    DISPLAY "  Display Value: " WS-DISPLAY-NUM-1
    DISPLAY "  COMP Value: " WS-COMP-NUM-1

    MOVE "CVT-002" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Type Conversion - COMP to COMP-3"
    MOVE WS-COMP-NUM-1 TO WS-COMP3-NUM-1
    DISPLAY "  COMP Value: " WS-COMP-NUM-1
    DISPLAY "  COMP-3 Value: " WS-COMP3-NUM-1

    MOVE "CVT-003" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Type Conversion - Numeric to String"
    MOVE 98765 TO WS-COUNTER
    MOVE WS-COUNTER TO WS-STRING-1
    DISPLAY "  Numeric: " WS-COUNTER
    DISPLAY "  String: '" WS-STRING-1 "'"

    MOVE "CVT-004" TO WS-TEST-ID
    ADD 1 TO WS-TEST-COUNT

    DISPLAY "[" WS-TEST-ID "] Type Conversion - String to Numeric"
    MOVE "00123" TO WS-STRING-1
    MOVE WS-STRING-1 TO WS-COUNTER
    DISPLAY "  String: '" WS-STRING-1 "'"
    DISPLAY "  Numeric: " WS-COUNTER
    DISPLAY " "
    .
T-TYPE-CONVERSIONS-EXIT. EXIT.

*******************************************************************************
* FINISH
*******************************************************************************

*----------------------------------------------------------------------
Z-FINISH               SECTION.
*----------------------------------------------------------------------
Z-00.

    DISPLAY WS-SEPARATOR
    DISPLAY "Total tests executed: " WS-TEST-COUNT
    DISPLAY "Test execution completed"
    DISPLAY WS-SEPARATOR
    .
Z-EXIT. EXIT.

END PROGRAM REGTEST-COMP.
