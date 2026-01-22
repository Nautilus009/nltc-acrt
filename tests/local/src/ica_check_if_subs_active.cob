*
****************************************************************
* Program ICA_CHECK_IF_SUBS_ACTIVE.COB                         *
* Migrated by European Business Solutions, SAS                 *
* On 2023-02-24 10:25:34, ISMapper version v3.2.1             *
****************************************************************
*
*########### PROGRAM_NAME:  [ ICA_CHECK_IF_SUBS_ACTIVE.COB ]    ##############*
*#                                                                           #*
*#                                                [ ןיילנוא לאכ ] : תכרעמ תת #*
*#                                                         [    ] :לודומה םש #*
*#                                                                           #*
*# יונמל ךייושמ רטמרפכ ןתינה  ע"ב / סיטרכ םאה תקדוב תינכותה :ילנויצקנופ רואת #*
*#                                             ליעפ טנרטניא                  #*
*#                                                                           #*
*#                                                                   :םיחתפמ #*
*#               ןורחא ןוכדע .ת        ךיראת       עצבמ םש       בלש         #*
*#                     [    ]    [13-JUL-2006] [TP_AKOTLE]     בוציע         #*
*#                                                                           #*
*#                                                                  :םירטמרפ #*
*#              (שומיש ,םיכרע ,רבסה) רואת  פ/ק          WORKSPACE םש         #*
*#                                                                           #*
*# טנרטניא יונמל סיטרכ/ע"ב ךויש תקידב הנבמ פ/ק ICA_CHECK_IF_SUBS_ACTIVE_WKSP #*
*#           הניטורמ האיצי סוטטס הרקב הנבמ פ/ק UTL_CONTROL_ACW
*#                                                                           #*
*#                                                               :עדימ ירגאמ #*
*#                    תירבעב םש / רואת    פ/ק                   הלבט         #*
*#       תיתיב תואקנב - הנשי תכרעמ ייונמ    ק                    180         #*
*#  תיתיב תואקנב - הנשי תכרעמ יונמ ירצומ    ק                    181         #*
*#               ןיילנוא לאכ תכרעמ ייונמ    ק                    080         #*
*#          ןיילנוא לאכ תכרעמ יונמ ירצומ    ק                    081         #*
*#                                                                           #*
*#                                                                           #*
*#                                                            :םישגדהו תורעה #*
*#                                                                           #*
*#                                                    :םייונישו םינוכדע בקעמ #*
*#                     יונישה רואת     עצבמ םש       ךיראת       CID         #*
*#      [                         ]  [         ]  [        ]    [   ]        #*
*# CCA Project - Readiness           TP_ONISSA    20-Jul-2017   #55557       #*
*#############################################################################*
*#-----------------------------------------------------------------------------
IDENTIFICATION DIVISION.
*#-----------------------------------------------------------------------------
PROGRAM-ID. ICA_CHECK_IF_SUBS_ACTIVE.

*#-----------------------------------------------------------------------------
ENVIRONMENT DIVISION.
*#-----------------------------------------------------------------------------

*#-----------------------------------------------------------------------------
DATA DIVISION.
*#-----------------------------------------------------------------------------

*#-----------------------------------------------------------------------------
WORKING-STORAGE SECTION.
*# 12-NOV-2012 16:52:13.43 - TP_TSARSU - include the TLG wksp.
*ebs COPY "ICA_CDD_WKSP:ICA_TLG_WKSP"             FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TLG_WKSP"                            .

*#-----------------------------------------------------------------------------

*ebs COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_WKSP"          FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_WKSP"                         .

*ebs 01 ICA_ICF_EXCEPTION_HANDLER
*ebs    PIC S9(9) COMP VALUE EXTERNAL ICA_ICF_EXCEPTION_HANDLER.
01 ICA_ICF_EXCEPTION_HANDLER PIC S9(9) COMP EXTERNAL.

*  האיגש תועדוה תרדגה
COPY "UTL_SOURCE:UTL_MESSAGE.INC".
COPY "ICA_SOURCE:ICA_MESSAGE.INC".

*  DB לומ תולועפ תרדגה
COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".

* תינכותה שומישב val יעובק
*ebs COPY "ICA_CDD_FIELD:Vz_HEB_YES"                     FROM DICTIONARY.
COPY "ICA_CDD_FIELD:Vz_HEB_YES"                                    .
*ebs COPY "ICA_CDD_FIELD:Vz_HEB_NO"                      FROM DICTIONARY.
COPY "ICA_CDD_FIELD:Vz_HEB_NO"                                     .
*ebs COPY "ICA_CDD_FIELD.V4005zACTIV"                    FROM DICTIONARY.
*ebs COPY "ICA_CDD_FIELD.V4005zACTIV"                                   .
COPY "ICA_CDD_FIELD:V4005zACTIV"                                   .
*ebs COPY "ICA_CDD_FIELD.V4005zWAITING_PRE_CONVERSION"   FROM DICTIONARY.
*ebs COPY "ICA_CDD_FIELD.V4005zWAITING_PRE_CONVERSION"                  .
COPY "ICA_CDD_FIELD:V4005zWAITING_PRE_CONVERSION"                  .

* האיגש תעדוהל טסקט רוזחיאל הניטור שומישל םינבמ
*ebs COPY "ICA_CDD_WKSP:ICA_XLATE_MSG_WKSP"              FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_XLATE_MSG_WKSP"                             .

* Start Change #55557 on 27 לויב 2016 by l_onissa
* 180 הנשי תכרעמ טנרטניא ייונמ
*COPY "ICD_CDD_WKSP:ICD_HBT_180_CST_CATLOG_DBW"      FROM DICTIONARY.

* 181 הנשי תכרעמ טנרטניא ייונמ ירצומ
*COPY "ICD_CDD_WKSP:ICD_HBT_181_PRD_CATLOG_DBW"      FROM DICTIONARY.
* End Change #55557

* 79 CAL ONLINE תוחוקל םיפסונ םיטרפ
*ebs COPY "ICD_CDD_WKSP:ICD_COT_079_CST_DETAILS_DBW"     FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_COT_079_CST_DETAILS_DBW"                    .

* 80 CAL ONLINE ייונמ
*ebs COPY "ICD_CDD_WKSP:ICD_COT_080_CST_CATLOG_DBW"      FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_COT_080_CST_CATLOG_DBW"                     .

* 81 CAL ONLINE ייונמ ירצומ
*ebs COPY "ICD_CDD_WKSP:ICD_COT_081_PRD_CATLOG_DBW"      FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_COT_081_PRD_CATLOG_DBW"                     .

* 100 תוחוקל תובותכ
*ebs COPY "ICD_CDD_WKSP:ICD_MCT_100_ADDR_DBW"            FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_MCT_100_ADDR_DBW"                           .

* Start Change #55557 on 27 לויב 2016 by l_onissa
* 190 םינשי םייונמ E_MAIL תובותכ
*COPY "ICD_CDD_WKSP:ICD_HBT_190_EMAIL_ADDR_DBW"    FROM DICTIONARY.
* End Change #55557

* רצומ גוס
*ebs COPY "ICD_CDD_WKSP:ICD_PRT_300_PROD_TYPES_DBW"       FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_PRT_300_PROD_TYPES_DBW"                      .

* 500 רמוחו לאכ תוחוקל
*ebs COPY "ICD_CDD_WKSP:ICD_CST_500_CST_CATLOG_DBW"      FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_CST_500_CST_CATLOG_DBW"                     .

* לוחתאל םינבמ
*ebs COPY "ICD_CDD_WKSP:ICD_COT_081_PRD_CATLOG_DBW"      FROM DICTIONARY
COPY "ICD_CDD_WKSP:ICD_COT_081_PRD_CATLOG_DBW"
         REPLACING ICD_COT_081_PRD_CATLOG             BY
                   ICD_COT_081_PRD
                   ICD_COT_081_PRD_CATLOG_PRW         BY
                   ICD_COT_081_PRD_PRW
                   ICD_COT_081_PRD_CATLOG_DBW         BY
                   ICD_COT_081_PRD_CATLOG_INIT.

* Start Change #55557 on 27 לויב 2016 by l_onissa
*COPY "ICD_CDD_WKSP:ICD_HBT_181_PRD_CATLOG_DBW"      FROM DICTIONARY
*         REPLACING ICD_HBT_181_PRD_CATLOG             BY
*                   ICD_HBT_181_PRD
*                   ICD_HBT_181_PRD_CATLOG_PRW         BY
*                   ICD_HBT_181_PRD_PRW
*                   ICD_HBT_181_PRD_CATLOG_DBW         BY
*                   ICD_HBT_181_PRD_CATLOG_INIT.
* End Change #55557

01 Lz_VARIABLES.
   03 Lz_EXIST_FIRST              PIC X     VALUE " ".
      88 Lz_SW_FIRST_TIME                   VALUE " ".
      88 Lz_SW_NOT_FIRST_TIME               VALUE "1".
   03 LS_SW_FETCH                 PIC X.
      88 Lz_END_FETCH                       VALUE "1".
   03 Lz_INQUIRE_RDB              PIC S9(9) COMP.
   03 Lz_FETCH_RDB                PIC S9(9) COMP.
   03 Lz_OPEN_CURSOR_RDB          PIC S9(9) COMP.
   03 Lz_CLOSE_CURSOR_RDB         PIC S9(9) COMP.
   03 Lz_CURSOR_SW                PIC X.
      88 Lz_START_CURSOR                    VALUE " ".
      88 Lz_END_CURSOR                      VALUE "1".
   03 Lz_SW_MERCHANT              PIC 9     VALUE 0.
      88 Lz_MERCHANT                        VALUE 1.

01 OREN PIC X(20) VALUE "00000000000000804347".

*#-----------------------------------------------------------------------------
LINKAGE SECTION.
*#-----------------------------------------------------------------------------
*ebs COPY "ICA_CDD_WKSP:ICA_CHECK_IF_SUBS_ACTIVE_WKSP"   FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_CHECK_IF_SUBS_ACTIVE_WKSP"                  .
*ebs COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"                 FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"                                .

*#-----------------------------------------------------------------------------
PROCEDURE DIVISION  USING ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                          UTL_CONTROL_ACW.
*#-----------------------------------------------------------------------------
A-MAIN                              SECTION.
*#-----------------------------------------------------------------------------
*# Section: A-MAIN
*# Purpose:
*# Description:
*#
*#-----------------------------------------------------------------------------
A-00.

    PERFORM A-INIT.
    PERFORM C-CHECK.
    PERFORM Z-FINISH.

A-EXIT.
    EXIT.

*-----------------------------------------------------------------------------
A-INIT                              SECTION.
*-----------------------------------------------------------------------------
A-00.
*# 12-NOV-2012 16:52:13.43 - TP_TSARSU - Move program name to TLG wksp.
    MOVE "ICA_CHECK_IF_SUBS_ACTIVE"
      TO Pz_PROG_NAME IN ICA_TLG_WKSP.

*# 12-NOV-2012 16:52:13.43 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_INIT_PROG.INC".


    IF Lz_SW_FIRST_TIME
* Start Change #55557 on 27 לויב 2016 by l_onissa
*       INITIALIZE ICD_HBT_181_PRD_CATLOG_INIT
* End Change #55557
        INITIALIZE ICD_COT_081_PRD_CATLOG_INIT
       ADD DPz_SYM_INQUIRE      TO DPz_SYM_ACCESS_RDB GIVING Lz_INQUIRE_RDB
       ADD DPz_SYM_FETCH        TO DPz_SYM_ACCESS_RDB GIVING Lz_FETCH_RDB
       ADD DPz_SYM_OPEN_CURSOR  TO DPz_SYM_ACCESS_RDB GIVING Lz_OPEN_CURSOR_RDB
       ADD DPz_SYM_CLOSE_CURSOR TO DPz_SYM_ACCESS_RDB GIVING Lz_CLOSE_CURSOR_RDB
       SET Lz_SW_NOT_FIRST_TIME TO TRUE
    END-IF.

*   Establish condition EXCEPTION_HANDLER.
    CALL 'LIB_ESTABLISH' USING BY VALUE ICA_ICF_EXCEPTION_HANDLER.

    MOVE SPz_MSG_NORMAL
    TO   SPz_ACW_PROC_AUX_STATUS    IN UTL_CONTROL_ACW.

    MOVE SPACES
      TO CO_USER_ID                 IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         E_MAIL_ADDR                IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP.

    MOVE 0
      TO CO_STATUS                  IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         LAST_ACTIVITY_DATE         IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         OPEN_DATE                  IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         JOINING_DATE               IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         Lz_SW_MERCHANT.

    MOVE "0"
      TO Pz_MSG_DIR(1)
         Pz_MSG_DIR(2).

    MOVE Vz_HEB_NO     TO ACTIVE_FLAG.

* check parameters
    IF Pz_PROD_EXT_ID               IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = SPACES
       OR
       Pz_PROD_EXT_ID               IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = ZEROES
       MOVE ICA_W_1611
       TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "רצומ"                  TO Pz_MSG_PARAM(1)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

    IF Pz_EXT_ID_NUM_TYPE_CODE      IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 0
       MOVE ICA_W_1611
       TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "רצומ ינוציח ההזמ"      TO Pz_MSG_PARAM(1)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

    IF Pz_PROD_FAMILY_CODE          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 0
       MOVE ICA_W_1611
       TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "רצומ תחפשמ"            TO Pz_MSG_PARAM(1)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

    IF Pz_PROD_FAMILY_CODE          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 6
       IF  (Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 2
         OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 21
         OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 34
         OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP > 250)

            SET Lz_MERCHANT TO TRUE
       ELSE
          PERFORM Z-FINISH
       END-IF
    ELSE
       IF Pz_PROD_FAMILY_CODE          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 13
          IF  (Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 6
            OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 22
            OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 35
            OR Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 250)

               SET Lz_MERCHANT TO TRUE
          ELSE
             PERFORM Z-FINISH
          END-IF
       END-IF
    END-IF

    CALL 'UTL_GET_DATE_TIME' USING UTL_GET_DATE_TIME_WKSP.

B-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C-CHECK                             SECTION.
*-----------------------------------------------------------------------------
C-00.

* check if prod belpng to cal online subscriber
    PERFORM C100-CHECK-CAL-ONLINE-SUBS.

* check if prod belpng to old subscriber only if not merchant subscriber
* Start Change #55557 on 27 לויב 2016 by l_onissa
*    IF CO_USER_ID          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = SPACES
*       AND NOT Lz_MERCHANT
*       PERFORM C200-CHECK-OLD-SUBS
*    END-IF.
* End Change #55557

C-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C100-CHECK-CAL-ONLINE-SUBS          SECTION.
*-----------------------------------------------------------------------------
C100-00.

* check if product belong to cal online active subscriber

* open cursor 081
    MOVE ICD_COT_081_PRD_CATLOG_INIT
      TO ICD_COT_081_PRD_CATLOG_DBW.

    MOVE Pz_PROD_EXT_ID             IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
      TO PROD_EXT_ID                IN ICD_COT_081_PRD_CATLOG_PRW.

    IF Pz_PROD_FAMILY_CODE          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 6
       OR
       Pz_PROD_FAMILY_CODE          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = 13
       MOVE 0
         TO EXT_ID_NUM_TYPE_CODE    IN ICD_COT_081_PRD_CATLOG_PRW
    ELSE
       MOVE Pz_EXT_ID_NUM_TYPE_CODE IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
         TO EXT_ID_NUM_TYPE_CODE    IN ICD_COT_081_PRD_CATLOG_PRW
    END-IF.

    MOVE Pz_PROD_FAMILY_CODE        IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
      TO PROD_FAMILY_CODE           IN ICD_COT_081_PRD_CATLOG_PRW.

    MOVE 4
      TO DPz_KEY_SEQUENCE           IN ICD_COT_081_PRD_CATLOG_DBW.
    MOVE Lz_OPEN_CURSOR_RDB
      TO DPz_ACTION                 IN ICD_COT_081_PRD_CATLOG_DBW.

    PERFORM C110-OPEN-CLOSE-081-CURSOR.

* prepare fetch parameters
    MOVE Lz_FETCH_RDB
      TO DPz_ACTION                 OF ICD_COT_081_PRD_CATLOG_DBW.

    SET Lz_START_CURSOR TO TRUE.

    PERFORM C120-READ-CURSOR-081 UNTIL Lz_END_CURSOR.

* close cursor 081
    MOVE Lz_CLOSE_CURSOR_RDB
      TO DPz_ACTION                 IN ICD_COT_081_PRD_CATLOG_DBW.
    PERFORM C110-OPEN-CLOSE-081-CURSOR.

C100-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C110-OPEN-CLOSE-081-CURSOR          SECTION.
*-----------------------------------------------------------------------------
C110-00.


    CALL 'ICD_COT_081_PRD_CATLOG_DBA' USING ICD_COT_081_PRD_CATLOG_DBW.

    IF DPz_STATUS                   IN ICD_COT_081_PRD_CATLOG_DBW
       NOT = SPz_MSG_NORMAL
       MOVE ICA_W_0742
       TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "081"                   TO Pz_MSG_PARAM(1)
       MOVE "CURSOR תריגס/תחיתפב"   TO Pz_MSG_PARAM(2)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

C110-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C120-READ-CURSOR-081                SECTION.
*-----------------------------------------------------------------------------
C120-00.

    CALL 'ICD_COT_081_PRD_CATLOG_DBA' USING ICD_COT_081_PRD_CATLOG_DBW.

    EVALUATE DPz_STATUS             IN ICD_COT_081_PRD_CATLOG_DBW

        WHEN SPz_MSG_NO_DATA_FOUND
        WHEN SPz_MSG_END_OF_DATA_SET
             SET Lz_END_CURSOR TO TRUE
        WHEN SPz_MSG_NORMAL
             PERFORM C125-READ-080
             
        WHEN OTHER
             MOVE ICA_W_0742
               TO SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
             MOVE "081"                   TO Pz_MSG_PARAM(1)
             MOVE "האירקב"                TO Pz_MSG_PARAM(2)
             CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
             PERFORM Z-FINISH
    END-EVALUATE.

C120-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C125-READ-080                       SECTION.
*-----------------------------------------------------------------------------
C125-00.

    MOVE CO_USER_ID                 IN ICD_COT_081_PRD_CATLOG
      TO CO_USER_ID                 IN ICD_COT_080_CST_CATLOG_PRW.
    MOVE Lz_INQUIRE_RDB
      TO DPz_ACTION                 OF ICD_COT_080_CST_CATLOG_DBW.
    MOVE 0
      TO DPz_KEY_SEQUENCE           OF ICD_COT_080_CST_CATLOG_DBW.

    CALL 'ICD_COT_080_CST_CATLOG_DBA' USING ICD_COT_080_CST_CATLOG_DBW.

    EVALUATE DPz_STATUS                  IN ICD_COT_080_CST_CATLOG_DBW
        WHEN SPz_MSG_NORMAL
             IF CO_USER_ID               IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = SPACES
                OR
                CO_STATUS                IN ICD_COT_080_CST_CATLOG = V4005zACTIV
                OR
                CO_STATUS                IN ICD_COT_080_CST_CATLOG
                     = V4005zWAITING_PRE_CONVERSION

                MOVE CO_USER_ID          IN ICD_COT_080_CST_CATLOG
                  TO CO_USER_ID          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                MOVE LAST_ACTIVITY_DATE  IN ICD_COT_080_CST_CATLOG
                  TO LAST_ACTIVITY_DATE  IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
             END-IF
             IF CO_STATUS                IN ICD_COT_080_CST_CATLOG
                = V4005zACTIV
                OR
                CO_STATUS                IN ICD_COT_080_CST_CATLOG
                     = V4005zWAITING_PRE_CONVERSION

                MOVE Vz_HEB_YES
                  TO ACTIVE_FLAG         IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                MOVE CO_STATUS           IN ICD_COT_080_CST_CATLOG
                  TO CO_STATUS           IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                MOVE OPEN_DATE           IN ICD_COT_080_CST_CATLOG
                  TO OPEN_DATE           IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                MOVE JOINING_DATE        IN ICD_COT_081_PRD_CATLOG
                  TO JOINING_DATE        IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
                PERFORM C130-GET-E-MAIL
                SET Lz_END_CURSOR TO TRUE
             END-IF

        WHEN SPz_MSG_NO_DATA_FOUND
             SET Lz_END_CURSOR TO TRUE

        WHEN OTHER
             MOVE ICA_W_0742
             TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
             MOVE "080"                   TO Pz_MSG_PARAM(1)
             MOVE "האירקב"                TO Pz_MSG_PARAM(2)
             CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
             PERFORM Z-FINISH
    END-EVALUATE.

C125-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C130-GET-E-MAIL                     SECTION.
*-----------------------------------------------------------------------------
C130-00.

    IF PROD_FAMILY_CODE             IN ICD_COT_081_PRD_CATLOG = 6
      OR
       PROD_FAMILY_CODE             IN ICD_COT_081_PRD_CATLOG = 13
       PERFORM C132-READ-SUBS-DETAILS
    ELSE
       PERFORM C134-GET-CUST-DETAILS
    END-IF.

C130-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C132-READ-SUBS-DETAILS              SECTION.
*-----------------------------------------------------------------------------
C132-00.

    MOVE CO_USER_ID                 IN ICD_COT_080_CST_CATLOG_PRW
      TO CO_USER_ID                 IN ICD_COT_079_CST_DETAILS_PRW.

    MOVE Lz_INQUIRE_RDB
      TO DPz_ACTION                 OF ICD_COT_079_CST_DETAILS_DBW.
    MOVE 0
      TO DPz_KEY_SEQUENCE           OF ICD_COT_080_CST_CATLOG_DBW.

    CALL 'ICD_COT_079_CST_DETAILS_DBA' USING ICD_COT_079_CST_DETAILS_DBW.

    IF DPz_STATUS                   IN ICD_COT_079_CST_DETAILS_DBW
       = SPz_MSG_NORMAL
       MOVE E_MAIL_ADDR             IN ICD_COT_079_CST_DETAILS
         TO E_MAIL_ADDR             IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP

    ELSE
       MOVE ICA_W_0742
         TO SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "079"                   TO Pz_MSG_PARAM(1)
       MOVE "האירקב"                TO Pz_MSG_PARAM(2)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

C132-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C134-GET-CUST-DETAILS               SECTION.
*-----------------------------------------------------------------------------
C134-00.

    MOVE CUST_INT_ID                IN ICD_COT_080_CST_CATLOG
      TO CUST_INT_ID                IN ICD_CST_500_CST_CATLOG_PRW.

    MOVE 0
      TO DPz_KEY_SEQUENCE           OF ICD_CST_500_CST_CATLOG_DBW.

    MOVE Lz_INQUIRE_RDB
      TO DPz_ACTION                 OF ICD_CST_500_CST_CATLOG_DBW.

    CALL 'ICD_CST_500_CST_CATLOG_DBA' USING ICD_CST_500_CST_CATLOG_DBW.

    IF DPz_STATUS                   IN ICD_CST_500_CST_CATLOG_DBW
       = SPz_MSG_NORMAL
       IF CUST_PRIVATE_ADDR_INTR_ID IN ICD_CST_500_CST_CATLOG = 0
          PERFORM C136-GET-100-MAIL-ADDRESS
       END-IF
    ELSE
       MOVE ICA_W_0742
         TO SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "079"                   TO Pz_MSG_PARAM(1)
       MOVE "האירקב"                TO Pz_MSG_PARAM(2)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

C134-EXIT.
    EXIT.
*-----------------------------------------------------------------------------
C136-GET-100-MAIL-ADDRESS           SECTION.
*-----------------------------------------------------------------------------
C136-00.

    MOVE CUST_PRIVATE_ADDR_INTR_ID  IN ICD_CST_500_CST_CATLOG
      TO ADDR_INTR_ID               IN ICD_MCT_100_ADDR_PRW.

    MOVE Lz_INQUIRE_RDB
      TO DPz_ACTION                 OF ICD_MCT_100_ADDR_DBW.

    MOVE 0
      TO DPz_KEY_SEQUENCE           OF ICD_MCT_100_ADDR_DBW.

    CALL 'ICD_MCT_100_ADDR_DBA' USING ICD_MCT_100_ADDR_DBW.
    IF DPz_STATUS                   IN ICD_MCT_100_ADDR_DBW
       = SPz_MSG_NORMAL
       MOVE E_MAIL_ADDR             IN ICD_MCT_100_ADDR
         TO E_MAIL_ADDR             IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
    ELSE
       MOVE ICA_W_0742
         TO SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
       MOVE "100"                   TO Pz_MSG_PARAM(1)
       MOVE "האירקב"                TO Pz_MSG_PARAM(2)
       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
       PERFORM Z-FINISH
    END-IF.

C136-EXIT.
    EXIT.
* Start Change #55557 on 27 לויב 2016 by l_onissa
*-----------------------------------------------------------------------------
*C200-CHECK-OLD-SUBS                 SECTION.
*-----------------------------------------------------------------------------
*C200-00.
* open cursor 181
*    MOVE ICD_HBT_181_PRD_CATLOG_INIT
*      TO ICD_HBT_181_PRD_CATLOG_DBW.

*    MOVE Pz_PROD_EXT_ID                    IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*      TO PROD_EXT_ID                IN ICD_HBT_181_PRD_CATLOG_PRW.
*    MOVE Pz_EXT_ID_NUM_TYPE_CODE    IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*      TO EXT_ID_NUM_TYPE_CODE       IN ICD_HBT_181_PRD_CATLOG_PRW.
*    MOVE 4
*      TO DPz_KEY_SEQUENCE           IN ICD_HBT_181_PRD_CATLOG_DBW.
*    MOVE Lz_OPEN_CURSOR_RDB
*      TO DPz_ACTION                 IN ICD_HBT_181_PRD_CATLOG_DBW.

*    PERFORM C210-OPEN-CLOSE-181-CURSOR.

* prepare fetch parameters
*    MOVE Lz_FETCH_RDB
*      TO DPz_ACTION                 OF ICD_HBT_181_PRD_CATLOG_DBW.

*    SET Lz_START_CURSOR TO TRUE.

*    PERFORM C220-READ-CURSOR-181 UNTIL Lz_END_CURSOR.

* close cursor 181
*    MOVE Lz_CLOSE_CURSOR_RDB
*      TO DPz_ACTION                 IN ICD_HBT_181_PRD_CATLOG_DBW.
*    PERFORM C210-OPEN-CLOSE-181-CURSOR.

*C200-EXIT.
*    EXIT.
*-----------------------------------------------------------------------------
*C210-OPEN-CLOSE-181-CURSOR         SECTION.
*-----------------------------------------------------------------------------
*C210-00.

*    CALL 'ICD_HBT_181_PRD_CATLOG_DBA' USING ICD_HBT_181_PRD_CATLOG_DBW.

*    IF DPz_STATUS                  IN ICD_HBT_181_PRD_CATLOG_DBW
*       NOT = SPz_MSG_NORMAL

*       MOVE ICA_W_0742
*       TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
*       MOVE "181"                   TO Pz_MSG_PARAM(1)
*       MOVE "CURSOR תריגס/תחיתפב"   TO Pz_MSG_PARAM(2)
*       CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
*       PERFORM Z-FINISH
*    END-IF.

*C210-EXIT.
*    EXIT.
*-----------------------------------------------------------------------------
*C220-READ-CURSOR-181                SECTION.
*-----------------------------------------------------------------------------
*C220-00.

*    CALL 'ICD_HBT_181_PRD_CATLOG_DBA' USING ICD_HBT_181_PRD_CATLOG_DBW.

*    EVALUATE DPz_STATUS             IN ICD_HBT_181_PRD_CATLOG_DBW

*        WHEN SPz_MSG_NO_DATA_FOUND
*        WHEN SPz_MSG_END_OF_DATA_SET
*             SET Lz_END_CURSOR TO TRUE
*        WHEN SPz_MSG_NORMAL
*             PERFORM C225-READ-180
*        WHEN OTHER
*             MOVE ICA_W_0742
*             TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
*             MOVE "181"                   TO Pz_MSG_PARAM(1)
*             MOVE "האירקב"                TO Pz_MSG_PARAM(2)
*             CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
*             PERFORM Z-FINISH
*    END-EVALUATE.

*C220-EXIT.
*    EXIT.
*-----------------------------------------------------------------------------
*C225-READ-180                      SECTION.
*-----------------------------------------------------------------------------
*C225-00.

*    MOVE HB_USER_ID                 IN ICD_HBT_181_PRD_CATLOG
*      TO HB_USER_ID                 IN ICD_HBT_180_CST_CATLOG_PRW.
*    MOVE Lz_INQUIRE_RDB
*      TO DPz_ACTION                 OF ICD_HBT_180_CST_CATLOG_DBW.
*    MOVE 0
*      TO DPz_KEY_SEQUENCE           OF ICD_HBT_180_CST_CATLOG_DBW.

*    CALL 'ICD_HBT_180_CST_CATLOG_DBA' USING ICD_HBT_180_CST_CATLOG_DBW.

*    EVALUATE DPz_STATUS               IN ICD_HBT_180_CST_CATLOG_DBW
*        WHEN SPz_MSG_NORMAL
*             IF CO_USER_ID            IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP = SPACES
*                OR
*                HB_STATUS             IN ICD_HBT_180_CST_CATLOG = V4005zACTIV

*                MOVE HB_USER_ID          IN ICD_HBT_180_CST_CATLOG
*                  TO CO_USER_ID          IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*                MOVE LAST_ACTIVITY_DATE  IN ICD_HBT_180_CST_CATLOG
*                  TO LAST_ACTIVITY_DATE  IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*             END-IF
*             IF HB_STATUS                IN ICD_HBT_180_CST_CATLOG = V4005zACTIV
*                MOVE Vz_HEB_YES
*                  TO ACTIVE_FLAG         IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*                MOVE HB_STATUS           IN ICD_HBT_180_CST_CATLOG
*                  TO CO_STATUS           IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*               MOVE OPEN_DATE           IN ICD_HBT_180_CST_CATLOG
*                  TO OPEN_DATE           IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*                PERFORM C230-GET-E-MAIL
*                 SET Lz_END_CURSOR TO TRUE
*             END-IF

*        WHEN SPz_MSG_NO_DATA_FOUND
*             SET Lz_END_CURSOR TO TRUE

*        WHEN OTHER
*             MOVE ICA_W_0742
*             TO   SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
*             MOVE "180"                   TO Pz_MSG_PARAM(1)
*             MOVE "האירקב"                TO Pz_MSG_PARAM(2)
*             CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
*             PERFORM Z-FINISH
*    END-EVALUATE.

*C225-EXIT.
*    EXIT.
*-----------------------------------------------------------------------------
*C230-GET-E-MAIL                     SECTION.
*-----------------------------------------------------------------------------
*C230-00.

*    IF E_MAIL_ADDR_INTR_ID         IN ICD_HBT_180_CST_CATLOG NOT = 0
*       MOVE E_MAIL_ADDR_INTR_ID     IN ICD_HBT_180_CST_CATLOG
*                                    IN ICD_HBT_180_CST_CATLOG_DBW
*         TO E_MAIL_ADDR_INTR_ID     IN ICD_HBT_190_EMAIL_ADDR_PRW
*           INITIALIZE DPz_ACTION    OF ICD_HBT_190_EMAIL_ADDR_DBW

*       MOVE Lz_INQUIRE_RDB
*         TO DPz_ACTION             OF ICD_HBT_190_EMAIL_ADDR_DBW

*       MOVE 0
*         TO DPz_KEY_SEQUENCE       OF ICD_HBT_190_EMAIL_ADDR_DBW

*       CALL 'ICD_HBT_190_EMAIL_ADDR_DBA' USING ICD_HBT_190_EMAIL_ADDR_DBW
*       IF DPz_STATUS               IN ICD_HBT_190_EMAIL_ADDR_DBW
*          = SPz_MSG_NORMAL
*          MOVE E_MAIL_ADDR         IN ICD_HBT_190_EMAIL_ADDR
*            TO E_MAIL_ADDR         IN ICA_CHECK_IF_SUBS_ACTIVE_WKSP
*       ELSE
*          MOVE ICA_W_0742
*           TO  SPz_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW
*          MOVE "190"                   TO Pz_MSG_PARAM(1)
*          MOVE "האירקב"                TO Pz_MSG_PARAM(2)
*          CALL 'ICA_ICF_XLATE_MSG' USING  UTL_CONTROL_ACW
*          PERFORM Z-FINISH
*       END-IF.

*C230-EXIT.
*    EXIT.
* End Change #55557
*-----------------------------------------------------------------------------
Z-FINISH                            SECTION.
*#-----------------------------------------------------------------------------
Z-00.



*# 12-NOV-2012 16:52:13.87 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    GO TO Z-00

    STOP RUN

    EXIT PROGRAM.
Z-EXIT.
    EXIT.
*#-----------------------------------------------------------------------------
