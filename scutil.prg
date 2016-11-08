*****************************************************************
*                                         <<<<<  SCITE >>>>>    
* PROGRAM NAME.: SCUTIL.PRG (COPYRIGHT 1992 by Dennis M. Carroll)
* CALLED FROM..: dBASE SUB MENU OF SCNEW.PRG
* Author.......: DENNIS M. CARROLL   
* Date.........: 20 MAR 96  1800  (# 08 LAST SCFILES CHANGE)
* Notice.......: SYSTEM PACKAGE AVAILABLE - PHONE (518) 283-2146
* Xbase Ver....: dBASE IV ver 1.5 and FoxPro 2
* Description..: SCITE UTILITY MENUS PROCEDURE FILE
*****************************************************************
do scutmenu

PROCEDURE SCUTMENU
mPICK = "1"
DO WHILE mPICK <> '?'
 CLEAR
 @ 1,1 TO 19,73 COLOR B/W
 @ 2,2 FILL TO 18,72 COLOR GR+/B 
 @ 2,5 SAY '∞∞∞∞∞ …ÕÕª …ÕÕ  À  …ÕÀÕª  …ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
 @ 3,5 SAY '∞∞∞∞∞ »ÕÕª ∫    ∫    ∫    ÃÕ  ∞∞∞∞∞∞ UTILITIES ∞'
 @ 4,5 SAY '∞∞∞∞∞ »ÕÕº »ÕÕ            »ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
 @ 5,2 TO 5,72 COLOR B/W
 @ 6,3 SAY '1. TAX MAP SHORT FORM/NO ZERO METH TO SC INDEX STYLE'
 @ 7,3 SAY '2. TAX MAP SHORT FORM TO LONG FORM DOT/DASH & ZEROS'
 @ 8,3 SAY '3. TAX MAP LONG FORM TO SC INDEX STYLE' 
 @ 9,3 SAY '4. SELECT COORDINATE WINDOW TO YOUR FILE'
 @ 10,3 SAY '5. RECORD EACH TIMESHEET RECEIPT'
 @ 11,3 SAY '6. SETUP FOR NEW QUARTER DATA ENTRY TO SCTALYY/DYY'
 @ 12,3 SAY '7. ADD NEW STAFF OR EXTRA STAFFCODE FOR HOURLY RATE'
 @ 13,3 SAY '8. DELETE dBASE TEMPORARY FILES'
 @ 14,3 SAY '9. FIX DATANOs (AND DATUMNOs)'
 @ 15,3 SAY 'ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ'
 @ 15,55 SAY 'ƒƒƒƒƒƒƒƒƒƒ'
 @ 16,3 SAY '(A)SCII TABLE∞(C)OLOR CODES∞(F)UNCTION KEYS∞'
 @ 16,48 SAY '(D)OCUMENT SYSTEM'
 @ 17,3 SAY '(M)ORE∞(R)ETURN TO MAIN MENU∞(Q)UIT TO DOS'
 @ 19,1 TO 21,25 COLOR B/W
 mOPTION = '0'
 @ 20,2 SAY 'YOUR OPTION (hotkey):' GET mOPTION
 READ
 DO CASE
  CASE mOPTION = '1' .OR. mOPTION = '2'
   DO SCTAXNO WITH '&mOPTION'
  CASE mOPTION = '3'
   DO SCTAXNON
  CASE mOPTION = '4'
   DO SCTWIND0
  CASE mOPTION = '5'
   DO TSCHECK
  CASE mOPTION = '6'
   DO RESETQTR 
  CASE mOPTION = '7'
   DO ADDSTAFF
  CASE mOPTION = '8'
   DO SC_DEL_X
  CASE mOPTION = '9'
   DO SC_NO_FIX
  CASE mOPTION = 'A'
   DO ASCII IN SCOGOOD
  CASE mOPTION = 'C'
   DO SCCOLOR IN SCOGOOD
  CASE mOPTION = 'D'
   DO SC_DOCU
  CASE mOPTION = 'F' 
   DO SCFKEY WITH '1'
  CASE mOPTION = 'R'  
   mPICK = '?'
  CASE mOPTION = 'Q'
   CLEAR ALL
   QUIT 
 ENDCASE
ENDDO

PROCEDURE SCTAXNO
PARAMETERS mLFTAXNO
NOTE *---- SHORT FORM TO SC INDEX KEY STYLE OR
NOTE *---- LONG FORM (DASHES & PERIODS) 
&& NAME FILE
GO TOP
DO WHILE .NOT. EOF( )
 IF '/' $TAXNO
  BROWSE
 ENDIF
 IF .NOT. '.' $TAXNO
  SKIP
 ENDIF
 POS = AT('.', TAXNO)
 mMAP = SUBSTR(TAXNO,1,(POS-1))
 DO CASE
  CASE &mMAP <10
   mMAP = "00" + mMAP
  CASE &mMAP > 9 .AND. &mMAP < 100
   mMAP = "0" + mMAP
 ENDCASE
 DO CASE
  CASE SUBSTR(TAXNO,(POS+1),1) = "-"
   DO CASE
    CASE  mLFTAXNO = '2'
     mSUBMAP = '.00'
    OTHERWISE
     mSUBMAP = "000" 
   ENDCASE
  CASE SUBSTR(TAXNO,(POS+2),1) = "-"
   DO CASE
    CASE mLFTAXNO = '2'
     mSUBMAP = '.0' + SUBSTR(TAXNO,(POS+1),1)
    OTHERWISE
     mSUBMAP = "00" + SUBSTR(TAXNO,(POS+1),1)
   ENDCASE
  OTHERWISE
   DO CASE
    CASE mLFTAXNO = '2' 
     mSUBMAP = '.' + SUBSTR(TAXNO,(POS+1),2)
    OTHERWISE
     mSUBMAP = "0" + SUBSTR(TAXNO,(POS+1),2)
   ENDCASE
 ENDCASE
 POS = AT('-',TAXNO)
 mBLOCK = SUBSTR(TAXNO,(POS+1),4)
 DO CASE
  CASE SUBSTR(TAXNO,(POS+2),1) = "-"
   mBLOCK = "000" + SUBSTR(TAXNO,(POS+1),1)
  CASE SUBSTR(TAXNO,(POS+3),1) = "-"
   mBLOCK = "00" + SUBSTR(TAXNO,(POS+1),2)
  CASE SUBSTR(TAXNO,(POS+4),1) = "-"
   mBLOCK = "0" + SUBSTR(TAXNO,(POS+1),3)
 ENDCASE
 RNO = SUBSTR(TAXNO,(POS+1))  
 POS = AT('-',RNO)
 mLOT = SUBSTR(RNO,(POS+1),2)
 TPOS = POS
 DO CASE
  CASE  SUBSTR(RNO,(POS+2),1) = "."
   mLOT = "0" + SUBSTR(RNO,(POS+1),1)
   TPOS = POS-1 
  CASE  SUBSTR(RNO,(POS+2),1) = " "
   mLOT = "0" + SUBSTR(RNO,(POS+1),1)
   TPOS = POS-1
 ENDCASE
 POS = TPOS
 mSUBLOT = SUBSTR(RNO,(POS+4),3)
 DO CASE
  CASE SUBSTR(RNO,(POS+3),1) = " "
   mSUBLOT = "000"
  CASE SUBSTR(RNO,(POS+5),1) = " "
   mSUBLOT = "00" + SUBSTR(RNO,(POS+4),1)
  CASE SUBSTR(RNO,(POS+6),1) = " " 
   mSUBLOT = "0" + SUBSTR(RNO,(POS+4),2)
 ENDCASE
 IF mLFTAXNO = '2'
  TTAXNO = "&mMAP" + "&mSUBMAP" + "-" + "&mBLOCK" + "-" + "&mLOT";
   + "." + "&mSUBLOT"
 ELSE
  TTAXNO = "&mMAP" + "&mSUBMAP" + "&mBLOCK" + "&mLOT" + "&mSUBLOT"
 ENDIF
 REPLACE TAXNO WITH "&TTAXNO"
 SKIP
ENDDO
&&DO SCMMENU    

PROCEDURE SCTAXNON
&&LONG FORM W DASHES & PERIODS TO COMPUTER INDEX STYLE
DO WHILE .NOT. EOF()
 POS = AT('.',TAXNO)
 mSZERO = STUFF(TAXNO,POS,1,'0')
 REPLACE TAXNO WITH "&mSZERO"
 POS = AT('-',TAXNO)
 mSREMOVE = STUFF(TAXNO,POS,1,'')
 REPLACE TAXNO WITH "&mSREMOVE"
 POS = AT('-',TAXNO)
 mSREMOVE = STUFF(TAXNO,POS,1,'')
 REPLACE TAXNO WITH "&mSREMOVE"
 POS = AT('.',TAXNO)
 mSZERO = STUFF(TAXNO,POS,1,'')
 REPLACE TAXNO WITH "&mSZERO"
 SKIP
ENDDO
&&DO SCMMENU

PROCEDURE SCTWIND0
CLEAR
ACCEPT 'NAME SENDING FILE : ' TO mSFILE
ACCEPT 'NAME RECEIVING FILE: ' TO mRFILE
INPUT  '1. SOUTHERNMOST S_N_NYCOOR: ' TO mSMCOORD  
INPUT  '2. NORTHERNMOST S_N_NYCOOR: ' TO mNMCOORD
INPUT  '3. WESTERNMOST  S_E_NYCOOR: ' TO mWMCOORD 
INPUT  '4. EASTERNMOST  S_E_NYCOOR: ' TO mEMCOORD
? mSFILE
USE (mSFILE)
? mSMCOORD
WAIT
COPY TO &mRFILE FOR S_N_NYCOOR > mSMCOORD .AND. S_N_NYCOOR < mNMCOORD;
 .AND. S_E_NYCOOR > mWMCOORD .AND. S_E_NYCOOR < mEMCOORD
WAIT

PROCEDURE ADDSTAFF
&& This procedure will permit addition of new staff or creation of
&& an additional staffcode for those persons receive increments
&&   during the quarter  
USE EHSTAFF ORDER EHSTAFF
BROWSE
DO DUPREC
BROWSE
COPY TO TEMP
SORT ON LASTNAME,FIRSTNAME TO SORTED
USE
ERASE EHSTAFF.DBF
RENAME SORTED.DBF TO EHSTAFF.DBF
USE EHSTAFF 
INDEX ON LASTNAME + FIRSTNAME TAG EHSTAFF
USE EHSTAFF ORDER EHSTAFF
BROWSE
ACCEPT "IS EVERYTHING OK?(Y/N): " TO mOK 
IF UPPER(mOK) = "Y"
 ERASE TEMP.DBF
ENDIF
DO SCMMENU

PROCEDURE RESETQTR
DO SCMTHYR
IF mMTH = "01"
 mYRE = VAL(mYR) - 1
 mYR = STR(mYRE,2)
ENDIF
USE "EHLOG" + mYR
ZAP
CLEAR
TEXT
  ADD DUMMY RECORD
ENDTEXT
APPEND
USE "EHDAY" + mYR
ZAP
TEXT
  ADD DUMMY RECORD
ENDTEXT
APPEND
DO SCMMENU

PROCEDURE SC_DEL_X
! DIR *.BAK > SCRUB.TXT
! DIR *.?BK >> SCRUB.TXT
! DIR TEMP*.* >> SCRUB.TXT  
! DIR *.$* >> SCRUB.TXT
! TYPE SCRUB.TXT | MORE
ACCEPT "DO YOU WISH TO RUB OUT ALL SUCH FILES (Y/N)? " TO mYN
IF UPPER(mYN) = 'Y'
 RUN DEL *.DBK
 RUN DEL *.MBK
 RUN DEL *.TBK
 RUN DEL *.BAK
 RUN DEL TEMP*.*
 && RUN DEL *.$*
ENDIF
ERASE SCRUB.TXT


PROCEDURE SCMSTUFF
&&--------   USE SCTAX ORDER SCTAX IN 1
USE SCXMEMO IN 2
SELECT 1
&& seek 'ggg"
&& SEEK 'TAX'
mCOUNT = 1
mSTUFF = MEMLINES(DATATEXT)
DO WHILE mCOUNT < mSTUFF + 1
 SET MEMOWIDTH TO 170 
 mADDLINE = MLINE(DATATEXT,mCOUNT)
 SELECT 2
 IF mCOUNT = 1
  ZAP
  APPEND BLANK
  SET MEMOWIDTH TO 170
  REPLACE TEMPMEMO WITH mADDLINE + CHR(13)
 ELSE
  DO CASE 
   CASE mCOUNT = 28
    REPLACE TEMPMEMO WITH SUBSTR(mADDLINE,1,80) + mQGAL + CHR(13) ADDITIVE
   && REPLACE TEMPMEMO WITH SUBSTR(mADDLINE,1,80) + " 62800" + CHR(13) ADDITIVE
   && REPLACE TEMPMEMO WITH SUBSTR(mADDLINE,1,57) + " $ 2300" + CHR(13) ADDITIVE
   OTHERWISE
    REPLACE TEMPMEMO WITH mADDLINE + CHR(13) ADDITIVE
  ENDCASE  
 ENDIF
 mCOUNT = mCOUNT + 1
 SELECT 1
ENDDO
SELECT 2
COPY MEMO TEMPMEMO TO TEMP.TXT
SELECT 1
APPEND MEMO DATATEXT FROM TEMP.TXT OVERWRITE
BROWSE
DO SCREAM

PROCEDURE SCWSSIGN
*** SETS UP FOR SAMPLE SIGNING
&&CLEAR ALL
CLEAR
TEXT 
**************************************************************
**************** P R I N T    M E N U ************************
****** SAMPLE SIGNING STATEMENTS - YOUR PICK *****************
**                                                          ** 
**  1.   SATISFACTORY STATEMENT - FINISHED WATER            **
**  2.   UNSATISFACTORY STATEMENT - FINISHED WATER          **
**  3.   SATISFACTORY STATEMENT - RAW WATER                 **
**  4.   UNSATISFACTORY STATEMENT - RAW WATER               **
**                                                          **
**      (R)ETURN TO MAIN MENU    (Q)UIT TO DOS              **  
**************************************************************
**************************************************************
ENDTEXT
ACCEPT "SELECT YOUR OPTION (1,2,3,4, OR 5): " TO mPROPT
IF mPROPT $'1,2,3,4,'
 USE SCTSTAFF ORDER SCTSTAFF
 BROWSE
 mSIGNER = TRIM(FIRSTNAME) + " " + MIDDLEINIT + " ";
 + TRIM(LASTNAME) + ", " + TITLE
 USE SCWSSIGN 
 LOCATE FOR SIGNTYPE = "&mPROPT"
   mFIELD1 = SIGN1
   mFIELD2 = SIGN2
   mFIELD3 = SIGN3
   mFIELD4 = SIGN4
   mFIELD5 = SIGN5 
ELSE 
 IF mPROPT = 'R'
  DO SCREAM
 ELSE
  CLEAR ALL
  QUIT
 ENDIF
ENDIF
GO TOP
INPUT "ENTER A LINECOUNT TO START: " TO LINECNT
mULINE = REPLICATE('_',70)
mTBLINE = REPLICATE('_',81)
CLEAR
TOPRINT = " "
@ 2,5 SAY "SEND REPORT TO PRINTER? (Y/N) " ;
 GET TOPRINT PICTURE "!"
 READ
IF UPPER(TOPRINT) = "Y"
 CLEAR
TEXT
*************************************************************
************** O P T I O N S - S E L E C T ******************
*************************************************************
**                                                         **
**   1. NO REPEAT                                          **
**   2. SPECIFY                                            **
**   3. PROMPT REPEAT FOR SINGLE SHEET FEED                **
**   4. QUIT                                               ** 
**                                                         **
*************************************************************
*************************************************************
ENDTEXT
INPUT "ENTER YOUR OPTION CHOICE (1,2, OR 3): " TO mSTYLE
IF MSTYLE = 2
KOUNT = 1
INPUT "ENTER # OF COPIES: " TO mSPEC
ENDIF     
SET DEVICE TO PRINT
PRINTJOB
_pquality = .T.
_ppitch = "condensed"
ENDPRINTJOB
ENDIF
mYN = "Y" 
DO WHILE UPPER(mYN) = "Y"
 @ LINECNT,8 SAY "&mTBLINE"
 @ LINECNT + 2,7 SAY "  " +mFIELD1 + "  "
 @ LINECNT + 3,7 SAY "  " +mFIELD2 + "  "
 @ LINECNT + 4,7 SAY "  " +mFIELD3 + "  "
 @ LINECNT + 5,7 SAY "  " +mFIELD4 + "  "
 @ LINECNT + 6,7 SAY "  " +mFIELD5 + "  "
 @ LINECNT + 7,8 SAY "&mTBLINE"
 @ LINECNT + 12,8 SAY "&mULINE" 
 @ LINECNT + 13,8 SAY "&mSIGNER"
 @ LINECNT + 13,65 SAY + DMY(DATE())
 EJECT
 DO CASE 
  CASE mSTYLE = 1
   mYN = "N"
  CASE mSTYLE = 2
   KOUNT = KOUNT + 1
   IF KOUNT = mSPEC + 1
    mYN = "N"
   ELSE 
    mYN = "Y"
   ENDIF
  CASE mSTYLE = 3
   ACCEPT "DO ANOTHER WITH SAME SIGNER (Y/N): " TO mYN
  CASE mSTYLE = 4
   @ 10,2 SAY 'NOT AVAILABLE'
   DO SCMMENU
 ENDCASE
ENDDO
SET DEVICE TO SCREEN

PROCEDURE SC_NO_FIX
GO TOP
DO WHILE .NOT. EOF()
 mYRMD = LTRIM(STR(99991332 - VAL(DTOS(DATADATE)))) 
 mDATANO = TRIM(SITENO) + "W" + "&mYRMD" + DATA_ID
 REPLACE DATANO WITH "&mDATANO"
 && REPLACE DATUMNO WITH DATANO + DATUM_NO  &&* ----- USING SCLOAD
 SKIP
ENDDO

PROCEDURE SC_EDEX
SET SAFETY OFF
SET HEADINGS OFF
CLEAR
@ 1,1 TO 8,73 COLOR B/W
@ 2,2 FILL TO 7,72 COLOR GR+/B 
@ 2 ,3 SAY ' 1. MAIN RESUME'
@ 3, 3 SAY ' 2. EDUCATION TABLE' 
@ 4, 3 SAY ' 3. A CODESTRING TABLE'
@ 5, 3 SAY ' 4. JOB SEARCH TABLE'
@ 7, 3 SAY '  (R)ETURN TO MAIN MENU  (Q)UIT TO DOS'
@ 10,1 TO 12,25 COLOR B/W
mEDEX = '0'
@ 11,2 SAY 'YOUR OPTION (hotkey):' GET mEDEX
READ
DO CASE
 CASE mEDEX = '1'
  GO TOP
  SEEK 'ZRESZ'
  LIST OFF WHEN+ITEM WHILE ITEM_DATE = 'ZRES' TO FILE SC_EDEX1.TXT
  CLEAR
  GO TOP
  SEEK 'XRES'
  LIST OFF WHEN+VERT_LINE+ITEM WHILE 'RES' $ITEM_DATE TO FILE SC_EDEX2.TXT
  && *-------------- use DOS additive file copy
  ! COPY SC_EDEX1.TXT + SC_EDEX2.TXT SC_EDEX.TXT
  ERASE SC_EDEX1.TXT
  ERASE SC_EDEX2.TXT
  MODIFY COMMAND SC_EDEX.TXT
 CASE mEDEX = '2' 
  GO TOP
  SEEK 'AA'
  LIST OFF "          " +VERT_LINE+WHEN+ITEM+WHERE+VERT_LINE;
   WHILE ITEM_DATE = 'AA' TO FILE SC_EDEX3.TXT
  MODIFY COMMAND SC_EDEX3.TXT
 CASE mEDEX = '3'
  CLEAR
  @ 1,1 TO 19,73 COLOR B/W
  @ 2,2 FILL TO 18,72 COLOR GR+/B 
  @ 2,5 SAY '∞∞∞∞∞ …ÕÕÕª  …ÕÕ  À  …ÕÀÕª  …ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
  @ 3,5 SAY '∞∞∞∞∞ »ÕÕÕª  ∫    ∫    ∫    ÃÕ  ∞∞CODE STRINGS∞∞∞∞'
  @ 4,5 SAY '∞∞∞∞∞ »ÕÕÕº  »ÕÕ            »ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
  @ 5,2 TO 5,72 COLOR B/W
  @ 6 ,3 SAY ' (C)OMPUTER EDUCATION / EXPERIENCE'
  @ 7 ,3 SAY ' (P)E EXAM EDUCATION / EXPERIENCE' 
  @ 8, 3 SAY ' (A)LL EDUCATION / EXPERIENCE' 
  @ 9, 3 SAY ' SINGLE CODESTRING ENTER CHARACTER'
  @ 15,3 SAY 'ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ'
  @ 17,3 SAY '∞∞∞∞(R)ETURN TO MAIN MENU∞∞∞∞∞∞∞∞∞'
  @ 19,1 TO 21,25 COLOR B/W
  mCODE = '3'
  @ 20,2 SAY 'CODESTRING (hotkey):' GET mCODE
  READ
  LIST OFF ' '+VERT_LINE+' '+WHEN+' '+VERT_LINE+' '+ITEM+' '+VERT_LINE+;
  ' '+WHERE+VERT_LINE FOR '&mCODE' $CODESTRING TO FILE SC_EDEX4.TXT
  MODIFY COMMAND SC_EDEX4.TXT
 CASE mEDEX = '4'  
  _plength = 60
  REPORT FORM SCJOBTBL TO FILE SCJOBTBL.TXT
  MODIFY COMMAND SCJOBTBL.TXT
ENDCASE 
SET HEADINGS ON 
SET SAFETY ON
RELEASE mCODE

PROCEDURE SC_DOCU
! TYPE '          CONFIG.SYS      ' >TEMP.TXT
! TYPE \CONFIG.SYS >TEMP.TXT
! TYPE \DBASE\SCBLANK.TXT >>TEMP.TXT 
! TYPE \AUTOEXEC.BAT >>TEMP.TXT
! TYPE \DBASE\SCBLANK.TXT >>TEMP.TXT 
! TYPE \DBASE\CONFIG.DB >>TEMP.TXT
USE SCFILES
GO TOP
LOCATE FOR MARK
mFILE = MAINFILE
USE (mFILE)
LIST STRUCTURE TO TEMP1.TXT
! COPY TEMP.TXT+SCBLANK.TXT+TEMP1.TXT TEMP.DOC
MODIFY COMMAND TEMP.DOC
USE

PROCEDURE SCOMEMO
PARAMETERS mFILE     && TRY 2ND PARAMETER LATER mRECNUM
&&Do WHILE mTRYAGAIN = 'Y'
SET SAFETY OFF
CLEAR
@ 1,1 TO 15,73 COLOR B/W
@ 2,2 FILL TO 14,72 COLOR GR+/B 
@ 2,5 SAY '∞∞∞∞∞ …ÕÕÕª  …ÕÕ  À  …ÕÀÕª  …ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
@ 3,5 SAY '∞∞∞∞∞ »ÕÕÕª  ∫    ∫    ∫    ÃÕ  ∞∞ MEMO HANDLER ∞∞'
@ 4,5 SAY '∞∞∞∞∞ »ÕÕÕº  »ÕÕ            »ÕÕ ∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞'
@ 5,2 TO 5,72 COLOR B/W
@ 6 ,3 SAY ' 1. OPTIMIZE DATABASE ASSOCIATED MEMOS'
@ 7 ,3 SAY ' 2. COPY MEMOS TO MEMO REFERENCE DISKETTE'
@ 8, 3 SAY ' 3. PULL ASSOCIATED MEMOS FROM REFERENCE DISKETTE' 
@ 9, 3 SAY ' 4. REMOVE HARD DISK MEMO FILE ONLY'
@ 10,3 SAY ' 5. TRANSFER DBASE<-->FOXPRO DBF & DBT(MEMO) FILES'
@ 11,3 SAY '    --------------------------------------------'
@ 12,3 SAY '        (R)ETURN TO MAIN MENU' 
@ 14,1 TO 16,25 COLOR B/W
mMEMO = '0'
@ 15,2 SAY 'YOUR OPTION (hotkey):' GET mMEMO
READ
IF mMEMO <> 'R'
 SET FULLPATH ON
 mDRIVE = SUBSTR(DBF(),1,2) 
 POS = AT('\', DBF())
 mDIRECT = TRIM(SUBSTR(DBF(),POS+1,12))
 IF '\' $mDIRECT
  POS = AT('\', mDIRECT)
  mDIRECT = SUBSTR(mDIRECT,1,POS-1)
 ENDIF
 mKEY = KEY(1)
 ? mKEY
 WAIT
 mPATH = mDRIVE + '\' + mDIRECT +'\' + mFILE
 DO CASE
  CASE FILE("&mPATH" + ".DBT") .OR. FILE("&mPATH" + ".FPT") 
   CLEAR
   @12,5 SAY "Optimizing memo field(s) - your SCITE is: " + mSCITE 
   @15,5 SAY "Your FILE is:   " + mPATH  
   @17,5 SAY "Your FILE INDEX is:   " + mKEY  
   TEXT
     Please wait ....    Your FILE is also being reindexed
   ENDTEXT
   mGOTOIT = 'Y'
   IF mMEMO <> '4' 
    mKEY = KEY(1)
    COPY TO mDRIVE + '\' + mDIRECT + '\' + 'TEMP'
    USE
    ERASE (mPATH) + '.DBF'
    IF mSCITE $'03,04,05,06,07,08'
     ERASE (mPATH) + '.FPT'
    ELSE 
     ERASE (mPATH) + '.DBT'
    ENDIF 
    mRN =  mDRIVE + '\' + mDIRECT + '\' + 'TEMP'
    RENAME (mRN) + '.DBF' TO (mPATH) + '.DBF'
    IF mSCITE $'03,04,05,06,07,08'
     RENAME (mRN) + '.FPT' TO (mPATH) + '.FPT'
    ELSE 
     RENAME (mRN) + '.DBT' TO (mPATH) + '.DBT'
    ENDIF
    IF "\" $mFILE  && checks for subdirectory file location
     POS = AT('\', mFILE)
     mFILE = TRIM(SUBSTR(mFILE,POS+1,8))
    ENDIF
    USE (mPATH)
    INDEX ON &mKEY TAG (mFILE)
   ENDIF
  OTHERWISE
   DO CASE
    CASE mMEMO = '3'
     mGOTOIT = 'X'
    CASE mMEMO $'1,2,4,5'
     mGOTOIT = 'N'
     @ 13,6 TO 15,45 COLOR B/W
     @ 14,8 SAY 'THIS DATABASE DOES NOT HAVE A MEMO FIELD'
     mBACKUP = 'Y' 
     @ 18,10 SAY 'BACKUP .dbf ONLY (Y/N) ?' GET mBACKUP
     READ
     IF mBACKUP = 'Y'
      mTACK =  'mFILE' + '&mFNO' 
      mBUPF = mTACK 
      DO DISK_IN 
      DO DRV_NAME
      CLOSE DATABASES
      DO CASE
       CASE mSCITE = '05'
        DO CASE
         CASE mDRV = 'A' 
          @ 11,2 SAY 'COPY D:' + &mBUPF + '.DBF TO A:'
         OTHERWISE
          @ 11,2 SAY 'COPY D:' + &mBUPF + '.DBF TO F:'
        ENDCASE
        WAIT  
        COPY FILE "D:" + &mBUPF + ".DBF" TO (mDRV) + ":" + &mBUPF + ".DBF" 
       OTHERWISE
        @ 11,2 SAY 'COPY C:' + &mBUPF + '.DBF TO A:'
        COPY FILE "C:" + &mBUPF + ".DBF" TO (mDRV) + ":" + &mBUPF + ".DBF" 
        WAIT
      ENDCASE
     ENDIF
   ENDCASE
 ENDCASE 
 && RUN DIR &mPATH.* /P
 DO CASE
  CASE mMEMO $'2,3,4,5' .AND. mGOTOIT $'X,Y' 
   CLOSE DATABASES
   IF mMEMO $'2,4'
    mMEMODEL = 'Y'
   ELSE 
    mMEMODEL = 'N'  
   ENDIF
   DO CASE
    CASE mMEMO $'2,3,5' 
     mTACK =  'mFILE' 
     mBUPF = mTACK 
     DO DISK_IN
     DO DRV_NAME
     DO CASE 
      CASE mMEMO $'2,5'
       IF mMEMO = '5'
        DO CASE
         CASE mSCITE $'01,02,0A'
          @ 8,2 SAY 'DBASE IV TO DBASE 3+ MEMO = FOXPLUS MEMO'
          @ 9,2 SAY 'COPY C:' + &mBUPF + '.DBF/.DBT TO A:'
          USE (mFILE)
          COPY TO "A:" + mFILE TYPE DBMEMO3
         CASE mSCITE $'03,04,05,06,07,08'
          @ 8,2 SAY 'FOXPRO TO FOXPLUS MEMO = dBASE 3+ MEMO:'
          IF mSCITE $'05,08'
           mMULTI_TRY = 'N'
           @ 15,2 SAY 'SPECIFY RECORD COUNT ATTEMPT (Y/N) ? :' GET mMULTI_TRY
           READ    
           DO CASE
            CASE mDRV = 'A'
             @ 11,2 SAY 'COPY ' +mPATH + '.DBF/.DBT TO A:'
             wait  
            OTHERWISE
             @ 11,2 SAY 'COPY ' +mPATH + '.DBF/.DBT TO F:'
             WAIT
           ENDCASE
          ELSE
           mDRV = 'A'
             @ 9,2 SAY 'COPY ' +mPATH + '.DBF/.DBT TO A:'
             && @ 9,2 SAY 'COPY C:' + &mBUPF + '.DBF/.DBT TO A:'   old
          ENDIF
          USE (mFILE)
          IF mMULTI_TRY = 'Y'
           mRECX = 0
           @ 18, 2 SAY 'HOW MANY' GET mRECX
           READ
          ELSE
          ENDIF
          SET TALK ON
          SET ODOMETER TO 1
          IF mMULTI_TRY = 'N'
           COPY TO (mDRV) +":" + mFILE TYPE FOXPLUS 
           && ---dBASE does not like above on-compile
          ENDIF
          mPASSCNT = 1
           DO WHILE .NOT. EOF()
            IF mPASSCNT = 1
             GO TOP
            ELSE
             &&SKIP
             mRECNO = RECNO() 
             GO mRECNO 
            ENDIF   
            CLEAR
            DO DISK_IN
             &&  erase (mDRV) +":" + mFILE + ".DBT" 
             &&  erase (mDRV) +":" + mFILE + ".DBF" 
             &&  wait 'ERASING ...'
            SET ODOMETER TO 1
            COPY NEXT mRECX TO (mDRV) +":" + mFILE TYPE FOXPLUS
&&COPY NEXT 20 TO F:SC_T_A.D01 TYPE FOXPLUS
&&SKIP
&&COPY NEXT 30 TO F:SC_T_A.D02 TYPE FOXPLUS
            SET ODOMETER TO 100
            SET TALK OFF
            mPASSCNT = mPASSCNT + 1
            IF .NOT. EOF()
             SKIP
            ENDIF
            IF EOF()
             CLEAR
             @ 3,4 SAY 'YOU ARE DONE'
             wait
            ELSE
             CLEAR
             @ 3,4 SAY 'HOW MANY THIS TIME (Y/N)? : ' GET mRECX
             READ  
            ENDIF
           ENDDO
        ENDCASE 
       ELSE
        @ 9,2 SAY 'TRANSFER C:' + &mBUPF + '.DBT TO A:' + &mBUPF + '.DBT'
        COPY FILE "C:" + &mBUPF + ".DBT" TO "A:" + &mBUPF + ".DBT" 
        @ 11,2 SAY 'REMOVE HARD DISK MEMOS (Y/N): ' GET mMEMODEL
        READ
       ENDIF
      OTHERWISE
       @ 9,2 SAY 'TRANSFER A:' + &mBUPF + '.DBT TO C:' + &mBUPF + '.DBT'
       IF mGOTOIT = 'Y' 
        ERASE 'C:' +&mBUPF + '.DBT'
       ENDIF
       COPY FILE "A:" + &mBUPF + ".DBT" TO "C:" + &mBUPF + ".DBT" 
     ENDCASE
   ENDCASE
   IF UPPER(mMEMODEL) = 'Y'
    ERASE (mFILE) + '.DBT'
   ENDIF  
 ENDCASE
 IF mGOTOIT <> 'Y'
  mDIR = "DIR " + "&mDRV" + ":*.*/P"
  RUN &mDIR
  WAIT  
 ENDIF
ENDIF

*----------- next statement installed 23 Jul 97 based on reassigned mvar 
USE (mFILE) ORDER (mFILE)

&&mTRYAGAIN = 'N'
&&enddo


 
PROCEDURE SCSTUFF
CLEAR
TEXT 
 WHICH STRING STUFF OPTION IS APPLICABLE
    1. DATUMNO -INSERT '01' TO REPLACE '1' IN 'WY1' RESULTING IN 'WY01'
    2. DATUMNO -INSERT 'WG' TO REPLACE 'W' AND 'WS' TO REPLACE 'S'
    3. LINENO - INSERT 'G' AT 1ST LINENO CHARACTER 
ENDTEXT
ACCEPT "WHICH KIND OF STUFF? :"  TO mKIND
DO CASE
 CASE mKIND = '1'
  SET FILTER TO "WY1" $DATUMNO
  GO TOP
  DO WHILE .NOT. EOF()
   mSTUFF = STUFF(DATUMNO,18,1,'01')
   REPLACE DATUMNO WITH "&mSTUFF"
   SKIP
  ENDDO
 CASE mKIND = '2'
  USE TEMP
  MODIFY STRUCTURE
  GO TOP
  DO WHILE .NOT. EOF()
   DO CASE
    CASE SUBSTR(DATUMNO,16,2) = 'W'
     mNEW = STUFF(DATUMNO,17,0,'G')
    CASE SUBSTR(DATUMNO,16,2) = 'S'
     mNEW = STUFF(DATUMNO,16,1,'WS')
   ENDCASE
   REPLACE DATUMNO WITH '&mNEW'
   IF .NOT. EOF()
    SKIP
   ENDIF
  ENDDO
 CASE mKIND = '3'
  USE SCGRANT
  GO TOP
  DO WHILE .NOT. EOF()
   mNEW = STUFF(LINENO,1,1,'G')
   REPLACE LINENO WITH '&mNEW'
   IF .NOT. EOF()
    SKIP
   ENDIF
  ENDDO
ENDCASE
browse
RETURN 

PROCEDURE SCPLINE
SET TALK OFF
SET SAFETY OFF
USE SCMANUAL ORDER SCMANUAL
COPY TO SORTED
USE
ERASE SCMANUAL.DBF
ERASE SCMANUAL.MDX
RENAME SORTED.DBF TO SCMANUAL.DBF
USE SCMANUAL
INDEX ON LINENO TAG SCMANUAL
USE
SET ALTERNATE TO TEMP.TXT
USE SCMANUAL ORDER SCMANUAL
GO TOP
SET ALTERNATE ON
DO WHILE .NOT. EOF()
 &&RECNO() <= 1401
 ? '          ' + LINE
 IF 'Page' $LINE
  ? ' '
 ENDIF
 SKIP
ENDDO
SET ALTERNATE OFF
SET TALK ON
SET SAFETY ON

PROCEDURE SCFKSET && look of calling program may be out of date; then delete
DO SCFKEY
RETURN

FUNCTION FLDTXT
 SET TALK OFF
 SET ALTERNATE TO TEMP.TXT
 SET ALTERNATE ON
 mFLDCNT = 1
 mSTRCHK = FIELD(mFLDCNT) 
 ? '---- FIELD ----',;
   '  ----------- Contents -----------', CHR(13) + CHR(10)
 DO WHILE mSTRCHK <> " "
 ??  '--',FIELD(mFLDCNT),':  ', EVAL(FIELD(mFLDCNT)), CHR(13) + CHR(10)
 mFLDCNT = mFLDCNT + 1
 mSTRCHK = FIELD(mFLDCNT) 
 IF LEN(mSTRCHK) < 1  
   EXIT
  ENDIF
 ENDDO
 SET TALK ON
 CLOSE ALTERNATE
 SET ALTERNATE OFF
 APPEND MEMO DATATEXT FROM TEMP.TXT
 messagebox("Field Table Structure and Field Attribute Data has been appended ... Erasing the temporary file TEMP.TXT now")
 delete file "c:\sc\temp.txt"
 RETURN
 &&   '--',FIELD(2),':  ', EVAL(FIELD(2)), CHR(13) + CHR(10),;
 &&   '--',FIELD(3),':  ', EVAL(FIELD(3)), CHR(13) + CHR(10),;
 &&   '--',FIELD(4),':  ', EVAL(FIELD(4)), CHR(13) + CHR(10),;
 &&   '--',FIELD(5),':  ', EVAL(FIELD(5)), CHR(13) + CHR(10),;
 &&   '--',FIELD(6),':  ', EVAL(FIELD(6)), CHR(13) + CHR(10)
ENDFUNC

FUNCTION DISK_IN
CLEAR
@ 3,0 TO 15,50 COLOR B/W
@ 5,2 SAY 'INSERT APPROPRIATE TRANSFER OR BACKUP DISKETTE'
WAIT 'PRESS (ESC) TO CANCEL - ANY OTHER KEY TO START TRANSFER'

FUNCTION DRV_NAME
mDRV = 'A'
CLEAR
@ 9,2 SAY 'SPECIFY TARGET DRIVE LETTER (A OR F): ' GET mDRV
READ
RETURN mDRV