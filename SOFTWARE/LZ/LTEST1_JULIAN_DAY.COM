$ ! LTEST1_JULIAN_DAY.COM      Version 24-Mar-1994
$
$    LINK -
       TEST1_JULIAN_DAY, -
       CNVRT_TO_FJUL, -
       JULDAY, -
       GREGDATE, -
       [GEOTAIL.L1.OBJ]L1_OBJ_LIB/LIBR
$
$    EXIT

