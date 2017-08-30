DATA LIST FILE= "df2.txt"  free (",")
/ subj lang CLred tmplab mu_N1  .

VARIABLE LABELS
subj "subj" 
 lang "lang" 
 CLred "CLred" 
 tmplab "tmplab" 
 mu_N1 "mu_N1" 
 .

VALUE LABELS
/
subj 
1 "BUL" 
 2 "ELT" 
 3 "GRU" 
 4 "KOS" 
 5 "KOZ" 
 6 "POG" 
 7 "ROM" 
 8 "SHE" 
/
lang 
1 "er" 
 2 "RE" 
/
tmplab 
1 "high" 
 2 "low" 
 3 "mid" 
.

EXECUTE.
