library (gdata)
library(plyr)
suppressWarnings(suppressMessages(library(gdata)))
cat("\014")
load("arrows.RData")
# read in the xls file:
# df <- data.frame(matrix(ncol = 18, nrow = 0))
colNameVec <- c("onset","offset","Word",
                "CWnored","CWred",
                "secon","secoff","SYL",
                "CLonNoRed", "CLoffNoRed", 
                "CLonRED", "CLoffRED",
                "CLonTot", "CLoffTot", 
                "subject", "text","textNo","lang")
colnames(df) <- colNameVec
subjects = c("GRU", "KOZ", "KOS", "POG", "SHE", "BUL", "ROM", "ELT")
total <- length(subjects)*8
i = 1
pb <- txtProgressBar(min = 0, max = total, style = 3)  
  
for (subject in subjects) {
  filepath = paste("/Users/RomanKoshkin/Documents/MATLAB/EEG/", subject, "_EEG/", sep = "")
  filename <- paste(subject, "_arrows EEG_2.xls", sep = "")
  shNames <- sheetNames(paste(filepath, filename, sep=""), verbose = FALSE, perl = "perl")
  if (subject=="GRU"){lim=4}
  else {lim=8}
  for (sh in c(1:lim)){
    ptm <- proc.time()
    tmp_df <- read.xls(
      paste(filepath, filename, sep=""), sheet=sh, perl = "perl", 
      encoding="UTF-8", verbose = FALSE, header = FALSE)
    
    # keep only the needed columns in the order specified:
    keeps <- c("V25", "V26", "V27", "V17", "V28", "V29", "V30", "V35", "V37", "V38", "V39", "V40", "V42", "V43")
    tmp_df <- tmp_df[keeps]
    names(tmp_df)[1:14] <- c("onset", "offset", "Word",
                             "CWnored","CWred", "secon", "secoff", "SYL", 
                             "CLonNoRed", "CLoffNoRed", "CLonRED", "CLoffRED",
                             "CLonTot", "CLoffTot")
    
    tmp_df$subject = subject
    curShName <- strsplit(shNames[sh], "#", perl = FALSE)[[1]][1]
    tmp_df$text = curShName
    tmp_df$textNo = strsplit(shNames[sh], "#", perl = FALSE)[[1]][2]
    languages <- c("re", "ER", "re", "ER", "re", "ER", "re", "ER")
    msk <- apply(sapply(curShName, grepl, c("France", "Morocco", "Honduras", "Colombia", "Peru", "CostaRica", "Uruguay", "Chile")), 1, all)
    tmp_df$lang = languages[msk]
    df <- rbind(df, tmp_df)
    # update progress bar
    i = i + 1
    setTxtProgressBar(pb, i)
    proc.time() - ptm
  }
}
close(pb)
df <- df[- which(is.na(df$SYL)), ] # delete completely empty rows
rm(keeps, languages, ptm, sh, shNames, msk, filepath, filename, 
   tmp_df, colNameVec, curShName, i, pb, subjects, subject, total)

####################################################################################
# subjects = c("GRU", "KOZ", "KOS", "POG", "SHE", "BUL", "ROM", "ELT")
# df2 <- df
# cols <- c("CLonNoRed", "CLonRED")
# for (s in subjects){
# e <- sum(df[df$lang=="ER" & df$subject==s, cols], na.rm = TRUE)
# r <- sum(df[df$lang=="re" & df$subject==s, cols], na.rm = TRUE)
# print(paste(round(e,digits = 0),round(r,digits = 0)))
# df2[df2$lang=="re" & df2$subject==s, cols] <- df2[df2$lang=="re" & df2$subject==s, cols] * e/r
# }