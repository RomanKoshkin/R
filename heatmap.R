library(gplots)
library(reshape)
library(RColorBrewer)

est = "P_mixed" #  "load_main" # 'P_mixed' #  'langXload'
coL_ofinterest = grep(est, colnames(P))
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = 299)

col_breaks = c(seq(0, 0.332, length=100),               # for red
               seq(0.333, 0.666, length=100),           # for yellow
               seq(0.667, 1, length=100))               # for green

Pdata <- melt(P[-c(1) , c(1,2, coL_ofinterest)], id=c("loQ","hiQ"))
o <- round(data.matrix(cast(Pdata, loQ ~ hiQ)), digits = 2)
rownames(o) <- o[, 1]
colnames(o)
o <- o[,-1]
o <- o[nrow(o):1, ] # flip the matrix upside down (for readability)

heatmap.2(o,
          cellnote = o,  # same data set for cell labels
          
          main = "Pvalues", # heat map title
          key = FALSE,
          keysize = 1,
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          #margins =c(0.5,0.5),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Colv=FALSE,            # turn off column clustering
          Rowv = FALSE)