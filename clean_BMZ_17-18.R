library(readxl)
library(tidyverse)
indir <- "/Users/farhad/Documents/OneDrive/PhD/Research Documents/Data/Field Book 2017-18/BMZLDH_2017"

xl <- list.files(path = indir, pattern = "*.xlsx$", full.names = TRUE)

getPhenotypeData <- function(i, xl){
  ff <- xl[i]
  ff <- as.data.frame(read_xlsx(ff, sheet = 3, skip = 7)) 
  
  # Select specific column 
  ff <- ff[,c(8:ncol(ff))]
  
  # Yellow Rust is missing for Trial 3, impute with NA
  if (i == 3){
    ff$`Yellow Rust%` <- NA
  }
  colnames(ff) <- str_replace_all(colnames(ff), fixed(" "),"")
  colnames(ff) <- str_replace_all(colnames(ff), "-","")
  # rearranging column names by alaphabetic order
  nm <- colnames(ff)[order(colnames(ff))]
  # rearrnge column postion by alaphabetic order
  ff <- ff[,nm]
  ff
} 

pheno <- lapply(1:length(xl), getPhenotypeData, xl)
nn <- unlist(lapply(pheno, colnames))
table(nn)
pheno <- do.call(rbind, pheno)

# to return the files to the original column order
dd <- read_xlsx(xl[1], sheet = 3, skip = 7)
# Only select the columns required
nm <- colnames(dd)[8:ncol(dd)]
nm <- str_replace_all(nm, fixed(" "),"")
nm <- str_replace_all(nm, "-","")
nm
# reorder the column in pheno
phenoClean <- pheno[,nm]

phenoClean[phenoClean == "na"] <- NA

write.csv(phenoClean, paste0(indir,"/clean-trials-all.csv"), row.names = FALSE)



getFirstLast5 <- function(tr, phenoClean, vars){
  # get the data for the specific trial
  trl <- phenoClean[phenoClean$Trial == tr, ]
  # convert to numeric
  trl <- as.tibble(trl) %>% mutate_all(as.numeric)
  # summarize by entry
  trl <- trl %>% 
    group_by(Entry) %>% 
    summarise_all(funs(mean(., na.rm = TRUE)))
  trl
  # sort the entire data by booting
  trl <- trl %>% arrange_(.dots = vars)
  
  # convert to vector
  cc <- pull(trl, !!vars)
  
  # get me first five row position
  k1 <- head(cc,5)
  y1 <- which(cc <= k1[5])
  first5 <- trl[y1,][,c("Entry",vars)]
  first5$ID <- 1
  
  # give me last 5
  k2 <- tail(cc, 5)
  y2 <- which(cc >= k2[1])
  last5 <- trl[y2,][,c("Entry",vars)]
  last5$ID <- 2
  
  firstlast <- rbind(first5,last5)
  
  return(t(firstlast))
}

booting <- lapply(1:10, getFirstLast5, phenoClean, "Booting")
heading <- lapply(1:10, getFirstLast5, phenoClean, "Heading")
gc <- lapply(1:10, getFirstLast5, phenoClean, "GroundCover")
maturity <- lapply(1:10, getFirstLast5, phenoClean, "Maturity")
yield <- lapply(1:10, getFirstLast5, phenoClean, "GrainYield")

tr1boot <- booting[[1]]
tr1head <- heading[[1]]
tr1gc <- gc[[1]]
tr1mat <- maturity[[1]]
tr1yld <- yield[[1]]

# common in all
trt <- c(tr1yld[1,], tr1mat[1,], tr1gc[1,], tr1boot[1,], tr1head[1,])
tb <- as.data.frame(table(trt))
tb <- tb %>% arrange(desc(Freq))
tb

sum(tb$Freq)

table(tr1yld[1,])
