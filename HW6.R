save(list=ls(all=T),  file="HW6.RData")
# header----
setwd("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/HW6")
setwd("C:/Users/manan/OneDrive - purdue.edu/Sem 3/Stat545/HW6")
load("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/HW6/HW6.RData")
options(scipen=5)

# functions----
shannon_entropy<-function(vec)
{
  freqs<-table(vec)/length(vec)
  return(-sum(freqs * log2(freqs)))
}

# data----
wp<-readChar("input/wp.txt", file.info("input/wp.txt")$size)
wp<-gsub("\r", "", wp)
wp<-gsub("\n", " ", wp)
wp<-gsub("\\s{2,100}", " ", wp)
wp<-tolower(wp)
wp<-gsub("[^a-z\\,\\.\\: ]", "", wp)
wp<-unlist(strsplit(wp, ""))
wp.freq<-t(as.matrix(table(wp)))

# operations----

barplot(wp.freq, main="Histogram of frequency")
shannon_entropy(wp)
