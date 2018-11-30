save(list=ls(all=T),  file="HW6.RData")
# header----
setwd("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/HW6")
load("~/Google Drive/Purdue University/Academics/Sem-3/STAT545/HW6/HW6.RData")
options(scipen=5)

# functions----
shannon_entropy<-function(vec)
{
  freqs<-table(vec)/length(vec)
  return(-sum(freqs * log2(freqs)))
}
random_perm<-function(x, sym = symbol)
{
  s<-sample(sym, 2)
  temp<-x[s[1]]
  x[s[1]]<-x[s[2]]
  x[s[2]]<-temp
  return(x)
}
acceptance<-function(T.mat, X, perm)
{
  rand_perm<-random_perm(perm)
  a<-likelihood(T.mat, X, rand_perm)
  b<-likelihood(T.mat, X, perm)
  acc<-min(1, (a$ll/b$ll))
  return(list(new_perm=rand_perm, new_msg=a$msg, old_msg=b$msg, old_perm=perm, acc=acc))
}
decode<-function(K, msg)
{
  Y<-list()
  for(i in 1:length(msg))
  {
    Y<-c(Y,unlist(K[[msg[i]]]))
  }
  return(unlist(Y))
}
encode<-function(K, msg, iter=1000)
{
  for(i in 1:iter)
  {
    K<-random_perm(K)
  }
  msg<-unlist(strsplit(msg, ""))
  a<-char_map(K, msg)
  return(list(perm=K, msg=a, orig=msg))
}
MHalgo<-function(T.mat, X, perm, run=3000, verbose=F, B=1000)
{
  CTR<-1
  while(CTR<=run)
  {
    FLAG<-F
    acc<-acceptance(T.mat, X, perm)
    if(CTR>B)
    {
      CTR<-1
      if(acc$acc<1)
      {
        perm = acc$old_perm
        FLAG<-T
      }
      if((CTR %% 100)==0 && verbose)
      {
        print(CTR)
        if(FLAG)
        {
          print(paste(acc$old_msg[1:20], collapse = ''))
        }
        else
        {
          print(paste(acc$new_msg[1:20], collapse = ''))
        }
      }
    }
    else
    {
      perm<-acc$new_perm
    }
    CTR<-CTR+1
  }
}
likelihood<-function(T.mat, X, perm)
{
  
  X<-gsub("\n", "", X)
  X<-unlist(strsplit(X, ""))
  X<-decode(perm, X)
  diff<-setdiff(symbol, X)
  freq.X<-as.matrix(table(X[1:length(X)-1],X[2:length(X)]))
  rn<-rownames(freq.X)
  rn<-c(rn, diff)
  cn<-colnames(freq.X)
  cn<-c(cn, diff)
  for(i in 1: length(diff))
  {
    freq.X<-rbind(freq.X, 0)
    freq.X<-cbind(freq.X, 0)
  }
  colnames(freq.X)<-cn
  rownames(freq.X)<-rn
  freq.X<-freq.X[,colnames(T.mat)]
  freq.X<-freq.X[rownames(T.mat),]
  freq.X[freq.X<0]<-1e-03
  return(list(ll=sum(freq.X * log(T.mat)), msg=X))
}

# data----
msg<-readChar("input/message.txt", file.info("input/message.txt")$size)
wp<-readChar("input/wp.txt", file.info("input/wp.txt")$size)
wp<-gsub("\r", "", wp)
wp<-gsub("\n", " ", wp)
wp<-gsub("\\s{2,100}", " ", wp)
wp<-tolower(wp)
wp<-gsub("[^a-z\\,\\.\\: ]", "", wp)
wp<-unlist(strsplit(wp, ""))
wp.freq<-t(as.matrix(table(wp)))
T.mat<-unclass(table(wp[1:length(wp)-1],wp[2:length(wp)]))
T.mat[T.mat==0]<-1e-03
T.mat<-t(apply(T.mat, 1, function(x) x/sum(x)))
symbol<-sort(unique(wp))
perm<-setNames(as.list(symbol), symbol)

# operations----
barplot(wp.freq, main="Histogram of frequency")
shannon_entropy(wp)
heatmap(T.mat, Colv = NA, Rowv = NA, scale = "column")
MHalgo(T.mat, msg, perm, verbose=T, run=10000)
