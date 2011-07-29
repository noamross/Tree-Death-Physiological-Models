parmstotable = function(parmstable) {
#this function creates a LaTeX table of the run parameter values
require(xtable)
require(Hmisc)
parmnames = parmtable[,"LaTeX"]
dummies = paste(rep("dum",length(parmnames)),1:length(parmnames),sep="")
dummies2 = paste(rep("dumm",length(parmnames)),1:length(parmnames),sep="")
columnnames = c("In R code", "Value", "Units")
ltable = matrix(nrow=length(parms),ncol=length(columnnames))
colnames(ltable) = columnnames
rownames(ltable) = dummies
ltable[,1] = parmtable[,"Parameter"]
ltable[,2] = parmtable[,"Value"]
ltable[,3] = dummies2
tableout = capture.output(print(xtable(ltable, caption="Parameters", align=c("llrr"))))
for(i in 1:length(parmnames)) {
  tableout = gsub(paste(dummies[i]," ",sep=""),paste(parmnames[i]," ",sep=""),tableout)
  tableout = gsub(paste("& ",names(parms)[i]," &",sep=""), paste("& ","\\\\texttt{",names(parms)[i],"}"," &",sep=""),tableout)
  tableout = gsub(paste(" ",dummies2[i]," ",sep=""),paste(" ",latexTranslate(parmtable[i,"Units"])," ",sep=""),tableout)
}
return(tableout)
}
#cat(tableout,sep='\n')


