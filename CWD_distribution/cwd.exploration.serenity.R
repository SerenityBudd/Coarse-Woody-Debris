source("libraries.R")
load("data/pool8.barcodes.Rda")

#exploratory graphs
with(pool8.barcodes[pool8.barcodes$stageht < 40,], plot(stageht~depth, col = rgb(0,0,0,0.3), pch = 20))
with(pool8.barcodes, plot(temp~depth))
with(pool8.barcodes, plot(current~depth))
with(pool8.barcodes, plot(cond~depth))
with(pool8.barcodes, plot(secchi~depth))
with(pool8.barcodes, plot(esveg92~depth))
with(pool8.barcodes, plot(wingdyke~depth))
with(pool8.barcodes, plot(trib~depth))
with(pool8.barcodes, plot(riprap~depth))
with(pool8.barcodes, plot(inout~depth))
with(pool8.barcodes, plot(closing~depth))
with(pool8.barcodes, plot(flooded~depth))



#Chi-squared tests to see if categorical variables are correlated or not.

# look at pool8.barcodes
str(pool8.barcodes)

# select the columns that are integers/numbers
pool8.barcodes.int <- select(pool8.barcodes, c(barcode, period:summary, effdist:dutycyc, utmzone:esveg92, wingdyke:othrstrc,contanrs:totfishc,pageno:rownum,length,grp_wdth:weight, recorder:batchno, lon:lat))

# quickly see summaries for the new df
for (i in 1:ncol(pool8.barcodes.int)) {
  print(colnames(pool8.barcodes.int)[i]) 
  print(summary(pool8.barcodes.int[,i]))
}

# get rid of colmns with too many NAs
pool8.barcodes.int <- select(pool8.barcodes.int, -c(rep, do, stageht, sveg92, vegd, eveg92, shtcnt, grp_wdth, weight, recorder, nfsh_cnt, orphflag))

# create a df to work with 
pool8.barcodes.num <- select(pool8.barcodes.int, c(depth,  temp, current, cond, secchi, wingdyke)) 

# look at pool8.barcodes.num
str(pool8.barcodes.num)
dim(pool8.barcodes.num)

# scatter plots of all the variables against each other
plot(pool8.barcodes.num)



# select the columns that are factors
pool8.barcodes.factor <- select(pool8.barcodes, c(site, sitetype, stratum, pool:gear, project, substrt, snag, labind, leader, fishcode, tfs, pathcode: userdef, aqua_code, aqua_desc))

# quickly see summaries for the new df
for (i in 1:ncol(pool8.barcodes.factor)) {
  print(colnames(pool8.barcodes.factor)[i]) 
  print(summary(pool8.barcodes.factor[,i]))
}

pool8.barcodes.factor <- select(pool8.barcodes.factor, -c(pool, gear, project, labind, leader, pathcode, subproj, userdef))

pool8.barcodes.factor <- select(pool8.barcodes.factor, c(colnames(pool8.barcodes.num), substrt, snag))

pool8.barcodes.factor <- pool8.barcodes.factor[complete.cases(pool8.barcodes.factor),]

chisq.test(tbl1)
chisq.test(tbl2)
chisq.test(tbl3)
chisq.test(tbl4)
chisq.test(tbl5)


tbl1 <- table(pool8.barcodes.factor[,c(3,7)]) #current v sbstrt
tbl2 <- table(pool8.barcodes.factor[,c(7,8)]) #sbstrt v cwd
tbl3 <- table(pool8.barcodes.factor[,c(4,7)]) #cond v sbstrt
tbl4 <- table(pool8.barcodes.factor[,c(5,7)]) #secchi v sbstrt
tbl5 <- table(pool8.barcodes.factor[,c(6,7)]) #wingdyke v sbstrt




str(pool8.barcodes.factor)

str(pool8.barcodes.factor[complete.cases(pool8.barcodes.factor),])
dim(pool8.barcodes.factor[complete.cases(pool8.barcodes.factor),])

plot(pool8.barcodes.factor)

library(MASS)

str(survey)
