

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/s/xxfloksp0968mgu/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month

df.raw$year = substr(as.character(df.raw$date), start = 1, stop = 4)
df.raw$month = substr(as.character(df.raw$date), start = 5, stop = 6)


# b) aggregate into monthly data with number of trips and total expenditure

df1 = aggregate(
  x = list(qty = df.raw$qty), 
  by = list(id = df.raw$id,year = df.raw$year), 
  FUN = sum)
df2 = aggregate(
  x = list(expd = df.raw$expd), 
  by = list(id = df.raw$id,year = df.raw$year), 
  FUN = sum)
df3 = aggregate(
  x = list(trip = df.raw$qty), 
  by = list(id = df.raw$id,year = df.raw$year), 
  FUN = length)
df.new = cbind(df1, expd = c(df2$expd))
df.new = cbind(df.new, trip = c(df3$trip))

# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calcualte RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.

df.complete = expand.grid(unique(df.new$id),c(1997,1998),c(paste("0",1:9,sep = ""),10,11,12))
colnames(df.complete) = c("id", 'year', 'month')

df.final = merge(df.new, df.complete, by = c("id", 'year', 'month'),all = T)

df.final$qty[is.na(df.final$qty)] = 0
df.final$expd[is.na(df.final$expd)] = 0
df.final$trip[is.na(df.final$trip)] = 0

df.final= subset(df.final, !(df.final$year == 1998 & df.final$month > '06'))

df.final$year = as.numeric(df.final$year)
df.final$month = as.numeric(df.final$month)

# now we should have the dataset we need; double check to make sure that every consumer is in every period


# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency

df.final$recency = NA
uid = unique(df.final$id)

df.final2 = c()

for(j in 1:length(uid)){
  
  tempid = uid[j]
  
  tempdf = df.final[df.final$id == tempid, ]
  
  
  for (i in 1:(length(tempdf$id)-1)){
    if(tempdf$trip[i] != 0 ){
      tempdf$recency[i+1] = 1
    }else{
      tempdf$recency[i+1] = tempdf$recency[i] + 1
    }
  }
  
  df.final2 = rbind(df.final2, tempdf)
  
}


# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 8 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency

df.final2$quarters = c()
for(i in 1: length(df.final2$id)){
  if(df.final2$month[i] < 4){
    df.final2$quarters[i] = 1
  }else if(df.final2$month[i] < 7 & df.final2$month[i] > 3){
    df.final2$quarters[i] = 2
  }else if(df.final2$month[i] < 10 & df.final2$month[i] > 6){
    df.final2$quarters[i] = 3
  }else{
    df.final2$quarters[i] = 4
  }
  if(df.final2$year[i] == 1998){
    df.final2$quarters[i] = df.final2$quarters[i] +4
  }
}

df.final2$frequency = NA
uid = unique(df.final2$id)
uqu = unique(df.final2$quarters)

df.final3 = c()

for(i in 1: length(uid)){
  tempid = uid[i]
  tempdf1 = df.final2[df.final2$id == tempid, ]
  for(j in 2:6){
    tempqu = uqu[j]
    tempdf2 =  tempdf1[tempdf1$quarters == (tempqu-1), ]
    tempdf1$frequency[tempdf1$quarters == j] = sum(tempdf2$trip)
  }
  
  df.final3 = rbind(df.final3, tempdf1)
}



# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the beginning to 
#   the PAST MONTH. Call this df$monvalue

df.final3$monvalue = NA
uid = unique(df.final$id)
df.final4 = c()
df.final3$enter = c()
for(i in 1:length(df.final3$id)){
if(df.final3$trip[i] == 0){
  df.final3$enter[i] = 0
}else{df.final3$enter[i] = 1
  }
}
for(i in 1:length(uid)){
  tempid = uid[i]
  tempdf = df.final3[df.final3$id == tempid, ]
for (j in 2:18) {
   tempdf$monvalue[j] = sum(tempdf$expd[1:j-1])/sum(tempdf$enter[1:j-1])
 }
  
  df.final4 = rbind(df.final4, tempdf)
}

# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

b1 <- -0.05
b2 <- 3.5
b3 <- 0.05

df.final4$index <- b1*df.final4$recency + b2*df.final4$frequency + b3*df.final4$monvalue


# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-trip revenue that these consumers generate and comment on 
#   whether the RFM index help you segment which set of customers are "more valuable"

df.final5 = df.final4[!is.na(df.final4$index),] 
group = quantile(df.final5$index, probs = seq(0.1,1,0.1),type = 1, na.rm = T)

for(i in 1:length(df.final5$id)){
  if(df.final5$index[i] <= group[1]) {
    df.final5$value[i] = 1
  }else{
    for(j in 2:length(group)){
      if(df.final5$index[i] <= group[j] & df.final5$index[i] > group[j-1]){
        df.final5$value[i] = j
      }
    }
  }
}


df.y = aggregate( x = list(meanexpd = df.final5$expd),
           by = list(group = df.final5$value),
           FUN = mean)

barplot(df.y$meanexpd,xlab = 'deciles in the RFM index', ylab = 'average expenditure',names.arg = 1:10)

#as the bar shows, the RFM can roughly estimate which set of customers are "more valuable"
#I will target group 10.

