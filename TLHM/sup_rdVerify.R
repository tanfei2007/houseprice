rm(list = ls())

require(lme4)
require(Metrics)
library(Matrix)
require(dplyr)

source("TSHM.R")
#source("Funs.R")
source("Funs_Current.R")
source("Eval.R")


df <- read.csv("../Data/data.csv", header = T, string = T)
#dim(df)
#200125   27
group <- read.csv("../Data/group.csv", header = T, string = T)
#length(group)
#5487
#load("nb.RData")
#load("nb_20.RData")
#load("nb_40.RData")
load("../Data/nb_200.RData")

colnames(df)
# [1] "DealID"           "CommunityID"      "TradingYear"      "TradingMonth"    
# [5] "TradingDay"       "TotalPrice"       "UnitPrice"        "BedroomNum"      
# [9] "LivingroomNum"    "Size"             "Orientation"      "FloorOfHouse"    
#[13] "BuiltYear"        "BuildingType"     "FloorOfBuilding"  "District"        
#[17] "Lat"              "Lng"              "FloorAreaRatio"   "LandscapingRatio"
#[21] "AvgPricePerM2"    "HousingType"      "BuildingNum"      "HousingNum"      
#[25] "SubwayLineInfo"   "SchoolID"         "NeighborBusiness" 
df <- df[!(df$HousingType %in% c("其他", "未知")), ]
# dim(df)
#[1] 200122     27


group <- group$CommunityID

df <- within(df, {
	Age <- TradingYear - BuiltYear
	resp <- UnitPrice
	TradingYear <- paste(TradingYear, "Q", ceiling(TradingMonth/3), sep = "")
	TradingYear <- as.factor(TradingYear)
	BedroomNum <- as.factor(BedroomNum)
	LivingroomNum <- as.factor(LivingroomNum)
})


##split datasets into different cases##
#2011Q1 2011Q2 2011Q3 2011Q4 2012Q1 2012Q2 2012Q3 2012Q4 2013Q1 2013Q2 2013Q3 2013Q4 2014Q1 2014Q2 2014Q3 2014Q4 2015Q1 
#13    138   1055   4920   6978  10488  10494  14107  15675  12074  17353  13877   8894   8762  11273  19572  17384 
#2015Q2 
#27065


#case 1
# train.df <- subset(df, !(TradingYear %in% c("2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear == "2015Q2")
# test.df <- test.df[order(test.df$CommunityID),]

#case 2
# train.df <- subset(df, !(TradingYear %in% c("2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]

#case 3
# train.df <- subset(df, !(TradingYear %in% c("2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]

#case 4
# train.df <- subset(df, !(TradingYear %in% c("2014Q3", "2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2014Q3", "2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]

#step 5
# train.df <- subset(df, !(TradingYear %in% c("2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]


#case 6
# train.df <- subset(df, !(TradingYear %in% c("2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]

#ccase 7
# train.df <- subset(df, !(TradingYear %in% c("2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]
# test.df <- test.df[test.df$District !='门头沟',]

#case 8
train.df <- subset(df, !(TradingYear %in% c("2013Q3","2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2")))
train.df <- train.df[order(train.df$CommunityID),]
test.df <- subset(df, TradingYear %in% c("2013Q3","2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2"))
test.df <- test.df[order(test.df$CommunityID),]
test.df <- test.df[test.df$District !='门头沟',]

#case 9
# train.df <- subset(df, !(TradingYear %in% c("2013Q2", "2013Q3","2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2")))
# train.df <- train.df[order(train.df$CommunityID),]
# test.df <- subset(df, TradingYear %in% c("2013Q2", "2013Q3","2013Q4","2014Q1","2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2"))
# test.df <- test.df[order(test.df$CommunityID),]
# test.df <- test.df[test.df$District !='门头沟',]

#*************seperate neighborhoods**************************
names(nb) <- as.character(group)

train.group <- sort(unique(train.df$CommunityID))
test.group <- sort(unique(test.df$CommunityID))

train.group <- as.character(train.group)
test.group <- as.character(test.group)

train.nb <- nb[train.group]
train.nb <- lapply(train.nb, function(x){
	rslt <- x[names(x) %in% train.group]
	return(rslt)
})
names(train.nb) <- as.character(train.group)

test.nb <- nb[test.group]
test.nb <- lapply(test.nb, function(x){
	rslt <- x[names(x) %in% test.group]
	return(rslt)
})
names(test.nb) <- test.group

train.df <- within(train.df, {
  TradingYear <- as.factor(as.character(TradingYear))
})


#***********************************************
train.period = sort(unique(train.df$TradingYear))
train.period = as.character(train.period)
test.period = sort(unique(test.df$TradingYear))
test.period = as.character(test.period)
#************************************************
p = 0
q = 3
K = 5
xi0 = 0.9
xi1 = 0.1


ptm <- proc.time()
## our methods
train.rslt <- TSHM(data = train.df, group = train.group, period = train.period, 
                   nb = train.nb, p = p, q = q, maxIter = 50, K, xi0 = xi0, xi1 = xi1)
runtime <- proc.time() - ptm
save(file='train.rslt.RData', train.rslt)



#One step/multip-step prediction

test.rslt <- predict(train.rslt$hmodel$model, newdata = test.df)
train.test.U <- getTestU2(test.df, train.U = train.rslt$U, Phi = train.rslt$Phi, p = p, q = q, nb = nb, K = K)

test.rows = match(test.df$CommunityID, rownames(train.test.U))
test.cols = match(test.df$TradingYear, colnames(train.test.U))

test.U = mapply(function(x,y){
	return(train.test.U[x, y])
}, test.rows, test.cols)

test.rslt.resp = test.rslt + test.U



#each test period
each.test.rslt.rmse <- lapply(test.period, function(x){
  idx_period = which(test.df$TradingYear == x)
  rslt = eval(trueValue = test.df$resp[idx_period], prdtValue = test.rslt.resp[idx_period])
  return(rslt)
})
each.test.rslt.rmse <- do.call(rbind, each.test.rslt.rmse)
each.test.rslt.rmse <- as.data.frame(each.test.rslt.rmse)
each.test.rslt.rmse$period = test.period
each.test.rslt.rmse = cbind(each.test.rslt.rmse[,ncol(each.test.rslt.rmse)], each.test.rslt.rmse[,1:(ncol(each.test.rslt.rmse) - 1)])
names(each.test.rslt.rmse)[1] <- 'period' 
print(each.test.rslt.rmse)

write.csv(file = 'OurMethod_8_step.csv', each.test.rslt.rmse)


