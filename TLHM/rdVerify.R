

rm(list = ls())

require(lme4)
require(Metrics)
library(Matrix)

setwd("..")
source("mrfLMR.R")
source("Funs.R")
source("loglik.R")
source("inv.R")

setwd("./RealData")

df <- read.csv("data.csv", header = T, string = T)
#dim(df)
#200125   27
group <- read.csv("group.csv", header = T, string = T)
#length(group)
#5487
load("nb.RData")
colnames(df)
# [1] "DealID"           "CommunityID"      "TradingYear"      "TradingMonth"    
# [5] "TradingDay"       "TotalPrice"       "UnitPrice"        "BedroomNum"      
# [9] "LivingroomNum"    "Size"             "Orientation"      "FloorOfHouse"    
#[13] "BuiltYear"        "BuildingType"     "FloorOfBuilding"  "District"        
#[17] "Lat"              "Lng"              "FloorAreaRatio"   "LandscapingRatio"
#[21] "AvgPricePerM2"    "HousingType"      "BuildingNum"      "HousingNum"      
#[25] "SubwayLineInfo"   "SchoolID"         "NeighborBusiness" 

heatmap <- group
group <- group$CommunityID

df <- within(df, {
	Age <- TradingYear - BuiltYear
	resp <- UnitPrice
	#resp <- log(UnitPrice*Size)
})

#browser()

## linear model BASE_LINE
lm.model <- lm(resp ~ TradingYear + BedroomNum + LivingroomNum + Size + 
	Orientation + FloorOfHouse + Age + BuildingType + FloorOfBuilding +
	District + FloorAreaRatio + LandscapingRatio + HousingType + BuildingNum + 
	HousingNum + SubwayLineInfo + SchoolID + NeighborBusiness, data = df)
#lm.rmse <- rmse(actual = df$resp, predicted = lm.model$fitted.values)
lm.rmse <- rmse(actual = exp(df$resp), predicted = exp(lm.model$fitted.values))

#browser()


## linear mixed model BASE_LINE
lx.model <- lmer(resp ~ TradingYear + BedroomNum + LivingroomNum + Size + 
	Orientation + FloorOfHouse + Age + BuildingType + FloorOfBuilding +
	District + FloorAreaRatio + LandscapingRatio + HousingType + BuildingNum + 
	HousingNum + SubwayLineInfo + SchoolID + NeighborBusiness +
					(1 | CommunityID), data = df)
#lx.rmse <- rmse(actual = df$resp, predicted = fitted(lx.model))
lx.rmse <- rmse(actual = exp(df$resp), predicted = exp(fitted(lx.model)))
#browser()




rslt <- mrfLMR(data = df, group = group, nb = nb, dst = F, maxIter = 200)
rslt.df <- within(df,{
	x <- rslt$mu[match(CommunityID, group)]
	resp <- resp - x
})
#mrf.rmse <- rmse(actual = rslt.df$resp, predicted = rslt[["lm.model"]]$fitted.values)
mrf.rmse <- rmse(actual = exp(rslt.df$resp), predicted = exp(rslt[["lm.model"]]$fitted.values))

heatmap <- cbind(heatmap, rslt$mu)
colnames(heatmap)[4] <- "nb"

write.csv(file = "heatmap_BJ.csv", heatmap, row.names = F, quote = T)









