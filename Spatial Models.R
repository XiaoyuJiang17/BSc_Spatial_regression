library(rgdal)
library(spdep)
library(rgeos)

#Reading in Shapefile with Data
data = readOGR(dsn="/Users/jiangxiaoyu/Desktop/Files for Spatial Analysis Project/Baden-Württemberg shpfile/Baden_Wurttemberg_Final.shp",layer="Baden_Wurttemberg_Final")
names(data) #X2020_csv1 to X2020_csv12 are: TEMP TEMP_ATTRIBUTES	DEWP	DEWP_ATTRIBUTES	STP	WDSP	WDSP_ATTRIBUTES	MXSPD	GUST	MAX	MIN	PRCP
            #X2020_csv13:Winterweizen
            #X2020_csv14:Wintergerste
summary(data)
spplot(data,"X2020_csv13") # make a map for Winterweizen
spplot(data,"X2020_csv14") # make a map for Wintergerste

#Creating Spatial Weights in R
queen.nb = poly2nb(data)
rook.nb = poly2nb(data,queen = FALSE)

# Regression for X2020_csv13(Winterweizen)
# Top 6 significant variable from Global regression: X2020_csv10, X2020_csv11, X2020_csv_2, X2020_csv12, X2020_csv_1, X2020_csv_7
# Bug here: we can only apply 6 independent variables here
reg.equ1 = X2020_csv13 ~ X2020_csv10 + X2020_csv11 +  X2020_csv_2 + X2020_csv12 + X2020_csv_1 + X2020_csv_7

#Convert queen.nb to queen.listw (needed for regression)
queen.listw = nb2listw(queen.nb)
rool.listw = nb2listw(rook.nb)
listw1 = queen.listw

#OLS for X2020_csv13 (Winterweizen)
reg1 = lm(reg.equ1,data=data)
summary(reg1)
lm.morantest(reg1,listw1) #null hypothesis: no spatial relationship

#SLX Spatially Lagged X
reg2 = lmSLX(reg.equ1,data = data,queen.listw)
summary(reg2)

#SAR Spatial Lag Autoregression
reg3 = lagsarlm(reg.equ1,data = data,queen.listw)
summary(reg3)

#SEM Spatial Error Model
reg4 = errorsarlm(reg.equ1,data=data,queen.listw)
summary(reg4)

# ------------------------------------------------------------------------------------------------------------------------

# Regression for X2020_csv14(Wintergerste)
# Top 6 significant variable from Global regression: X2020_csv_1, X2020_csv_9, X2020_csv_7, X2020_csv_2, X2020_csv12, X2020_csv_3
reg.equ2 = X2020_csv14 ~ X2020_csv_1 +  X2020_csv_9 + X2020_csv_7 + X2020_csv_2 + X2020_csv12 + X2020_csv_3

#Convert queen.nb to queen.listw (needed for regression)
queen.listw = nb2listw(queen.nb)
rool.listw = nb2listw(rook.nb)
listw2 = queen.listw

#OLS for X2020_csv14 (Wintergerste)
reg1 = lm(reg.equ2,data=data)
summary(reg1)
lm.morantest(reg1,listw2) #null hypothesis: no spatial relationship
lm.LMtests(reg1,listw2,test=c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

#SLX Spatially Lagged X
reg2 = lmSLX(reg.equ2,data = data,queen.listw)
summary(reg2)

#SAR Spatial Lag Model
reg3 = lagsarlm(reg.equ2,data = data,queen.listw)
summary(reg3)

#SEM Spatial Error Model
reg4 = errorsarlm(reg.equ2,data=data,queen.listw)
summary(reg4)

