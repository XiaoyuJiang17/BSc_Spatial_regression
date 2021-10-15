library(GWmodel)
library(rgdal)

data_csv = read.csv("/Users/jiangxiaoyu/Desktop/21 Summer/Spatial_Regression/2016_2020data.csv", header=T)
names(data_csv)
coordinates(data_csv) <- ~LATITUDE+LONGITUDE

# Regression for Winterweizen

fixedgaussian_bw <- bw.gtwr(Winterweizen ~ TEMP + TEMP_ATTRIBUTES + DEWP + DEWP_ATTRIBUTES + STP + WDSP + WDSP_ATTRIBUTES, data=data_csv,
                            obs.tv = data_csv$YEAR, approach = "AICc", kernel = "gaussian", adaptive = F, lamda = 0.05, t.units = "YEAR", ksi = 0,
                            verbose = TRUE)

#Run the GTWR model with the fixed Gaussian bandwidth
gtwr_model <- gtwr(Winterweizen ~ TEMP + TEMP_ATTRIBUTES + DEWP + DEWP_ATTRIBUTES + STP + WDSP + WDSP_ATTRIBUTES, data=data_csv, 
                   obs.tv = data_csv$YEAR, st.bw = fixedgaussian_bw, kernel="gaussian",
                   adaptive=FALSE, p=2, theta=0, longlat=F,lamda=0.05,t.units = "YEAR",ksi=0)

gtwr_model ## summary of model

#------------------------------------------------------------------------------------------------------------------------------------

# Regression for Wintergerste

fixedgaussian_bw2 <- bw.gtwr(Wintergerste ~ TEMP + TEMP_ATTRIBUTES + DEWP + DEWP_ATTRIBUTES + STP + WDSP + WDSP_ATTRIBUTES + MXSPD + GUST + MAX + MIN + PRCP, data=data_csv,
                            obs.tv = data_csv$YEAR, approach = "AICc", kernel = "gaussian", adaptive = F, lamda = 0.05, t.units = "YEAR", ksi = 0,
                            verbose = TRUE)

#Run the GTWR model with the fixed Gaussian bandwidth
gtwr_model2 <- gtwr(Wintergerste ~ TEMP + TEMP_ATTRIBUTES + DEWP + DEWP_ATTRIBUTES + STP + WDSP + WDSP_ATTRIBUTES + MXSPD + GUST + MAX + MIN + PRCP, data=data_csv, 
                   obs.tv = data_csv$YEAR, st.bw = fixedgaussian_bw2, kernel="gaussian",
                   adaptive=FALSE, p=2, theta=0, longlat=F,lamda=0.05,t.units = "YEAR",ksi=0)

gtwr_model2 ## summary of model
