#### sCFR Review Paper - Data gathering and analysis
TODAY <- format(Sys.Date(), "%d-%m-%Y")
TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')

#### folder containing table outputs
output_dir <- paste0(getwd(),"/output")

##### Cross-study variables
n=10 #no. cfr studies
m=1  #no. ifr studies
study            <- rep("",n)
strain           <- rep("",n)
country          <- rep("",n)
period           <- rep("",n)
method           <- list(list("Cases"),list("Deaths"))
study_i          <- rep("",m)
strain_i         <- rep("",m)
country_i        <- rep("",m)
period_i         <- rep("",m)
method_i         <- list(list("Cases"),list("Deaths"))
studies          <- list()
seasons          <- list()
geograp          <- list()
studies_i        <- list()
seasons_i        <- list()
geograp_i        <- list()
IFR100k_years    <- list()
IFR100k_years1   <- list()
IFR100k_years2   <- list()
CFRs100k_years   <- list()
CFRs100k_years1  <- list()
CFRs100k_years2  <- list()
CFRs100k         <- rep(NA,n)
CFRs100k2        <- rep(NA,n)
CFRs100k1        <- rep(NA,n)
IFR100k          <- rep(NA,m)
IFR100k2         <- rep(NA,m)
IFR100k1         <- rep(NA,m)



### IFR seasonal - single study ################################################
i=1
study_i[i]     <- "Kwok 2017"
strain_i[i]    <- "seasonal"
country_i[i]   <- "China (Hong Kong)"
period_i[i]    <- "Jan-09-Dec-11"
seasons_i[[i]] <- c("Jan-09-Nov-09", "Dec-09-Nov-10", "Dec-10-Dec-11", "Jan-09-Nov-09", "Dec-09-Nov-10", "Dec-10-Dec-11")
studies_i[[i]] <- rep(study_i[i],length(seasons_i[[i]]))
geograp_i[[i]] <- rep(country_i[i],length(seasons_i[[i]]))
method_i[[i]]  <- c("Cases"="Sero survey","Deaths"="confirmed or excess")
#Annual IFR and UI - reported
IFR100k_years[[i]] =c(9.3, 30.8, 37.0, 46.2, 78.7, 54.6)
IFR100k_years1[[i]]=c(6.6, 20.9, 28.1, 22.7, 53.5, 18.9)
IFR100k_years2[[i]]=c(13.1,45.5, 48.8, 93.0, 116, 149.0) 

#Overall IFR and UI - derived
IFR100k[i]   <- mean(IFR100k_years[[i]]) #[1] 42.76667
ns = length(IFR100k_years[[i]])
SE = sd(IFR100k_years[[i]])/sqrt(ns)
IFR100k1[i]  <- IFR100k[i] - qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 23.51094
IFR100k2[i]  <- IFR100k[i] + qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 62.0224




##### CFRs #####################################################################



##### Abdalla 2020
i=1
study[i]     <- "Abdalla 2020"
strain[i]    <- "seasonal"
country[i]   <- "Saudi Arabia"
period[i]    <- "2010-2016"
seasons[[i]] <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
suspec       =  c(37551, 12903, 2743, 1545, 5398, 27097, 26265) #sum(suspected) #[1] 113502
deaths       =  c(176, 16, 12, 11, 49, 232, 111)                #sum(deaths)    # [1] 607
#Annual CFR and UI - derived
CFRs100k_years[[i]]  = round(10^5*deaths/suspec,1) #[1] 468.7 124.0 437.5 712.0 907.7 856.2 422.6
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - derived
CFRs100k[i]   <- mean(CFRs100k_years[[i]]) #561.2429
ns = length(CFRs100k_years[[i]])
SE = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i]  <- CFRs100k[i]-qt(.95,ns-1,lower.tail=TRUE)*SE # 357.0
CFRs100k2[i]  <- CFRs100k[i]+qt(.95,ns-1,lower.tail=TRUE)*SE # 765.5
#as reported in Table 1
CFRs100k[i]   <- 534.8
CFRs100k1[i]  <- 500   #(method not stated)
CFRs100k1[i]  <- 580   #(method not stated)



##### Bandaranayake 2011
i=2
study[i]     <- "Bandaranayake 2011"
strain[i]    <- "seasonal"
country[i]   <- "New Zealand"
period[i]    <- "2010"
seasons[[i]] <- c("2010-Jan-Oct")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
method[[i]]  <- c("Cases"="sero survey + questionnaire","Deaths"="confirmed")
sympt        =  176308
deaths       =  15
#Annual and overall CFR - reported
CFRs100k_years[[i]]  = round(10^5*deaths/sympt,1) #8.5
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - reported
CFRs100k[i]  <- 10^5*deaths/sympt #8.507839 
#as reported in text
CFRs100k[i]  <- 8.5
CFRs100k1[i] <- NA
CFRs100k2[i] <- NA



##### Hauge 2019
i=3
study[i]     <- "Hauge 2019"
strain[i]    <- "seasonal"
country[i]   <- "Norway"
period[i]    <- "2008-2016"
seasons[[i]] <- c("2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
symp         =  c(68919, 189875, 80072, 76041, 108197, 49510, 91377, 80771, 92462) #mean(symp)      [1] 93024.89
fatalhosp    =  c(8, 28, 17, 36, 85, 47, 124, 88, 232)                             #mean(fatalhosp) [1] 73.88889
#Annual CFR and UI - derived
CFRs100k_years[[i]]  = round(10^5*fatalhosp/symp,1) #[1]  11.6  14.7  21.2  47.3  78.6  94.9 135.7 108.9 250.9
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - derived
CFRs100k[i]  <- mean(CFRs100k_years[[i]])    #84.9
#            10^5*mean(fatalhosp)/mean(symp) #79.4
ns           = length(CFRs100k_years[[i]])
SE           = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i] <- CFRs100k[i]-qt(.95,ns-1,lower.tail=TRUE)*SE # 37.5
CFRs100k2[i] <- CFRs100k[i]+qt(.95,ns-1,lower.tail=TRUE)*SE # 132.2



##### Kondratiuk 2016 - 2 seasons
i=4
study[i]     <- "Kondratiuk 2016"
strain[i]    <- "seasonal"
country[i]   <- "Poland"
period[i]    <- "2012-2014"
seasons[[i]] <- c("2012-2013", "2013-2014")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
symp         =  c(3164405, 2780945)
deaths       =  c(115, 8) 
#Annual CFR and UI - derived
CFRs100k_years[[i]]  = round(10^5*deaths/symp,1) #3.6 0.3
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - derived
CFRs100k[i]  <- mean(CFRs100k_years[[i]])  #1.95
#            10^5*mean(deaths)/mean(symp)  #2.07
ns           = length(CFRs100k_years[[i]])
SE           = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i] <- max(0,CFRs100k[i]-qt(.95,ns-1,lower.tail=TRUE)*SE) #0 truncated (n=2 unsuitable)
CFRs100k2[i] <- CFRs100k[i]+qt(.95,ns-1,lower.tail=TRUE)*SE #12.37



##### Li 2011 - 2 seasons
i=5
study[i]     <- "Li 2011"
strain[i]    <- "seasonal"
country[i]   <- "China (Guangzhou)"
period[i]    <- "2009-2011"
seasons[[i]] <- c("2010-2011", "2011-2012")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
symp         =  c(20885, 17188)
deaths       =  c(15,1) 
#Annual CFR and UI - reported in text
CFRs100k_years[[i]]  = round(10^5*deaths/symp,1) #71.8  5.8
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - derived
CFRs100k[i]  <- mean(CFRs100k_years[[i]])  #38.8
#            10^5*mean(deaths)/mean(symp)  #42.1
ns           = length(CFRs100k_years[[i]])
SE           = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i] <- max(0,CFRs100k[i]-qt(.95,ns-1,lower.tail=TRUE)*SE) #0 truncated (n=2 unsuitable)
CFRs100k2[i] <- CFRs100k[i]+qt(.95,ns-1,lower.tail=TRUE)*SE #247.2




##### McDonald 2023
i=6
study[i]     <- "McDonald 2023"
strain[i]    <- "Seasonal A-B"
country[i]   <- "Netherlands"
period[i]    <- "2011-2020"  #between A(H1N1)pdm and COVID-19 pandemics
#aggregated nine seasons, from 2011/2012 to 2019/2020,
seasons[[i]] <- c("2011-2020")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
method[[i]]  <- c("Cases"="Bayesian modelling + ILI/GP + LAB + internet-survey(care-seeking)","Deaths"="Model IAD + all-cause + respiratory")
#Overall CFR and UI - reported in Table 1
deaths       = 5585 
symp         = 4676000
#Annual-aggregated CFR and UI - reported in text
CFRs100k_years[[i]]  = round(10^5*deaths/symp,1) #119.4
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - reported in text
CFRs100k[i]  = 10^5*deaths/symp   #119.4 
#as reported
CFRs100k[i]  = 120
CFRs100k1[i] = 10^5/100*0.116     #116
CFRs100k2[i] = 10^5/100*0.123     #123




##### Mishra 2010
i=7
study[i]     <- "Mishra 2010"
strain[i]    <- "seasonal"
country[i]   <- "India (Pune)"
period[i]    <- "2009"
seasons[[i]] <- c("2009-Aug-Oct")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
#Annual CFR and UI - reported in Table 3
CFRs100k_years[[i]]  = 130
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR - reported in Table 3
CFRs100k[i]  = 130
CFRs100k1[i] = NA
CFRs100k2[i] = NA



##### Pana 2020
i=8
study[i]     <- "Pana 2020"
strain[i]    <- "seasonal"
country[i]   <- "Romania"
period[i]    <- "2014-2019"
seasons[[i]] <- c("2014-2019")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
#aggregated seasons
symp         = c(591151) #average per season
deaths       = c(577)   #average per season
#Annual CFR and UI - derived
CFRs100k_years[[i]]  = 10^5*deaths/symp #[1] 97.6
CFRs100k_years1[[i]] = rep(NA,length(CFRs100k_years[[i]]))
CFRs100k_years2[[i]] = rep(NA,length(CFRs100k_years[[i]]))
#Overall CFR and UI - derived
CFRs100k[i]  = 10^5*deaths/symp #[1] 97.6
# no UI reported for cases and deaths
CFRs100k1[i] = NA
CFRs100k2[i] = NA



##### Rosano 2019
i=9
study[i]     <- "Rosano 2019" #Filippini 2021"
strain[i]    <- "Seasonal"
country[i]   <- "Italy"
period[i]    <- "2013-2017" #"2012-2018"
seasons[[i]] <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
method[[i]]  <- c("Cases"="reported+annual-mean","Deaths"="reported+annual-mean")
casesv     = c(  4542000,  6299000,  4876900,  5440900)
deathsv    = c(    7027,    20259,    15801,  24981)
#UI deaths
deaths1    = c(  5785, 18506, 14434, 23001)
deaths2    = c(  8347, 22064, 17293, 27014)
#UI cases - assume have double spread as deaths - there is more uncertainty about symptomatic (cf. CDC estimates)
cases1     = casesv*(1-2*(deathsv-deaths1)/deathsv)
cases2     = casesv*(1+2*(deaths2-deathsv)/deathsv)
#Annual CFR and UI - derived
CFRs100k_years[[i]]  = (10^5)*deathsv/casesv #[1] 154.7 321.6 324.0 459.1
    #UI from ratios
library(pairwiseCI)
CFRs100k_years1[[i]] <- CFRs100k_years[[i]]
CFRs100k_years2[[i]] <- CFRs100k_years[[i]]
ns = length(CFRs100k_years[[i]])
for (it in (1:ns)){
  deathso1  = deaths1[it];  
  deathso2  = deaths2[it];  
  sympo1    = cases1[it];  
  sympo2    = cases2[it];  
  denom_pt  = casesv[it];
  denom_CIs = c(sympo1,sympo2)
  numer_pt  = 10^5*deathsv[it];
  numer_CIs = 10^5*c(deathso1,deathso2)
  CIs = pairwiseCI::MOVERR(denom_pt, denom_CIs, numer_pt, numer_CIs, alternative = "two.sided")$conf.int
  CFRs100k_years1[[i]][it]=as.numeric(CIs[1]) #[1]  106.3 266.6 266.4 386.5
  CFRs100k_years2[[i]][it]=as.numeric(CIs[2]) #[1]  246.6 395.9 399.6 554.7
  print(paste0("i=",it,", ",round(CFRs100k_years1[[i]][it]),",", round(CFRs100k_years[[i]][it]),",",round(CFRs100k_years2[[i]][it])))
}
#Overall CFR and UI - derived
CFRs100k[i]   <- mean(CFRs100k_years[[i]]) #314.9
ns = length(CFRs100k_years[[i]])
SE = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i]  <- CFRs100k[i]-qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 168.2
CFRs100k2[i]  <- CFRs100k[i]+qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 461.5



##### Rolfes 2016 
# CDC 2010-2023 (except 2020-2021)
# Estimates are not available for the 2020-2021 flu season due to minimal influenza activity.
# https://www.cdc.gov/flu-burden?CDC_AAref_Val=https://www.cdc.gov/flu/about/burden/index.html
# https://www.cdc.gov/flu-burden/php/data-vis/past-seasons.html
i=10
study[i]     <- "Rolfes 2016"
strain[i]    <- "Seasonal"
country[i]   <- "USA"
period[i]    <- "2010-2023"
method[[i]]  <- c("Cases"="surveillance+model","Deaths"="surveillance+deaths+EM-model")
seasons[[i]] <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", 
                  "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2021-2022", "2022-2023")
studies[[i]] <- rep(study[i],length(seasons[[i]]))
geograp[[i]] <- rep(country[i],length(seasons[[i]]))
deathsv  = c(  36000,   12000,    42000,    37000,    51000,    22000,    38000,    51000,    27000,    25000,    4900,    21000)
hospv    = c(   280000,  130000,   570000,   340000,   590000,   270000,   490000,   710000,   370000,   390000,  100000,   360000)
sympv    = c( 21000000, 9300000, 33000000, 29000000, 30000000, 23000000, 29000000, 41000000, 28000000, 35000000, 9400000, 31000000)
# UI
symp1    <- 10^6*c(16,8.6, 26, 24, 26, 19, 24, 35, 24, 28, 7.8, 26)
symp2    <- 10^6*c(46, 12, 66, 41, 39, 32, 44, 53, 40, 70,  15, 51)
deaths1  <- 10^3*c( 21, 10, 25, 23, 34, 17, 28, 36, 19, 18,  4, 18)
deaths2  <- 10^3*c(140, 23,110,100,100, 35, 60, 95, 96, 78, 24, 97)
#Annual CFR and UI - derived
CFRs100k_years[[i]]=round(10^5*deathsv/sympv,1)
#[1] 171.4 129.0 127.3 127.6 170.0  95.7 131.0 124.4  96.4  71.4  52.1  67.7
   #UI from ratios
library(pairwiseCI)
CFRs100k_years1[[i]]<-CFRs100k_years[[i]]
CFRs100k_years2[[i]]<-CFRs100k_years[[i]]
ns = length(CFRs100k_years[[i]])
for (it in (1:ns)){
deathso1  = deaths1[it];  
deathso2  = deaths2[it];  
sympo1    = symp1[it];  
sympo2    = symp2[it]; if(it==3 || it==10){sympo2 = 0.99*symp2[it]} #otherwise get NA - not sure why - but minor correction
denom_pt  = sympv[it];
denom_CIs = c(sympo1,sympo2)
numer_pt  = 10^5*deathsv[it];
numer_CIs = 10^5*c(deathso1,deathso2)
CIs = pairwiseCI::MOVERR(denom_pt, denom_CIs, numer_pt, numer_CIs, alternative = "two.sided")$conf.int
CFRs100k_years1[[i]][it]=as.numeric(CIs[1])
#[1]  65.6  94.2  53.7  71.1 105.1  62.8  78.0  80.8  58.4  33.2  31.2  40.1
CFRs100k_years2[[i]][it]=as.numeric(CIs[2])
#[1] 693.5 248.8 346.0 353.2 339.5 158.5 215.5 237.2 347.8 229.7 260.1 318.2
print(paste0("i=",it,", ",round(CFRs100k_years1[[i]][it],1),",", round(CFRs100k_years[[i]][it],1),",",round(CFRs100k_years2[[i]][it],1)))
}
#Overall CFR and UI - derived
CFRs100k[i]   <- mean(CFRs100k_years[[i]]) #[1] 113.6667
ns = length(CFRs100k_years[[i]])
SE = sd(CFRs100k_years[[i]])/sqrt(ns)
CFRs100k1[i]  <- CFRs100k[i] - qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 93.98868
CFRs100k2[i]  <- CFRs100k[i] + qt(.95,ns-1,lower.tail=TRUE)*SE #[1] 133.3447



###### Summary data-frames

(dmean   <- data.frame( Seasonal_infuenza_study =c(study,study_i),
                     Geography =c(country,country_i),
                     Period    =c(period,period_i),
                     Risk_type =c(rep("sCFR",n),"IFR"),
                     Risk_mean =round(c(CFRs100k,IFR100k),1),
                     LowerU    =round(c(CFRs100k1,IFR100k1),1),
                     UpperU    =round(c(CFRs100k2,IFR100k2),1) )      )

(dseason <- data.frame( Seasonal_infuenza_study = unlist(c(studies,studies_i)),
                     Geography = unlist(c(geograp,geograp_i)),
                     Period    = unlist(c(seasons,seasons_i)),
                     Risk_type = c(rep("sCFR",length(unlist(seasons))),rep("IFR",length(unlist(seasons_i)))),
                     Risk_mean = round(unlist(c(CFRs100k_years,IFR100k_years)),1),
                     LowerU    = round(unlist(c(CFRs100k_years1,IFR100k_years1)),1),
                     UpperU    = round(unlist(c(CFRs100k_years2,IFR100k_years2)),1) )      )

## pdf: data frame
pdf(file = paste0(output_dir,"/table_sCFR-IFR_av-seasons.pdf"))#,paper="a4r") 
gridExtra::grid.table(dmean, theme = ttheme_default(base_size = 8))
dev.off()
pdf(file = paste0(output_dir,"/table_sCFR-IFR_by_season.pdf"))#,paper="a4r") 
gridExtra::grid.table(dseason, theme = ttheme_default(base_size = 5, padding = unit(c(2, 2), "mm") )) #padding = unit(1, "mm") )
dev.off()


###Confidence intervals of ratios
### (if assumed numerator and denominator variables awere normal with given CI, the ratio would be a Cauchy)
### Estimationof UIs from the UIs of numerator and denominator:
### package: pairwiseCI
### pairwiseCI::MOVERR : MOVER-R method by Donner and Zhou (2012). 
### Description: Compute confidence intervals for the ratio (theta1/theta0) of two parameters based 
### on point estimates and confidence intervals for the two parameters, theta1 and theta0.

