#### sCFR Review Paper - FIGURES
#### 1-Fig-table of seasonal sCFR and (for comparison) IFR
#### 2-Fig of pandemic sCFR

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggh4x)

TODAY <- format(Sys.Date(), "%d-%m-%Y")
TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')

#### folder containing figure outputs
output_dir <- paste0(getwd(),"/output") #getwd()


#### Read data #################################################################
df0  <- read.csv(paste0(getwd(),"/data/studies_seasonal-sCFR-IFR.csv"))
#
exclude = c("Global","Europe", "Asia", "North America")
#df seasonal cfr - each season and their mean
df   <- df0 %>% filter(type=='s_cfr' & !is.element(Geography, exclude) & mean=="n")
dfm  <- df0 %>% filter(type=='s_cfr' & !is.element(Geography, exclude) & mean=="y")
#df seasonal ifr - each season and their mean
# for comparing with scfr
dfi  <- df0 %>% filter(type=='s_ifr' & !is.element(Geography, exclude) & mean=="n")
dfim <- df0 %>% filter(type=='s_ifr' & !is.element(Geography, exclude) & mean=="y")
#df pandemic cfr
dfp  <- df0 %>% filter(type=='p_cfr' & !is.element(Geography, exclude) & mean=="n")

#### Period
p    =df$Period
p_m  =dfm$Period
pi   =dfi$Period
pi_m =dfim$Period
pp   =dfp$Period

#### Geography
y    =df$Geography
y_m  =dfm$Geography
yi   =dfi$Geography;
yi_m =dfim$Geography;
yp   =dfp$Geography

#### CFR='ifr' (sCFR or IFR)
LOG =0;#1 #No, Yes
v    =df$ifr;       if(LOG==1) v    =log10(v)
v_m  =dfm$ifr;      if(LOG==1) v_m  =log10(v_m)   #mean
vi   =dfi$ifr;      if(LOG==1) vi   =log10(vi)    #IFR
vi_m =dfim$ifr;     if(LOG==1) vi_m =log10(vi_m)  #IFR mean
vp   =dfp$ifr;      if(LOG==1) vp   =log10(vp)    #CFR pandemic

#### UI
v1   = df$ifr1;    if(LOG==1) v1    = log10(v1)
v2   = df$ifr2;    if(LOG==1) v2    = log10(v2)
v1_m = dfm$ifr1;   if(LOG==1) v1_m  = log10(v1_m)  #mean
v2_m = dfm$ifr2;   if(LOG==1) v2_m  = log10(v2_m) 
v1i  = dfi$ifr1;   if(LOG==1) v1i   = log10(v1i)   #IFR
v2i  = dfi$ifr2;   if(LOG==1) v2i   = log10(v2i)
v1i_m= dfim$ifr1;  if(LOG==1) v1i_m = log10(v1i_m) #IFR mean
v2i_m= dfim$ifr2;  if(LOG==1) v2i_m = log10(v2i_m)


#### Table preparation #########################################################
#### Col 1 - Geography + Period
yt    <- paste(y,    ", ",p)
yt_m  <- paste(y_m,  ", ", strrep(" ", (nchar(p[1])-2)), " mean")
yit   <- paste(yi,   ", ",pi)
yit_m <- paste(yi_m, ", ", strrep(" ", (nchar(pi[1])-nchar(" mean")))," mean")

#Col 1 - Removing repeated names
#Saudi Arabia, Norway, Italy, USA
#=> fails because some dates are the same, duplicating the factor and invalidating the ordering
#Col 1 - try set order a priori
#yt <- factor(yt, levels=yt, ordered = T) 
#=> fails

#### Geography + Period + Seasons_mean + IFR + IFR_mean
yyy=c(yt,yt_m,yit,yit_m)
yys = sort(yyy,index=T)
iyys=yys$ix #indices of original vector => apply to sort 95%UI vector

#### Col 2 - CFR + UI
vv    <- paste0(round(v),   " [",round(v1),   "-",round(v2),   "]")
vv_m  <- paste0(round(v_m), " [",round(v1_m), "-",round(v2_m), "]")
vvi   <- paste0(round(vi),  " [",round(v1i),  "-",round(v2i),  "]")
vvi_m <- paste0(round(vi_m)," [",round(v1i_m),"-",round(v2i_m),"]")
## add decimals by hand
ii=which(y=="Poland (nationwide)")
vv[ii] = paste0(v[ii]," [",v1[ii],"-",v2[ii],"]")
## set order: CFR + CFR_mean + IFR + IFR_mean (inc UI)
vvv = c(vv,vv_m,vvi,vvi_m)
vvs = vvv[iyys]

#### Data frames for figure
dg  =data.frame(v=v,    v1=v1,    v2=v2,    y=yt,    p=p) #y=y,    p=p)
dgm =data.frame(v=v_m,  v1=v1_m,  v2=v2_m,  y=yt_m,  p=p_m) #y=y_m,  p=p_m)
dgi =data.frame(v=vi,   v1=v1i,   v2=v2i,   y=yit,   p=pi)
dgim=data.frame(v=vi_m, v1=v1i_m, v2=v2i_m, y=yit_m,p=pi_m)
dgp =data.frame(v=vp,   y=yp,  p=pp)
#### Headers
#(Zone- needed for ordering: need header to be last in alphabetic order)
dh  =data.frame(v=c(0),    v1="",    v2="",    y=c(paste("Zone-Geography", strrep(" ", 15), "Period")),    p=c("Period")) #header
vvs = append(vvs,"Risk [95%UI]")
yys$x = append(yys$x,paste("Geography", " ", "Period"))


#### sCRF vertically-tabled plot ###############################################             

#plot features
sz = 2  #shape base size
szleg = 10
colors <- c("seasonal-sCFR"="blue", "seasonal-IFR"="cyan", "mean"="red","head"="black")

p2 <- ggplot() +
  scale_color_manual(values = colors) +
  geom_point(data=dh,   aes(x=v, y=y, group=p),                       size=0,    shape=3, stroke = 0, show.legend = F) + #shape=18
  geom_point(data=dg,   aes(x=v, y=y, group=p, color="seasonal-sCFR"),size=sz-1, shape=3, stroke = 1, show.legend = T) + #shape=18
  geom_point(data=dgi,  aes(x=v, y=y, group=p, color="seasonal-IFR"), size=sz-1, shape=3, stroke = 1, show.legend = T) + #shape=18
  geom_point(data=dgm,  aes(x=v, y=y, group=p, color="mean"),         size=sz,   shape=3, stroke = 2, show.legend = F) +
  geom_point(data=dgim, aes(x=v, y=y, group=p, color="mean"),         size=sz,   shape=3, stroke = 2, show.legend = F) +
  labs(x='Risk per 100,000', y="") + #'y geography'
  guides( y.sec = guide_axis_manual( labels = vvs)) + # requires ggh4x
  guides(color = guide_legend(override.aes = list(size = 4 ))) +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = szleg),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.position="top")
#print
p2
figname = paste0("/figure-table_sCFR-IFR") #paste0("/figure-table_sCFR-IFR_",TODAY)
   pdf(paste0(output_dir,figname,".pdf")); print(p2); dev.off()
ggsave(paste0(output_dir,figname,".png"), p2, device = "png")



#### Pandemic sCFR #############################################################
colors <- c("model"="blue", "seasonal-sCFR"="red", "seasonal-IFR"="violet", "pdm09-sCFR"="blue", "mean"="blue")

pp<- ggplot() +
  scale_color_manual(values = colors) +
  geom_point(data=dgp,  aes(x=v, y=y,color="pdm09-sCFR"), size=sz-1, shape=3, stroke=1, show.legend = T) + #FALSE) +   
  labs(x='Risk per 100,000', y="") +#'geography'
  theme(legend.title=element_blank(),
        legend.text = element_text(size = szleg),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.position="top") +
  guides(color = guide_legend(override.aes = list(size = 2))) 

#print
pp
figname = paste0("/figure_sCFR_pdm09") #paste0("/figure_sCFR_pdm09_",TODAY)
   pdf(paste0(output_dir,figname,".pdf")); print(pp); dev.off()
ggsave(paste0(output_dir,figname,".png"), pp, device = "png")


##summary statistics
print(paste0("seasonal range: ",range(dg$v)[1],", ",range(dg$v)[2],", median: ", median(dg$v)))
#[1] "seasonal range: 0.3, 907.7, median: 124"
print(paste0("pandemic range: ",range(dgp$v)[1],", ",range(dgp$v)[2],", median: ", median(dgp$v)))
#[1] "pandemic range: 0, 1200, median: 12.5"
print(paste0("pandemic (wo outliers 440, 1200) range: ",range(dgp$v[dgp$v<440])[1],", ",range(dgp$v[dgp$v<440])[2],", median: ", median(dgp$v[dgp$v<440])))
#[1] "pandemic (wo outliers 440, 1200) range: 0, 100, median: 10"

#######################################

