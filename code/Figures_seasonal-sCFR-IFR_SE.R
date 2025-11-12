#### sCFR Review Paper - FIGURES
#### 1-Fig2  - table of seasonal sCFR and (for comparison) IFR + SE
#### 2-Fig3  - seasonal sCFR vs socio-economics
#### 3-FigS1 - pandemic sCFR
#### 4-Table - summary stats
#### ggsave png - deactivated

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggh4x)

TODAY <- format(Sys.Date(), "%d-%m-%Y")
TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')

#### folder containing figure outputs
output_dir <- paste0(getwd(),"/output") #getwd()


#### Read data #################################################################################
df0  <- read.csv(paste0(getwd(),"/data/studies_seasonal-sCFR-IFR_SE.csv"))
#
exclude = c("Global","Europe", "Asia", "North America")
#df seasonal cfr - each season and their mean
df   <- df0 %>% filter(type=='s_cfr' & !is.element(Geography, exclude) & mean=="n") #
dfm  <- df0 %>% filter(type=='s_cfr' & !is.element(Geography, exclude) & mean=="y") #
#df seasonal ifr - each season and their mean
## -for comparing with scfr
dfi  <- df0 %>% filter(type=='s_ifr' & !is.element(Geography, exclude) & mean=="n") #
dfim <- df0 %>% filter(type=='s_ifr' & !is.element(Geography, exclude) & mean=="y") #
#df pandemic cfr
dfp  <- df0 %>% filter(type=='p_cfr' & !is.element(Geography, exclude) & mean=="n")
#df 1 pt (mean) per country
dfa <- df0 %>% filter((type=='s_cfr' | type=='s_ifr') & !is.element(Geography, exclude) & (mean=="y" | mean=="S")) #a-mean, s=country with 1-2 obs only

#### Period
p    =df$Period   #
p_m  =dfm$Period  #
pi   =dfi$Period  #
pi_m =dfim$Period #
pp   =dfp$Period

#### Geography
y    =df$Geography   #
y_m  =dfm$Geography  #
yi   =dfi$Geography; #
yi_m =dfim$Geography;#
yp   =dfp$Geography

#### CFR='ifr' (sCFR or IFR)
LOG =0; #1 #No, Yes
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

#SE
unitGDP = 1/1000
digitGDP=0
le     =df$LifeExp
gdp    =round(df$GDPpc*unitGDP,eval(digitGDP))
le_m   =dfm$LifeExp
gdp_m  =round(dfm$GDPpc*unitGDP,eval(digitGDP))
lei    =dfi$LifeExp
gdpi   =round(dfi$GDPpc*unitGDP,eval(digitGDP))
lei_m  =dfim$LifeExp
gdpi_m =round(dfim$GDPpc*unitGDP,eval(digitGDP))

#SE with single point per country
## -arithmetic mean or the single observation 
v_a    =dfa$ifr;
le_a   =dfa$LifeExp
gdp_a  =round(dfa$GDPpc*unitGDP,eval(digitGDP))


#### Table preparation #########################################################################
#### Col 1 - Geography + Period ################################################################
yt    <- paste(y,    ", ",p)
yt_m  <- paste(y_m,  ", ", strrep(" ", (nchar(p[1])-2)), " mean")
yit   <- paste(yi,   ", ",pi)
yit_m <- paste(yi_m, ", ", strrep(" ", (nchar(pi[1])-nchar(" mean")))," mean")

#### Geography + Period + Seasons_mean + IFR + IFR_mean
yyy=c(yt,yt_m,yit,yit_m)
yys = sort(yyy,index=T)
iyys= yys$ix #indices of original vector => apply to sort 95%UI vector

#### space within: "Risk [95%UI]", space, "LE" - before ordering (based on print vv, vv_m..., and then on viewing pdf output)
space=rep("",length(c(v,v_m,vi,vi_m))) #length 51
#v
n1=0;  n2=n1+7; space[(n1+1):n2]   = strrep(" ",5); #7
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",9); #8
n1=n2; n2=n1+6; space[(n1+1):n2]   = strrep(" ",8); #14
n1=n2; n2=n1+3; space[(n1+1):n2]   = strrep(" ",6); #17
n1=n2; n2=n1+2; space[(n1+1):n2]   = strrep(" ",7); #19
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",8); #20
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",10); #21
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",3); #22 netherlands - 3 spaces, shortest
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",6); #23
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",8); #24 
n1=n2; n2=n1+4; space[(n1+1):n2]   = strrep(" ",3); #28 italy        - 3 spaces, shortest
n1=n2; n2=n1+4; space[(n1+1):n2]   = strrep(" ",4); #32
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",2); #33
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",6); #34
n1=n2; n2=n1+2; space[(n1+1):n2]   = strrep(" ",4); #36
n1=n2; n2=n1+4; space[(n1+1):n2]   = strrep(" ",6); #40
L1=1; L2=length(v)
space_v=space[L1:L2]
#vv_m
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",3); #41
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",7); #42
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",3); #43
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",4); #44
L1=L2+1; L2=L2+length(v_m)
space_v_m=space[L1:L2]
#vvi
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",14); #45
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",10); #46
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",10); #47
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",10); #48
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",8); #49
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",8); #50
L1=L2+1; L2=L2+length(vi)
space_vi=space[L1:L2]
#vvi_m
n1=n2; n2=n1+1; space[(n1+1):n2]   = strrep(" ",10); #51
L1=L2+1; L2=L2+length(vi_m)
space_vi_m=space[L1:L2]


#### Col 2 - CFR + UI + LifeExp + GDP ##########################################################
vv    <- paste0(round(v),   " [",round(v1),   "-",round(v2),   "]", space_v,    le,    strrep(" ",7), gdp)
vv_m  <- paste0(round(v_m), " [",round(v1_m), "-",round(v2_m), "]", space_v_m,  le_m,  strrep(" ",7), gdp_m)
vvi   <- paste0(round(vi),  " [",round(v1i),  "-",round(v2i),  "]", space_vi,   lei,   strrep(" ",7), gdpi)
vvi_m <- paste0(round(vi_m)," [",round(v1i_m),"-",round(v2i_m),"]", space_vi_m, lei_m, strrep(" ",7), gdpi_m)

## add decimals by hand (not rounded)
ii=which(y=="Poland (nationwide)")
vv[ii] = paste0(v[ii]," [",v1[ii],"-",v2[ii],"]", space_v[ii], le[ii], strrep(" ",7), gdp[ii])
## set order: CFR + CFR_mean + IFR + IFR_mean (inc UI)
vvv = c(vv,vv_m,vvi,vvi_m)
vvs = vvv[iyys]


#### Data frames for FIG 2, FIG S1, Table
# scfr studies and study-over-season-means, seasonal
dg  =data.frame(v=v,    v1=v1,    v2=v2,    y=yt,    p=p) 
dgm =data.frame(v=v_m,  v1=v1_m,  v2=v2_m,  y=yt_m,  p=p_m) 
# ifr study and study-mean, seasonal
dgi =data.frame(v=vi,   v1=v1i,   v2=v2i,   y=yit,   p=pi)
dgim=data.frame(v=vi_m, v1=v1i_m, v2=v2i_m, y=yit_m, p=pi_m)
# scfr pandemic
dgp =data.frame(v=vp,   y=yp,  p=pp)
#### Headers
#(Zone- needed for ordering: need header to be last in alphabetic order)
dh  =data.frame(v=c(0),    v1="",    v2="",    y=c(paste("Zone-Geography", strrep(" ", 15), "Period")),    p=c("Period")) #header
vvs = append(vvs,  paste("Risk [95%UI]", "  ", "LE", "  ", "GDPpc"))
yys$x = append(yys$x,paste("Geography", " ", "Period"))



#### FIG 2 #### sCRF vertically-tabled plot ###################################################################                 
##plot features
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
##pdf, png
   figname = paste0("/figure-table_sCFR-IFR_SE_",TODAY)
   pdf(paste0(output_dir,figname,".pdf")); print(p2); dev.off()
   #ggsave(paste0(output_dir,figname,".png"), p2, device = "png")

dev.off()



#### FIG 3 #### SE plots ######################################################################################
##TODO: could make ggplots
##pdf
  pdf(paste0(output_dir,"/figure_sCFR-LE-GDPpc_mean-vs-raw-data_",TODAY,".pdf"))

par(mfrow=c(2,3))
##text position
grange=range(c(gdp,gdp_m,gdpi,gdpi_m)); 
lrange=range(c(le,le_m,lei,lei_m));
vrange=range(c(v,  v_m, vi, vi_m));
xg = min(grange) + 0.80*diff(grange);
yg = min(grange) + 0.95*diff(grange);
xl = min(lrange) + 0.80*diff(lrange);
yl = min(lrange) + 0.95*diff(lrange);
xv = min(vrange) + 0.80*diff(vrange);
yv = min(vrange) + 0.95*diff(vrange);
##colours
c0="black"
c1="blue"
c2="red"
c3="cyan"
#arithmetic vs raw data
for (i in 1:2){ #panels
if (i==1) {gdp_c= gdp_a; v_c=v_a; le_c=le_a; } else {
           gdp_c= c(gdp, gdpi); v_c=c(v,vi); le_c=c(le,lei); }
### a, d
plot  (gdp_c, v_c,    col=c1, pch=3, ylab ="sCFR per 100,000", xlab ="GDP per capita (1000 US$)", xlim=grange, ylim=vrange) #, main=title)
if (i==1) {abline(lm (v_c ~ gdp_c),col=c1, pch=3)
          text(x = xg, y = yv, label = paste("p =", round(summary(lm (v_c ~ gdp_c))$coefficients[8],2))) 
		  title("a    Mean",     adj = 0)} else {
		  title("d    All data", adj = 0)}
### b, e
plot  (le_c,  v_c,    col=c1, pch=3, ylab ="", xlab ="Life expectancy from birth (years)", xlim=lrange, ylim=vrange)
if (i==1) {abline(lm (v_c ~ le_c), col=c1, pch=3)
          text(x = xl, y = yv, label = paste("p =", round(summary(lm (le_c ~ v_c))$coefficients[8],2))) 
		  title("b", adj = 0)} else {
		  title("e", adj = 0)}
### c
### i=2: dont plot, multiple pts have same gdp and le; only changes the p-value of regression, but regression not suitable here
if (i==1) {
plot  (gdp_c, le_c,   col=c1, pch=3, xlab ="GDP per capita (1000 US$)", ylab ="Life expectancy from birth (years)", xlim=grange, ylim=lrange) }
if (i==1) {abline(lm (le_c ~ gdp_c),  col=c1, pch=3)
          text(x = xg, y = yl, label = paste("p =", round(summary(lm (le_c ~ gdp_c))$coefficients[8],2)))
		  title("c", adj = 0)} 
} #panels
dev.off()



#### FIG S1 #### Pandemic sCFR ################################################################################
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

##print
pp
##pdf, png
figname = paste0("/figure_sCFR_pdm09_",TODAY) #paste0("/figure_sCFR_pdm09_",TODAY)
   pdf(paste0(output_dir,figname,".pdf")); print(pp); dev.off()
   #ggsave(paste0(output_dir,figname,".png"), pp, device = "png")



##### summary statistics
##print
print(paste0("seasonal range: ",range(dg$v)[1],", ",range(dg$v)[2],", median: ", median(dg$v)))
#[1] "seasonal range: 0.3, 907.7, median: 124"
print(paste0("pandemic range: ",range(dgp$v)[1],", ",range(dgp$v)[2],", median: ", median(dgp$v)))
#[1] "pandemic range: 0, 1200, median: 12.5"
print(paste0("pandemic (wo outliers 440, 1200) range: ",range(dgp$v[dgp$v<440])[1],", ",range(dgp$v[dgp$v<440])[2],", median: ", median(dgp$v[dgp$v<440])))
#[1] "pandemic (wo outliers 440, 1200) range: 0, 100, median: 10"

##pdf data frame
(drange   <- data.frame( scfr_infuenza =c("seasonal", "pandemic 2009", "idem, exc 2 outliers"),
                         Range_min = round(c(range(dg$v)[1],range(dgp$v)[1], range(dgp$v[dgp$v<440])[1]),1),
                         Range_max = round(c(range(dg$v)[2],range(dgp$v)[2], range(dgp$v[dgp$v<440])[2]),1),
                         Q1 = round(c(quantile(dg$v,1/4)[1],quantile(dgp$v,1/4)[1], quantile(dgp$v[dgp$v<440],1/4)[1]),1),
                         Q3 = round(c(quantile(dg$v,3/4)[1],quantile(dgp$v,3/4)[1], quantile(dgp$v[dgp$v<440],3/4)[1]),1),
                         Q2 = round(c(quantile(dg$v,2/4)[1],quantile(dgp$v,2/4)[1], quantile(dgp$v[dgp$v<440],2/4)[1]),1))    )

##pdf
  pdf(file = paste0(output_dir,"/table_sCFR-range_seasonal-and-pdm09_",TODAY,".pdf"))#,paper="a4r") 
  gridExtra::grid.table(drange, theme = ttheme_default(base_size = 8))   #padding = unit(c(2, 2), "mm") ))
  dev.off()


##############################################################################################
