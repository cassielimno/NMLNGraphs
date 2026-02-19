#looking at trends in 2024 nmln data
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(grid) 
library(tayloRswift)
library(cowplot)
library(patchwork)
library(purrr)

setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/NMLN analysis/SummaryScriptsandData")


#upload trend data
trends<- read.csv("NMLNNutrientTrendData2024.csv")

#THIS IS THE COMPLETED DATA for 2024
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
data<- read.csv("HydroShareFinalData2007-2024_cr12726.csv")
#NEXT YEAR MAKE SURE THIS ALSO INCLUDES TALLY LAKE IT IS GETTING FILTERED OUT ####
nmln<- data %>% filter(Project_ID == "NMLN")

nmln$Activity_Start_Date<-as.POSIXct(nmln$Activity_Start_Date)
nmln<-nmln %>% mutate(year = year(Activity_Start_Date), 
                      month = month(Activity_Start_Date), day = yday(Activity_Start_Date))
nmln<- nmln %>% mutate(monthname = month.abb[month])
nmln$Result_Value<-as.numeric(nmln$Result_Value)


#make a col that is nmln common lake names  NOTE THIS ONLY WORKS FOR NMLN LAKES NOT STREAMS
nmln<- nmln %>% mutate(lakename = case_when(grepl("ABBOT", Station_ID) ~ "Abbot",
                                            grepl( "ASHLEY-E", Station_ID) ~ "Ashley Lake East",
                                            grepl("ASHLEY-W", Station_ID) ~  "Ashley Lake West",
                                            grepl("BAILEY", Station_ID) ~  "Bailey",
                                            grepl("BEAVER", Station_ID) ~  "Beaver",
                                            grepl("BIG-THERR", Station_ID) ~  "Big Therriault",
                                            grepl("BLAINE", Station_ID) ~  "Lake Blaine",
                                            grepl( "BLANCH", Station_ID) ~ "Blanchard",
                                            grepl("BOOTJACK", Station_ID) ~  "Bootjack",
                                            grepl("BULL", Station_ID) ~ "Bull Lake",
                                            grepl("DICKEY", Station_ID) ~ "Dickey",
                                            grepl( "DOLLAR", Station_ID) ~  "Dollar",
                                            grepl("ECHO", Station_ID) ~ "Echo",
                                            grepl("FISH", Station_ID) ~ "Fish",
                                            grepl( "FH-SKIDOO", Station_ID) ~ "Flathead Lake Skidoo Bay",
                                            grepl("FH-INDIAN", Station_ID) ~ "Flathead Lake Indian Bay" ,
                                            grepl( "FH-WOODS", Station_ID) ~ "Flathead Lake Woods Bay",
                                            grepl("FH-CONRAD", Station_ID) ~ "Flathead Lake Conrad Point",
                                            grepl("FH-CRESC", Station_ID) ~ "Flathead Lake Cresc",
                                            grepl("FH-MACK", Station_ID) ~ "Flathead Lake Mackinaw Alley",
                                            grepl("FH-SOMERS", Station_ID) ~ "Flathead Lake Somers Bay",
                                            grepl("FH-YELLOW", Station_ID) ~ "Flathead Lake Yellow Bay",
                                            grepl("FH-MARCO", Station_ID) ~ "Flathead Lake Marco",
                                            grepl("FH-MISS", Station_ID) ~ "Flathead Lake Miss",
                                            grepl("FH-DAYT", Station_ID) ~ "Flathead Lake Dayt",
                                            grepl("FH-WAYF", Station_ID) ~ "Flathead Lake Wayf",
                                            grepl("FOY", Station_ID) ~ "Foy",
                                            grepl("GLEN", Station_ID) ~ "Glen",
                                            grepl("HALFM", Station_ID) ~ "Halfmoon" ,
                                            grepl("HANS-DOY", Station_ID) ~ "Hanson-Doyle" ,
                                            grepl("HOLLAND", Station_ID) ~ "Holland" ,
                                            grepl("JETTE", Station_ID) ~ "Jette",
                                            grepl("LAKEOW", Station_ID) ~ "Lake ow",
                                            grepl("LAKEFI", Station_ID) ~ "Five" ,
                                            grepl("LINDBERGH", Station_ID) ~ "Lindbergh" ,
                                            grepl("LITT-BITT", Station_ID) ~  "Little Bitterroot",
                                            grepl("LOON", Station_ID) ~ "Loon Lake",
                                            grepl("LOSTCOO", Station_ID) ~ "Lost Loon",
                                            grepl("LOWER-STILL", Station_ID) ~ "Lower Stillwater" ,
                                            grepl("MARYRON-E", Station_ID) ~ "Mary Ronan East" ,
                                            grepl("MARYRON-W", Station_ID) ~ "Mary Ronan West" ,
                                            grepl("MCGILV", Station_ID) ~ "McGilvray" ,
                                            grepl("MURPHY", Station_ID) ~ "Murphy" ,
                                            grepl("MURRAY", Station_ID) ~ "Murray" ,
                                            grepl( "PETERS", Station_ID) ~ "Peterson",
                                            grepl("ROGERS", Station_ID) ~ "Rogers",
                                            grepl("SKYLES", Station_ID) ~ "Skyles" ,
                                            grepl("SMITH", Station_ID) ~ "Smith Lake",
                                            grepl("SOPHIE", Station_ID) ~ "Sophie" ,
                                            grepl("SPENCER", Station_ID) ~ "Spencer" ,
                                            grepl( "SWAN-S", Station_ID) ~ "Swan Lake South",
                                            grepl("SWAN-N", Station_ID) ~ "Swan Lake North" ,
                                            grepl("SWAN-L", Station_ID) ~ "Swan Lake",
                                            grepl("TALLY", Station_ID) ~ "Tally",
                                            grepl("TETRAU", Station_ID) ~ "Tetrault" ,
                                            grepl( "UPP-STILL", Station_ID) ~ "Upper Stillwater",
                                            grepl("UPP-WF", Station_ID)~ "Upper Whitefish",
                                            grepl("WF-LK-IP1",  Station_ID) ~ "Whitefish" ,
                                            .default = "uhoh"
))


#check this
unique(nmln$lakename)

#the making of trend data
#lets make T and N trend graphs ####


#make a function to extract p values
my_lm<-lm(Result_Value ~ year, data = nmln %>% filter(Characteristic_ID == "TN", Station_ID == "ECHO"))


overall_p <- function(my_lm) {
  f <- summary(my_lm)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}



overall_p(my_lm)
summary(my_lm)






#this code puts in all non-detects as the method detection limit
nmln2<- nmln %>% 
  mutate(result_nd = case_when(Result_Detection_Condition == "Not Detected" ~ nmln$Method_Detection_Limit_Value,
                               Result_Detection_Condition == "" ~ nmln$Result_Value,
                               is.na(Result_Detection_Condition) ~ nmln$Result_Value)) 




#take out all the sites with no TN data or too little TN data (there is deff an easier way but this is how i did it)
nmln2<- filter(nmln2, !Station_ID == "BULL", !Station_ID == "FH-CRESC", 
               !Station_ID == "FH-DAYT", !Station_ID == "FH-MARCO", !Station_ID == "FH-MISS", 
               !Station_ID == "FH-WAYF", !Station_ID == "LAKEOW", !Station_ID == "SWAN-L")


unique(nmln2$Station_ID)


#make nmln only nutrients 

nmln2<- filter(nmln2, Characteristic_ID == "TN" | Characteristic_ID == "TP" | Characteristic_ID == "CHL-A-CP")



#make loop that works for all characteristics
#make an empty dataframe
nmln.stats<-data.frame()

#for loop to extract trend and p value for each lake using results that include non-detects
for (i in unique(nmln2$Station_ID)) {
  
  #make object called station ID that is Station ID to help with merging
  Station_ID<- i
  
  #add another loop for characteristic ID
  for (j in unique(nmln2$Characteristic_ID)) { 
    
    #make object for characteristic ID
    Characteristic_ID<- j
    
    #run lm
    my_lm<-lm(result_nd ~ year, data = nmln2 %>% filter(Characteristic_ID == j, Station_ID == i))
    
    #extract coeffcients
    cf<-coef(my_lm)
    
    #extract slope from coeffcients
    slope<- as.numeric(cf[2])
    
    #extract p using function made above
    p <- overall_p(my_lm)
    
    
    #r-squared
    r <- summary(my_lm)$r.squared
    
    #make a stats data frame for i
    stats<-data.frame(p, slope, r, Station_ID, Characteristic_ID)
    
    #if it gets stuck to see which lake is tripping it up
    print(i)
    
    #combine each stats dataframe
    nmln.stats<- rbind(stats, nmln.stats)
    
    
  } }


#make final dataframe with stats attached
nmln3<-merge(x = nmln.stats, y = nmln2, by = c("Station_ID", "Characteristic_ID"))

nmln3$year<-as.numeric(nmln3$year)
#now filter for p values that are significant AND just nitrogen data
tn.sig.nmln<-nmln3 %>% filter(p< 0.05, Characteristic_ID == "TN")
#see how many
unique(tn.sig.nmln$Station_ID)

#tp graphs
tp.sig.nmln<-nmln3 %>% filter(p< 0.05, Characteristic_ID == "TP")
#see how many
unique(tp.sig.nmln$Station_ID)



#now chl sig graphs
chl.sig.nmln<-nmln3 %>% filter(p< 0.05, Characteristic_ID == "CHL-A-CP")
#see how many
unique(chl.sig.nmln$Station_ID)




#lets try doing corr and PCA analysis #####
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(ggfortify)
library(GGally)
library(corrplot)
library(DescTools)
library(missMDA)


#format data so it will work

#pivot wider
nmln.pca<- pivot_wider(nmln, names_from = Characteristic_ID, values_from = Result_Value) %>% clean_names()
glimpse(nmln.pca)

#create anoxia col 
nmln.pca<- nmln.pca %>% group_by(station_id, year) %>% mutate(low.do = case_when(result_depth_height_measure == max(result_depth_height_measure, na.rm = TRUE) ~ do),
                                                              max.temp = max(temp_w, na.rm =TRUE))

max(nmln.pca$do)

#add in an area col ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/NMLN analysis/SummaryScriptsandData")
# #read in dataframe that includes lake area, lat and long
# gis<-read.csv("NMLNLakeTN2022_2.csv")
# 
# #select for cols you want
# gis1<- gis %>% select(Sttn_ID, AREA, PERIMET, HUC_8_d, Longitd, Latitud, COUNTY, EPA_ECO, ELEV, FSHREG)
# 
# #delete duplicates
# gis1<-gis1[!duplicated(gis1), ]
# 
# #filter out lakes that should not be there --- this is bad gis data fml
# gis2<- gis1 %>% filter(COUNTY == "FLATHEAD" | COUNTY == "LAKE" | COUNTY == "LINCOLN" |
#                          COUNTY == "MISSOULA") %>% filter(AREA > 404)
# 
# #gonna export to clean up the duplicates in excel 
# write.csv(gis2, "NMLNGISData.csv")

#re-uploading fixed data
edit<- read.csv("NMLNGISDataFinal.csv")

#remove tally and whitefish
edit<-edit %>% filter(!(Sttn_ID == "TALLY" | Sttn_ID == "WF-LK-IP1"))


#NOW MOVE ON TO OTHER DATAFRAME WE WILL MERGE LATER ####
#clean dataframe
nmln.pca2<- nmln.pca %>% select(station_id, activity_start_date,
                               lakename, tn, chl_a_cp, tp, bar_pressure, max.temp,
                               sc, total_alk, x7440_70_2, year, low.do)


nmln.pca3<- nmln.pca2 %>% group_by(station_id, activity_start_date) %>% 
  mutate(tn.y = sum(tn, na.rm = TRUE), tp.y = sum(tp, na.rm = T), chl_a_cp.y = sum(chl_a_cp, na.rm = T),
         bar_pressure.y = mean(bar_pressure, na.rm = T), max.temp.y = Mode(max.temp, na.rm = T),
         sc.y = mean(sc, na.rm = T), total_alk.y = mean(total_alk, na.rm = T), 
         x7440_70_2.y = mean(x7440_70_2, na.rm = TRUE), low.do.y = mean(low.do, na.rm = T))
#do for all vars then select only new cols and then delete duplicates

#select out only new cols
nmln.pca4<- nmln.pca3 %>% select(station_id, activity_start_date,
                                 lakename, tn.y, chl_a_cp.y, tp.y, max.temp.y,
                                 sc.y,  year, low.do.y)
#remove duplicates
nmln.pca5<-nmln.pca4[!duplicated(nmln.pca4), ]

#remove NaN values
# nmln.pca6 <- nmln.pca5[!is.nan(nmln.pca5), ]
# 
# nmln.pca5<- na.omit(nmln.pca5)
# 
# #make NaN = NA
# nmln.pca6<- nmln.pca5 %>% mutate(x7440_70_2.y = case_when(x7440_70_2.y == "NaN" ~ "", 
#                                                           .default = as.character(x7440_70_2.y)))
# 
# nmln.pca6$x7440_70_2.y<-as.numeric(nmln.pca6$x7440_70_2.y)

# Convert infinite values to NA
nmln.pca5$max.temp.y<-sub(-Inf, NA, nmln.pca5$max.temp.y)

nmln.pca5$low.do.y<-sub(Inf, NA, nmln.pca5$low.do.y)

glimpse(nmln.pca5)

#merge with other dataframe that had gis data #####

#try to merge with other data
#filter old sites out of nmln.pca
nmln.edit<- nmln.pca5 %>% filter(!(station_id == "BULL" | station_id == "FH-CRESC" | 
                                    station_id == "FH-DAYT" | station_id == "FH-MARCO" |
                                    station_id == "FH-MISS" | station_id == "FH-WAYF" | 
                                    station_id == "LAKEOW" | station_id == "SWAN-L" ))
nmln.edit$station_id<-sub("LOSTCOO", "LOSTLOON", nmln.edit$station_id)

#check the ids
unique(edit$Sttn_ID)
unique(nmln.edit$station_id)

#rename station id col in edit
edit<- edit %>%  rename(station_id = Sttn_ID)
edit<- edit %>% mutate(station_id2 = station_id)

#merge
nmlnfinal <- inner_join(nmln.edit %>% distinct(station_id, .keep_all = TRUE),
                 edit %>% distinct(station_id, .keep_all = TRUE),by="station_id")

#check this
unique(nmlnfinal$station_id)

unique(nmln.pca$station_id)

#check for duplicates
check<-nmlnfinal[duplicated(nmlnfinal), ]

#add trend data ####
  
  #check for duplicates
  nmln.stats2<-nmln.stats[!duplicated(nmln.stats), ]


#pivot wider
nmln.stats3<- pivot_wider(nmln.stats2, names_from = Characteristic_ID, values_from = c(p,slope,r)) %>% clean_names()
glimpse(nmln.pca)

nmln.stats3$station_id<-sub("LOSTCOO", "LOSTLOON", nmln.stats3$station_id)

#merge 
nmlnfinal1 <- inner_join(nmlnfinal %>% distinct(station_id, .keep_all = TRUE),
                        nmln.stats3 %>% distinct(station_id, .keep_all = TRUE),by="station_id")



#now make correlations #####
nmlnfinal1$max.temp.y<- as.numeric(nmlnfinal1$max.temp.y)

nmlnfinal1$low.do.y<- as.numeric(nmlnfinal1$low.do.y)

nmlnfinal1$HUC_8_d<- as.numeric(nmlnfinal1$HUC_8_d)

nmlnfinal2<-nmlnfinal1 %>% select(-X)

glimpse(nmlnfinal2)

#explore correlations
cor.nmln1<-cor(nmlnfinal2[,c(4:15,18, 21:29)],  use="pairwise.complete.obs")#number select the varibles you want to apply it to
#make corr plot
corrplot(cor.nmln1, type = "lower")
# use this website to make it prettier https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html ######


#gives corr coeff plus shows direct relationships 
#THIS TAKES A WHILE
ggpairs(nmlnfinal2[,c(4:15,18, 21:29)])


#edit down varibles to make a better plot
nmlncut<-nmlnfinal2 %>% select(station_id, tn.y, chl_a_cp.y, tp.y, max.temp.y, sc.y, 
                               low.do.y, AREA, slope_tp, slope_chl_a_cp, slope_tn)

#rename for clarity
nmlncut<- nmlncut %>% rename(TN = tn.y, TP = tp.y, CHL = chl_a_cp.y, MaxTemp = max.temp.y,
                             SC = sc.y, LowDO = low.do.y, Area = AREA, Slope_TP = slope_tp,
                             Slope_TN = slope_tn, Slope_CHL = slope_chl_a_cp)

#now plot this
cor.nmlncut<-cor(nmlncut[,3:12],  use="pairwise.complete.obs")#number select the varibles you want to apply it to
#make corr plot
corrplot(cor.nmlncut, type = "lower")

#SPECIAL corr plot
testRescut = cor.mtest(nmlncut[,3:12], conf.level = 0.95)

#adds numbers for correlations that are significant
corrplot(cor.nmlncut, p.mat = testRescut$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 1.2, order = 'AOE', diag=FALSE)

#do corr plot with just lakes that are in the slide show (lakes with strongest trends) #####
trendlakes<- nmlnfinal2 %>% filter(station_id == "MCGILV" | station_id == "BLAINE" |
                                     station_id == "LOW-STILL" | station_id == "SKYLES" |
                                     station_id == "BOOTJACK" | station_id == "LITT-BITT" |
                                     station_id == "TETRAU" | station_id == "FISH")




#now do a corr plot on these
cor.trends<-cor(trendlakes[,c(4:15, 18, 21:29)],  use="pairwise.complete.obs")#number select the varibles you want to apply it to
#make corr plot
corrplot(cor.trends, order = 'hclust', addrect = 2)

#SPECIAL corr plot
testRes = cor.mtest(trendlakes[,c(4:15, 18, 21:29)], conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(cor.trends, p.mat = testRes$p, sig.level = 0.10, type = "lower")



#on edited down varibles 
trendlakescut <- nmlncut%>% filter(station_id == "MCGILV" | station_id == "BLAINE" |
                                     station_id == "LOW-STILL" | station_id == "SKYLES" |
                                     station_id == "BOOTJACK" | station_id == "LITT-BITT" |
                                     station_id == "TETRAU" | station_id == "FISH")



#SPECIAL corr plot
testRestrendcut = cor.mtest(trendlakescut[,3:12], conf.level = 0.95)

cortrendscut<-cor(trendlakescut[,3:12],  use="pairwise.complete.obs")

#adds numbers for correlations that are significant
corrplot(cortrendscut, p.mat = testRestrendcut$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 1.2, order = 'AOE', diag=FALSE)







































#start with the PCA ##### THIS CODE IS MESSY AND MAY NOT WORK

#because we ahve na's in the data this creates realtistic false values for teh NAs so we can
#run a PCA analysis
pcadata<-imputePCA(nmln.pca5[,4:11])

#select only numerical varibles it doent work on characters
nmln.pca.res<-PCA(pcadata$completeObs, scale=T, graph=F)#means dont want graph


nmln.pca.res$eig#gives out list, $ to get specific values
#plot the points
plot(nmln.pca.res, label = 'none')#shows 70% of the variance

                 #getting the eigan values
eig.val<- get_eigenvalue(nmln.pca.res)
eig.val



#cos2 is the quality of representation of the columns and rows
nmln.pca.res$var$cor#deminsion one and run corr back to org data, shows why things
#look the way they do
nmln.pca.res$var$contrib#how much of these is made up by this
#what is the most driving dimension??? -- dem 1
nmln.pca.res$eig
#shows which components drive each, comp 1 and 2 are most important

#scree plot
fviz_eig(nmln.pca.res)


#correlation circle
fviz_pca_var(nmln.pca.res, axes = c(1,2), repel = T)
#tells how each of these varibles end up being broadcasted, think of points b4
?fviz_pca_var

#better description of those axis
nmln.desc<-dimdesc(nmln.pca.res,
                   axes = 1:5,
                   proba = 0.05)
nmln.desc
#another way to look at correlation coeff
nmln.desc$Dim.1
nmln.desc$Dim.2


#individuals mapped to PCA space------>could run this with DOC indivuals and look at time scale 
#to see if DOC drivers change over time   WHAT CAN YOU CONCLUDE FROM THIS PLOT???? WHAT DO THE POINTS MEAN
fviz_pca_ind(nmln.pca.res,
             col.ind = nmln.pca5$year,#can also add factor in here to split it up by factor()
             label = 'none')
fviz_pca_var(chem.pca)
#biplot puts both plots on top of each other 
fviz_pca_biplot(nmln.pca.res,
                col.ind = nmln.pca5$year,
                label = 'var',
                repel = T)

#a really nice summary
Investigate(nmln.pca.res)



























