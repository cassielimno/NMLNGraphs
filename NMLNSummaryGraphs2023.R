#nmln example graphs for 2023
#making summary stats for NMLN 2022 report
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(grid) 
library(tayloRswift)
library(cowplot)
library(patchwork)
library(forcats)

#load theme
mlc_theme <- theme(
  axis.title.x=element_text(size=14, face="bold", colour = "black"),
  axis.title.y=element_text(size=14, face="bold", colour = "black"),
  axis.text.x = element_text(size=12, face="bold", angle=90, hjust=1, colour = "black"),
  axis.text.y = element_text(size=12, face="bold", colour = "black"),
  legend.text = element_text(colour="black", size = 11, face = "bold"),
  legend.title = element_text(colour="black", size=11, face="bold"),
  legend.position= "right", 
  axis.line.x = element_line(color="black", linewidth  = 0.3),
  axis.line.y = element_line(color="black", linewidth  = 0.3),
  panel.border = element_rect(colour = "black", fill=NA, size=0.3),
  title = element_text(size = 12, face = "bold"))
#THIS IS THE COMPLETED DATA WITH 2022 AND SECCHI AND TURB ADDED USE THIS
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
data.now<-read.csv("HydroShareDataFinal2022.csv")
data.now2<-read.csv("HydroShareFinalALL2023.csv")
data.now2<- data.now2 %>% filter(Project_ID == "NMLN")
data.now2$Activity_Start_Date<-as.POSIXct(data.now2$Activity_Start_Date)
data.now2<-data.now2 %>% mutate(year = year(Activity_Start_Date), 
                                month = month(Activity_Start_Date), day = yday(Activity_Start_Date))
data.now2<- data.now2 %>% mutate(monthname = month.abb[month])
data.now2$Result_Value<-as.numeric(data.now2$Result_Value)


#make a col that is nmln common lake names  NOTE THIS ONLY WORKS FOR NMLN LAKES NOT STREAMS
data.now2<- data.now2 %>% mutate(lakename = case_when(grepl("ABBOT", Station_ID) ~ "Abbot",
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
                                            grepl( "FH-SKIDOO", Station_ID) ~ "Flathead lake Skidoo Bay",
                                            grepl("FH-INDIAN", Station_ID) ~ "Flathead lake Indian Bay" ,
                                            grepl( "FH-WOODS", Station_ID) ~ "Flathead lake Woods Bay",
                                            grepl("FH-CONRAD", Station_ID) ~ "Flathead lake Conrad Point",
                                            grepl("FH-CRESC", Station_ID) ~ "Flathead lake Cresc",
                                            grepl("FH-MACK", Station_ID) ~ "Flathead lake Mackinaw Alley",
                                            grepl("FH-SOMERS", Station_ID) ~ "Flathead lake Somers Bay",
                                            grepl("FH-YELLOW", Station_ID) ~ "Flathead lake Yellow Bay",
                                            grepl("FH-MARCO", Station_ID) ~ "Flathead lake Marco",
                                            grepl("FH-MISS", Station_ID) ~ "Flathead lake Miss",
                                            grepl("FH-DAYT", Station_ID) ~ "Flathead lake Dayt",
                                            grepl("FH-WAYF", Station_ID) ~ "Flathead lake Wayf",
                                            grepl("FOY", Station_ID) ~ "Foy",
                                            grepl("GLEN", Station_ID) ~ "Glen",
                                            grepl("HALFM", Station_ID) ~ "Halfmoon" ,
                                            grepl("HANS-DOY", Station_ID) ~ "Hanson-Doyle" ,
                                            grepl("HOLLAND", Station_ID) ~ "Holland" ,
                                            grepl("JETTE", Station_ID) ~ "Jette",
                                            grepl("LAKEOW", Station_ID) ~ "Lake ow",
                                            grepl("LAKEFI", Station_ID) ~ "Five" ,
                                            grepl("LINDBERGH", Station_ID) ~ "Lindbergh" ,
                                            grepl("LITT-BITT", Station_ID) ~  "Little Bitteroot",
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

#filter out streams
data.now2<- data.now2 %>% filter(Station_Type == "Lake")
#start by making groups for small med and large lakes ####
small<-data.now2 %>% filter(lakename == "Abbot" | lakename ==  "Bailey" |lakename == "Big Therriault" |
                              lakename == "Bootjack" | lakename == "Dollar" | lakename == "Fish" |
                              lakename == "Halfmoon" | lakename == "Hanson-Doyle" | lakename == "Jette" |
                              lakename == "Loon Lake" | lakename == "Lost Loon" | lakename == "McGilvray" |
                              lakename == "Murray" | lakename == "Peterson" | lakename == "Skyles" |
                              lakename == "Smith Lake" | lakename == "Spencer" | lakename == "Tetrault" |
                              lakename == "Upper Whitefish")

unique(small$lakename)

med <- data.now2 %>% filter(lakename == "Beaver" | lakename == "Lake Blaine" | lakename == "Blanchard" |
                              lakename == "Five" | lakename == "Foy" | lakename == "Glen" |
                              lakename == "Holland" | lakename == "Lower Stillwater" | lakename == "Murphy" |
                              lakename == "Rogers" | lakename == "Sophie")

unique(med$lakename)


large<- data.now2 %>% filter(lakename == "Ashley Lake East"  |lakename == "Ashley Lake West" | lakename == "Dickey" |
                               lakename == "Echo" | lakename == "Flathead lake Conrad Point" | lakename == "Flathead lake Indian Bay" |
                               lakename == "Flathead lake Mackinaw Alley" | lakename == "Flathead lake Skidoo Bay" |
                               lakename == "Flathead lake Somers Bay" | lakename == "Flathead lake Woods Bay" |
                                lakename == "Flathead lake Yellow Bay" |
                               lakename == "Lindbergh" | lakename == "Little Bitteroot" | lakename == "Mary Ronan East" |
                               lakename == "Mary Ronan West" | lakename == "Swan Lake South" | lakename ==  "Swan Lake North" | lakename == "Tally" | 
                               lakename == "Upper Stillwater" | lakename == "Whitefish")


unique(large$lakename)


#now time to make some graphs ####
#working directory to save graphs
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2023/TN")

#make just original bar graphs
#tiff line is to save graphs then run dev line after
tiff("SmallTN2023.tiff", units="in", width=7, height=5, res=300)

ggplot(data = small %>% filter(Characteristic_ID == "TN", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "navy")+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("2023 Small Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()


#again for med
tiff("MedTN2023.tiff", units="in", width=7, height=5, res=300)

ggplot(data = med %>% filter(Characteristic_ID == "TN", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "navy")+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("2023 Medium Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()

#again for large
tiff("LargeTN2023.tiff", units="in", width=7, height=5, res=300)

ggplot(data = large %>% filter(Characteristic_ID == "TN", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "navy")+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("2023 Large Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()


#now for TP ####
#make just original bar graphs
#correct saving wd
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2023/TP")


tiff("SmallTP2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = small %>% filter(Characteristic_ID == "TP", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "forestgreen")+
  ylab("Total Persulfate Phosphorus (mg/L)")+
  xlab("")+
  ggtitle("2023 Small Lakes (Surface Area <100 Acres) \nTotal Persulfate Phosphorus")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()


#again for med
tiff("MedTP2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = med %>% filter(Characteristic_ID == "TP", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "forestgreen")+
  ylab("Total Persulfate Phosphorus (mg/L)")+
  xlab("")+
  ggtitle("2023 Medium Lakes (Surface Area <100 Acres) \nTotal Persulfate Phosphorus")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()

#again for large
tiff("LargeTP2023.tiff", units="in", width=7, height=5, res=300)
ggplot(data = large %>% filter(Characteristic_ID == "TP", year == 2023))+
  geom_col(aes(y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)), 
           fill = "forestgreen")+
  ylab("Total Persulfate Phosphorus (mg/L)")+
  xlab("")+
  ggtitle("2023 Large Lakes (Surface Area <100 Acres) \nTotal Persulfate Phosphorus")+
  scale_y_continuous(expand = c(0,0))+
  mlc_theme

dev.off()

#other options
#bar graph with multiple years
small$year<-as.character(small$year)
ggplot(data = small %>% filter(Characteristic_ID == "TN", year > 2019), 
       aes(fill = year, y = Result_Value*.001, x = fct_reorder(lakename, Result_Value, .na_rm = TRUE)))+
  geom_bar(position = "dodge", stat = "identity")+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("Small Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen")+
  mlc_theme

#point graph with error bars??
detach(package:plyr)
#make a mean for each lake
small.mean<- small %>% group_by(lakename, Characteristic_ID) %>% mutate(mean = mean(Result_Value, na.rm = TRUE),
                                                                        min = min(Result_Value, na.rm = TRUE),
                                                                        max = max(Result_Value, na.rm = TRUE),
                                                                        sd = sd(Result_Value, na.rm = TRUE))

ggplot(data = small.mean %>% filter(Characteristic_ID == "TN"), 
       aes( x = fct_reorder(lakename, Result_Value, .na_rm = TRUE), y = mean*.001))+
  geom_point( size = 2.5)+
  geom_errorbar(aes(ymin = mean*.001-sd*.001, ymax = mean*.001+sd*.001))+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("Small Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen (error bars show standard deviation)")+
  mlc_theme

ggplot(data = small.mean %>% filter(Characteristic_ID == "TN"), 
       aes( x = fct_reorder(lakename, Result_Value, .na_rm = TRUE), y = mean*.001))+
  geom_point( size = 2.5)+
  geom_errorbar(aes(ymin = mean*.001-min*.001, ymax = mean*.001+max*.001))+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("Small Lakes (Surface Area <100 Acres) \nTotal Persulfate Nitrogen (error bars show min and max vlaues)")+
  mlc_theme


#show trend through time
ggplot(data = med %>% filter(Characteristic_ID == "TN"),  aes(x = year, y = Result_Value*.001))+
  geom_point()+
  geom_smooth(aes(x = year, y = Result_Value*.001), method = "lm", se = FALSE)+
  facet_wrap(~lakename)+
  ylab("Total Persulfate Nitrogen (mg/L)")+
  xlab("")+
  ggtitle("Medium Lakes \nTotal Persulfate Nitrogen")+
  mlc_theme
  
  
#choose one lake to make more graphs about??
  ggplot(data = large %>% filter(Characteristic_ID == "TP", lakename == "Dickey"),  aes(x = year, y = Result_Value*.001))+
    geom_point(size = 2)+
    geom_smooth(aes(x = year, y = Result_Value*.001), method = "lm", se = FALSE)+
    ylab("Total Phosphorus (mg/L)")+
    xlab("Year")+
    ggtitle("Dickey Lake Phosphorus Trend")+
  mlc_theme 
  
  ggplot(data = large %>% filter(Characteristic_ID == "TEMP-W", lakename == "Dickey", 
                                 Result_Depth_Height_Measure > 8),  aes(x = year, y = Result_Value))+
    geom_point(size = 2)+
    ylab("Temperature")+
    xlab("Year")+
    ggtitle("Dickey Lake Temperature Trend")+
    mlc_theme 
  
  
  
  
  
  
  
glimpse(large)

dickey<- large %>% filter(Characteristic_ID == "TN", lakename == "Beaver")

summary(lm(Result_Value~year, large %>% filter(Characteristic_ID == "TN", lakename == "Beaver")))


  





















