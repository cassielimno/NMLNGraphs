#making summary stats for NMLN 2024 report
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(grid) 
library(tayloRswift)
library(cowplot)
library(patchwork)
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
#THIS IS THE COMPLETED DATA for 2024
data<- read.csv("HydroShareFinalALL2024.csv")
nmln<- data %>% filter(Project_ID == "NMLN")



#modify date and make month col
glimpse(nmln)
#nmln$Activity_Start_Date<-as.POSIXct(nmln$Activity_Start_Date, format = "%M/%D/%Y")
#nmln$Activity_Start_Date<-mdy(nmln$Activity_Start_Date)
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

#make blue gradient background ####



mycols <- colors()[c( 432, 590, 616, 619)] #


g <- rasterGrob(mycols, width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 


#graph to check colors on if needed 
#link to helpful chart for color #s
#https://rjbioinformatics.wordpress.com/wp-content/uploads/2016/07/screen-shot-2016-07-09-at-5-18-32-pm.png?w=675#038;h=378
# ggplot(mtcars, aes(factor(cyl))) + # add gradient background 
#   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
#   geom_bar() # add data layer 

#I added breaks(2011:2024) to the scale fill manual and then took it out and the 
#years went into the right order

#to make sure graphs go to the right place
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/NMLN analysis/NMLNSummaryGraphs2024")

#now do DO loop ####
for (i in unique(nmln$lakename)){
  
  onelake<- nmln %>% filter(lakename == i)
  
  p0<- 
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakenameD, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold")) 
  
  
  p.5<-
    
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none")
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  
}


#now do DO loop
#this loop is for saving the graphs####
#to make sure graphs go to the right place

setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/DO")
plot_list = list()
for (i in unique(nmln$lakename)){
  
  onelake<- nmln %>% filter(lakename == i)
  
  p0<- 
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakenameD, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none")
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_DO.png", sep="")
  png(file_name, height = 1000, width = 1050, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#cut dates in DO for blanchard ####
#lets cut to just to july and and august ####
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/DO")
nmlnblanch<- nmln %>% filter(Station_ID == "BLANCH")
plot_list = list()
for (i in unique(nmlnblanch$lakename)){
  
  onelake<- nmlnblanch %>% filter(lakename == i)
  
  p0<- 
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month == 8 | month == 7, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakenameD, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month == 8 | month == 7, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none")
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  #ggsave(p, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
  
  file_name = paste(i, "_DO.png", sep="")
  png(file_name, height = 1050, width = 1010, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}

#dev.off()

#fix width of swan lake graphs for DO ####
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/DO")
nmlnswan<- nmln %>% filter(Station_ID == "SWAN-N" |Station_ID == "SWAN-S")
plot_list = list()
for (i in unique(nmlnswan$lakename)){
  
  onelake<- nmlnswan %>% filter(lakename == i)
  
  p0<- 
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakenameD, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "DO", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
    geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Dissolved oxygen")+
    ylab("Depth (m)")+
    xlab("Dissolved Oxygen mg/l")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none")
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "DO", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_DO.png", sep="")
  png(file_name, height = 1600, width = 2000, res = 200)
  print(plot_list[[i]])
  dev.off()
}

#dev.off()



#now for temperature and printing the graphs #####
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TEMP")
plot_list = list()
for (i in unique(nmln$lakename)){
  
  onelake<- nmln %>% filter(lakename == i)
  
  p0<-  ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity",  x = 88, y = -6, label = "CRITICAL \nTHERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT \nLETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))  
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity", x = 88, y = -6, label = "CRITICAL THERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT LETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none") 
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_temp.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#make graphs for smith lake and lost loon manually ####
#they are too shallow to work with the other ones
nmln.shallow<- nmln %>% filter(Station_ID == "SKYLES" | Station_ID == "SPENCER"| 
                                 Station_ID == "ROGERS" | Station_ID =="SMITH" |
                                 Station_ID ==  "LOSTCOO" | Station_ID == "ABBOT")

unique(nmln.shallow$lakename)

#LOOP FOR SHALLOW LAKES
#this loop will replace the same images with better versions so run it AFTER 
plot_list = list()
for (i in unique(nmln.shallow$lakename)){
  
  onelake<- nmln.shallow %>% filter(lakename == i)
  
  p0<- 
    ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity",  x = 88, y = -1, label = "CRITICAL \nTHERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -1, label = "INCIPIENT \nLETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakenameD, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))  
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity", x = 88, y = -1, label = "CRITICAL THERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -1, label = "INCIPIENT LETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none") 
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_temp.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#cutting dates for Temp for Blanchard ####
#use nmln blanch created when I do the same thing for DO above
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TEMP")
plot_list = list()
for (i in unique(nmlnblanch$lakename)){
  
  onelake<- nmlnblanch %>% filter(lakename == i)
  
  p0<-  ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month == 7 | month == 8, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity",  x = 88, y = -6, label = "CRITICAL \nTHERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT \nLETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))  
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month == 7 | month == 8, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity", x = 88, y = -6, label = "CRITICAL THERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT LETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none") 
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_temp.png", sep="")
  png(file_name, height = 1050, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#making wider graphs for swan for TEMP ####
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TEMP")
plot_list = list()

for (i in unique(nmlnswan$lakename)){
  
  onelake<- nmlnswan %>% filter(lakename == i)
  
  p0<-  ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity",  x = 88, y = -6, label = "CRITICAL \nTHERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT \nLETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(.01,.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))  
  
  
  p.5<-
    ggplot(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month < 9, year < 2024))+
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    annotate("rect", xmin =86, xmax = 90, ymin = -Inf, ymax =0.01, alpha = .4, color = "red", fill = "red")+
    geom_text(position = "identity", x = 88, y = -6, label = "CRITICAL THERMAL \nMAXIMUM", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    annotate("rect", xmin =77, xmax = 86, ymin = -Inf, ymax =0.01, alpha = .4, color = "sienna1", fill = "sienna1")+
    geom_text(position = "identity", x = 82, y = -6, label = "INCIPIENT LETHAL \nTEMPERATURE", aes(angle = 90,), color = "white", size = 3, fontface = "bold")+
    geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
    theme_bw()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", month <10 & month > 1, year == 2024), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
    scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                                 "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
    scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21, 21, 21), name = "Date")+
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
    ggtitle(onelake$lakename, " \nSummer Temperature")+
    ylab("Depth (m)")+
    xlab("Temperature F")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_continuous(expand = c(.01,.01))+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14, face = "bold"))+
    theme(legend.position = "none") 
  
  
  p1<-ggplot()+
    geom_point(data = onelake %>% filter(Characteristic_ID == "TEMP-W", 
                                         month <10 & month > 1, year == 2024), 
               aes(x= Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date), 
               size = 3, shape = 23)+
    scale_fill_manual(name = "This Year", values = c("lawngreen"))+
    theme_bw()+
    mlc_theme
  
  leg1<- get_legend(p0)
  leg2<- get_legend(p1)
  
  legboth<-plot_grid(leg1, leg2, ncol = 1)
  
  print(plot_grid(p.5,
                  legboth,
                  nrow = 1,
                  align = "h",
                  axis = "t",
                  rel_widths = c(1, 0.3)))
  
  p = plot_grid(p.5,
                legboth,
                nrow = 1,
                align = "h",
                axis = "t",
                rel_widths = c(1, 0.3))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, "_temp.png", sep="")
  png(file_name, height = 1600, width = 2000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


# Another option: create pdf where each page is a separate plot. could be useful? if it works####
pdf("plots.pdf")
for (i in 1:3) {
  print(plot_list[[i]])
}


#create loop for TSI graph ####
#carsons trophic state index
#make index for all nmln 
#note on this ####
#it is my understanding that in R log is equal to ln (natural log) if this is not true
#this should be changed instead to natural log
#sometimes group_by doesn't work this line helps
detach(package:plyr)
glimpse(nmln)
#have to use data.now2 because lab data does not seem to have right project ID
#AT SOME POINT ADD LAKENAME CODE TO THIS ####
nmln.tsi<- nmln %>%  mutate(TSI = case_when(Characteristic_ID == "TP" ~ 14.42*log(Result_Value)+4.15,
                                            Characteristic_ID == "CHL-A-CP" ~ 9.81*log(Result_Value)+30.6,
                                            Characteristic_ID == "TN" ~ 14.34*log(Result_Value/1000)+ 54.45,
                                            .default = ),
                            year = year(Activity_Start_Date),
                            month = month(Activity_Start_Date),
                            day = yday(Activity_Start_Date),
                            monthname = month.abb[month])


nmln.tsi<- nmln.tsi %>% group_by(Station_ID, year) %>% mutate(TSI_mean = mean(TSI, na.rm = TRUE))

nmln.tsi$monthname<-factor(nmln.tsi$monthname, levels = month.abb)


#change this for graphing
nmln.tsi$year<-as.character(nmln.tsi$year)
nmln.tsi$Activity_Start_Date<-as.POSIXct.Date(nmln.tsi$Activity_Start_Date)

glimpse(nmln.tsi)
glimpse(nmln)
#loop for TSI
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TSI")
plot_list = list()
for (i in unique(nmln.tsi$Station_ID)){
  
  onelake<- nmln.tsi %>% filter(Station_ID == i)
  
  my_colors <- scales::hue_pal()(4)
  print(ggplot(data = onelake, aes(x = year, y = TSI))+
          annotate("rect", xmin = "2008" , xmax = "2024", ymin = 0, ymax = 40, alpha = .2, color = "lightskyblue", fill = "lightskyblue")+
          annotate("rect", xmin = "2008", xmax = "2024", ymin = 40, ymax = 50, alpha = .2, color = "aquamarine4", fill = "aquamarine4")+
          annotate("rect", xmin = "2008", xmax = "2024" , ymin = 50, ymax = 70, alpha = .2, color = "darkolivegreen", fill = "darkolivegreen2")+
          geom_text(x = "2017", y = 45, label = "MESOTROPHIC")+
          geom_text(x = "2017", y = 60, label = "EUTROPHIC")+
          geom_point(aes(x = year, y = TSI, fill = Characteristic_ID), size = 2.5, shape = 21)+
          #geom_point(aes(x = year, y = TSI_mean), size = 4, shape = 21, fill = "#c77cFF")+
          geom_smooth(method = "lm")+
          theme_bw()+
          mlc_theme+
          geom_text(x = "2017", y = 20, label = "OLIGOTROPHIC")+
          scale_fill_manual(
            values = my_colors,
            limits = c('TN', 'TP', 'CHL-A-CP'),
            labels = c('Total Nitrogen', 'Total Phosphorus', 'Chlorophyll'),
            name = ""
          )+
          ylab("TSI")+
          xlab("Year")+
          ggtitle(onelake$lakename, " \nTrophic State Index")+
          scale_y_continuous(expand = c(0.01,0.01))+
          scale_x_discrete(expand = c(0.01,0.01)))
  
  p =  ggplot(data = onelake, aes(x = year, y = TSI))+
    annotate("rect", xmin = "2008" , xmax = "2024", ymin = 0, ymax = 40, alpha = .2, color = "lightskyblue", fill = "lightskyblue")+
    annotate("rect", xmin = "2008", xmax = "2024" , ymin = 40, ymax = 50, alpha = .2, color = "aquamarine4", fill = "aquamarine4")+
    annotate("rect", xmin = "2008", xmax = "2024" , ymin = 50, ymax = 70, alpha = .2, color = "darkolivegreen", fill = "darkolivegreen2")+
    geom_text(x = "2017", y = 45, label = "MESOTROPHIC")+
    geom_text(x = "2017", y = 60, label = "EUTROPHIC")+
    geom_point(aes(x = year, y = TSI, fill = Characteristic_ID), size = 2.5, shape = 21)+
    #geom_point(aes(x = year, y = TSI_mean), size = 4, shape = 21, fill = "#c77cFF")+
    geom_smooth(method = "lm")+
    theme_bw()+
    mlc_theme+
    geom_text(x = "2017", y = 20, label = "OLIGOTROPHIC")+
    scale_fill_manual(
      values = my_colors,
      limits = c('TN', 'TP', 'CHL-A-CP'),
      labels = c('Total Nitrogen', 'Total Phosphorus', 'Chlorophyll'),
      name = ""
    )+
    ylab("TSI")+
    xlab("Year")+
    ggtitle(onelake$lakename, " \nTrophic State Index")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_discrete(expand = c(0.01,0.01))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, ".png", sep="")
  png(file_name, height = 1000, width = 1200, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#make FH-mack alone because it does not contian 2017
nmln.mac<- nmln.tsi %>% filter(Station_ID == "FH-MACK")
#keeping it in a loop so it'll replace the old mack graph in the folder

plot_list = list()
for (i in unique(nmln.mac$Station_ID)){
  
  onelake<- nmln.mac %>% filter(Station_ID == i)
  
  my_colors <- scales::hue_pal()(4)
  
  
  p = ggplot(data = onelake, aes(x = year, y = TSI))+
    annotate("rect", xmin = "2008" , xmax = "2024", ymin = 0, ymax = 40, alpha = .2, color = "lightskyblue", fill = "lightskyblue")+
    annotate("rect", xmin = "2008", xmax = "2024" , ymin = 40, ymax = 50, alpha = .2, color = "aquamarine4", fill = "aquamarine4")+
    annotate("rect", xmin = "2008", xmax = "2024" , ymin = 50, ymax = 70, alpha = .2, color = "darkolivegreen", fill = "darkolivegreen2")+
    geom_text(x = "2018", y = 45, label = "MESOTROPHIC")+
    geom_text(x = "2018", y = 60, label = "EUTROPHIC")+
    geom_point(aes(x = year, y = TSI, fill = Characteristic_ID), size = 2.5, shape = 21)+
    #geom_point(aes(x = year, y = TSI_mean), size = 4, shape = 21, fill = "#c77cFF")+
    geom_smooth(method = "lm")+
    theme_bw()+
    mlc_theme+
    geom_text(x = "2018", y = 22, label = "OLIGOTROPHIC")+
    scale_fill_manual(
      values = my_colors,
      limits = c('TN', 'TP', 'CHL-A-CP'),
      labels = c('Total Nitrogen', 'Total Phosphorus', 'Chlorophyll'),
      name = ""
    )+
    ylab("TSI")+
    xlab("Year")+
    ggtitle(onelake$lakename, " \nTrophic State Index")+
    scale_y_continuous(expand = c(0.01,0.01))+
    scale_x_discrete(expand = c(0.01,0.01))
  
  plot_list[[i]]  = p
  
  file_name = paste(i, ".png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
}





#trying to fix legend #####

ggplot(data = nmln %>% filter(Characteristic_ID == "DO", month < 9, year < 2024, Station_ID == "ABBOT"))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  annotate("rect", xmin =0, xmax = 1, ymin = -Inf, ymax =0.01, alpha = .2, color = "red", fill = "red")+
  geom_text(x = .5, y = -4.2, label = "ANOXIC", aes(angle = 90,), color = "white")+
  geom_point(aes(x = Result_Value, y = -1*Result_Depth_Height_Measure, fill = Analysis_Start_Date, size = Analysis_Start_Date, shape = Analysis_Start_Date))+
  theme_bw()+
  geom_point(data = nmln %>% filter(Characteristic_ID == "DO", month <10 & month > 1, year == 2024, Station_ID == "ABBOT"), aes(x= Result_Value, y = -1*Result_Depth_Height_Measure), fill = "lawngreen", size = 3, shape = 23)+
  scale_fill_manual(values = c("gray1", "saddlebrown", "plum", "palevioletred1", "darkviolet",  "slateblue", "lightsteelblue", "mediumturquoise", "cyan4",  "deepskyblue", "mediumblue","palegreen4", "darkgreen",
                               "darkseagreen1", "springgreen", "yellow",  "goldenrod", "sienna1", "indianred", "thistle", "firebrick", "red4",  "deeppink", "orchid1", "purple4", "peachpuff4",  "snow4", "palegoldenrod"), name = "Date")+
  scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,21,21,21,21,21,21,21,21,21,21,21,21,21), name = "Date")+
  scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), name = "Date")+
  ggtitle( " \nSummer Dissolved oxygen")+
  ylab("Depth (m)")+
  xlab("Dissolved Oxygen mg/l")+
  scale_y_continuous(expand = c(.01,.01))+
  scale_x_continuous(expand = c(.01,.01))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 14, face = "bold")) 






#lets make T and N trend graphs ####

ggplot(data = nmln %>% filter(Characteristic_ID == "TN", Station_ID == "ABBOT"),
       aes(y = Result_Value, x = year))+
  geom_point(aes(y = Result_Value, x = year))+
  ylab("Total Nitrogen (ug/l)")+
  geom_smooth(method = 'lm', se = FALSE)+
  mlc_theme

summary(lm(Result_Value ~ year, data = nmln %>% filter(Characteristic_ID == "TN", Station_ID == "ABBOT")))

for(i in unique(nmln$Station_ID)) {
  nmln$pval[nmln$Station_ID == i] <-lm(Result_Value ~ year, data = nmln %>% filter(Characteristic_ID == "TN", Station_ID == i))$p.value
}


nmln$pval <-lm(Result_Value ~ year, data = nmln %>% filter(Characteristic_ID == "TN", Station_ID == "ABBOT"))$p.value

for (i in unique(df$day)) {
  df$p.val[df$day == i] <- wilcox.test(df[df$day %in% 1:i,]$group_a, df[df$day %in% 1:i,]$group_b, alternative = 'g')$p.value
}


#another way

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
nmln2<- filter(nmln2, !Station_ID == "BULL", !Station_ID == "FH-CONRAD", !Station_ID == "FH-CRESC", 
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
    
    #make a stats data frame for i
    stats<-data.frame(p, slope, Station_ID, Characteristic_ID)
    
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

#make TN graphs ####
#in a loop for sig graphs
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TN")

plot_list = list()

for (i in unique(tn.sig.nmln$Station_ID)){
  
  onelake<- tn.sig.nmln %>% filter(Station_ID == i)
  
  plot = print(ggplot(data = onelake)+
                 geom_point(aes(y = result_nd, x = year))+
                 ylab("Total Nitrogen (ug/l)")+
                 xlab("Year")+
                 geom_smooth(method = 'lm', se = FALSE,  aes(y = result_nd, x = year))+
                 mlc_theme+
                 ggtitle(onelake$lakename, " \nTotal Nitrogen trend"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  #this is the good code for saving #####
  file_name = paste(i, "_TN.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#do that again for tp
#now filter for p values that are significant AND just phosphorus data
tp.sig.nmln<-nmln3 %>% filter(p< 0.05, Characteristic_ID == "TP")
#see how many
unique(tp.sig.nmln$Station_ID)

#make TP graphs ####
#in a loop for sig graphs
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TP")

plot_list = list()
for (i in unique(tp.sig.nmln$Station_ID)){
  
  onelake<- tp.sig.nmln %>% filter(Station_ID == i)
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year))+
                  ylab("Total Phosphorus (ug/l)")+
                  xlab("Year")+
                  geom_smooth(method = 'lm', se = FALSE,  aes(y = result_nd, x = year))+
                  mlc_theme+
                  ggtitle(onelake$lakename, " \nTotal Phosphorus trend"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  file_name = paste(i, "_TP.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#now chl sig graphs
chl.sig.nmln<-nmln3 %>% filter(p< 0.05, Characteristic_ID == "CHL-A-CP")
#see how many
unique(chl.sig.nmln$Station_ID)

setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/CHL")

#in a loop for sig graphs

plot_list = list()
for (i in unique(chl.sig.nmln$Station_ID)){
  
  onelake<- chl.sig.nmln %>% filter(Station_ID == i)
  
  plot = print(ggplot(data = onelake)+
                 geom_point(aes(y = result_nd, x = year))+
                 ylab("Chlorophyll (ug/l)")+
                 xlab("Year")+
                 geom_smooth(method = 'lm', se = FALSE,  aes(y = result_nd, x = year))+
                 mlc_theme+
                 ggtitle(onelake$lakename, " \n Chlorophyll trend"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  file_name = paste(i, "_CHL.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#make all non-sig plots  ####
nmln3.nonsig<- nmln3 %>% filter(p > 0.05)

#TN loop
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TN")

#make tn dataframe
nmln3.tn.nonsig<- nmln3.nonsig %>% filter(Characteristic_ID == "TN")

plot_list = list()
for (i in unique(nmln3.tn.nonsig$Station_ID)){
  
  onelake<- nmln3.tn.nonsig %>% filter(Station_ID == i)
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year))+
                  ylab( "Total Nitrogen (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Nitrogen"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  file_name = paste(i, "_TN.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#TP non sig loop
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/TP")

#make tn dataframe
nmln3.tp.nonsig<- nmln3.nonsig %>% filter(Characteristic_ID == "TP")

plot_list = list()
for (i in unique(nmln3.tp.nonsig$Station_ID)){
  
  onelake<- nmln3.tp.nonsig %>% filter(Station_ID == i)
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year))+
                  ylab( "Total Phosphorus (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Phosphorus"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  file_name = paste(i, "_TP.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}


#CHL non sig loop
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/GRAPHS/NMLNSummaryGraphs2024/CHL")

#make tn dataframe
nmln3.chl.nonsig<- nmln3.nonsig %>% filter(Characteristic_ID == "CHL-A-CP")

plot_list = list()
for (i in unique(nmln3.chl.nonsig$Station_ID)){
  
  onelake<- nmln3.chl.nonsig %>% filter(Station_ID == i)
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year))+
                  ylab( "Total Chlorophyll (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Chlorophyll"))
  
  
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  file_name = paste(i, "_CHL.png", sep="")
  png(file_name, height = 1000, width = 1000, res = 200)
  print(plot_list[[i]])
  dev.off()
  
  
}

#next steps #####
#try changing this to mann kendall will less be significant??
library(rkt)
#make loop that works for all characteristics
#make an empty dataframe
nmln.stats.mk<-data.frame()
#for loop to extract trend and p value for each lake using results that include non-detects
for (i in unique(nmln2$Station_ID)) {
  
  #make object called station ID that is Station ID to help with merging
  Station_ID<- i
  
  #add another loop for characteristic ID
  for (j in unique(nmln2$Characteristic_ID)) { 
    
    #make object for characteristic ID
    Characteristic_ID<- j
    
    #new data frame
    mkdata<-nmln2 %>% filter(Characteristic_ID == j, Station_ID == i)
    
    #run lm
    my_lm<-lm(result_nd ~ year, data = nmln2 %>% filter(Characteristic_ID == j, Station_ID == i))
    
    mk<-rkt(mkdata$year, mkdata$Result_Value, rep = "a")
    
    #extract coeffcients
    cf<-print(mk)
    
    #extract slope from coeffcients
    slope<- as.numeric(mk[3])
    
    #extract p using function made above
    p <- as.numeric(mk[1])
    
    #save tau as well
    tau <- as.numeric(mk[12])
    
    #make a stats data frame for i
    stats<-data.frame(p, slope, tau, Station_ID, Characteristic_ID)
    
    #if it gets stuck to see which lake is tripping it up
    print(i)
    
    #combine each stats dataframe
    nmln.stats.mk<- rbind(stats, nmln.stats.mk)
    
    
  } }

#make final dataframe with stats attached
nmln3.mk<-merge(x = nmln.stats.mk, y = nmln2, by = c("Station_ID", "Characteristic_ID"))

#now filter for p values that are significant AND just nitrogen data
tn.sig.nmln.mk<-nmln3.mk %>% filter(p< 0.05, Characteristic_ID == "TN")
#see how many
unique(tn.sig.nmln$Station_ID)
#doing mk analysis adds dickey
unique(tn.sig.nmln.mk$Station_ID)


#now filter for p values that are significant AND just nitrogen data
tp.sig.nmln.mk<-nmln3.mk %>% filter(p< 0.05, Characteristic_ID == "TP")
#see how many
unique(tp.sig.nmln$Station_ID)
#doing mk analysis adds dickey
unique(tp.sig.nmln.mk$Station_ID)


#now filter for p values that are significant AND just nitrogen data
chl.sig.nmln.mk<-nmln3.mk %>% filter(p< 0.05, Characteristic_ID == "CHL-A-CP")
#see how many
unique(chl.sig.nmln$Station_ID)
#doing mk analysis adds dickey
unique(chl.sig.nmln.mk$Station_ID)


#graph this??
#use loess stuff from online
glimpse(chl.sig.nmln.mk)

plot_list = list()
for (i in unique(chl.sig.nmln.mk$Station_ID)){
  
  onelake<- chl.sig.nmln.mk %>% filter(Station_ID == i)
  
  
  trend_line<-predict(loess(result_nd ~ year, data = onelake))
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year), size = 2)+
                  geom_line(aes(x = year, y = trend_line), color = "blue", size = 1.25)+
                  ylab( "Total Chlorophyll (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Chlorophyll"))
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  # file_name = paste(i, ".tiff", sep="")
  # tiff(file_name, height = 2500, width = 2500, res = 300)
  # print(plot_list[[i]])
  # dev.off()
  
  
}


#do mk analysis for tn as well
plot_list = list()
for (i in unique(tn.sig.nmln.mk$Station_ID)){
  
  onelake<- tn.sig.nmln.mk %>% filter(Station_ID == i)
  
  
  trend_line<-predict(loess(result_nd ~ year, data = onelake))
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year), size = 2)+
                  geom_line(aes(x = year, y = trend_line), color = "blue", size = 1.25)+
                  ylab( "Total Nitrogen (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Nitrogen"))
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  # file_name = paste(i, ".tiff", sep="")
  # tiff(file_name, height = 2500, width = 2500, res = 300)
  # print(plot_list[[i]])
  # dev.off()
  
  
}


#for phosphorus
plot_list = list()
for (i in unique(tp.sig.nmln.mk$Station_ID)){
  
  onelake<- tp.sig.nmln.mk %>% filter(Station_ID == i)
  
  
  trend_line<-predict(loess(result_nd ~ year, data = onelake))
  
  
  plot =  print(ggplot(data = onelake)+
                  geom_point(aes(y = result_nd, x = year), size = 2)+
                  geom_line(aes(x = year, y = trend_line), color = "blue", size = 1.25)+
                  ylab( "Total Phosphorus (ug/l)")+
                  xlab("Year")+
                  mlc_theme+
                  ggtitle(onelake$lakename, "\n Total Phosphorus"))
  
  plot_list[[i]]  = plot
  
  #code for saving when needed
  # file_name = paste(i, ".tiff", sep="")
  # tiff(file_name, height = 2500, width = 2500, res = 300)
  # print(plot_list[[i]])
  # dev.off()
  
  
}


#export nmln3 with trend data and nmln.tsi with tsi data
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/NMLN analysis/SummaryScriptsandData")

write.csv(nmln.tsi, "NMLNTSIdata2024.csv")
write.csv(nmln3, "NMLNNutrientTrendData2024.csv")






