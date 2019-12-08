#load required libraries (make sure you have installed them)
library(tidyverse)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(dplyr)
library(extrafont)

#read the csv file
setwd("D:/R data/Rdata")
set.seed(12345)
df<- read.csv(file = 'Metro Density.csv', header = TRUE, sep = ',', dec = '.')

#assign column names
colnames(df) <- c("Metro", "Population", "Mile 0", paste0('Miles ',1:90))
#Remove word "Metro Area" from names and add as new column. Here we use tm package.
#at the same time we can convert from Factor to character.
df$Metro = removeWords(as.character(df$Metro), c(" Metro Area"))
#selecting top 25 Metro areas. 
df <- df %>% arrange(desc(Population)) %>% slice(1:25)
#remove columns which are not required.
dd <- df[,c(1,3:28)]
#converting Wide Format data into long format data. GGplot doesnot support wide format data.
ulong <- tidyr::gather(dd,Distance, Density,2:27)

#create order for miles. 
order <- c("Mile 0", paste0("Miles ", 1:25))

#we need to create levels in order for equal and logical distributon of data within the plot.
#arranging distnace order for scale
ulong$Distance <- factor(as.character(ulong$Distance),levels=order)

#create a new variable from incidence
ulong$DenseFactor <- cut(ulong$Density,
                          breaks = c(0,2000,3400,4500,5500,6500,8500,14000,max(ulong$Density,na.rm=T)),
                          labels=c("0-2,000","2,000-3,400","3,400-4,500","4,500-5,500",
                                   "5,500-6,500","6,500-8,500","8,500-14,000", ">14,000"))

#ulong$DenseFactor <- factor(as.character(ulong$DenseFactor),levels=rev(levels(ulong$DenseFactor)))

#Preparing for new plot
dfv <- ggplot(data = ulong, aes(x = sort(ulong$Distance),
                                y = ulong$Metro,
                                fill=ulong$DenseFactor))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="#252525",size=0.15, show.legend = TRUE)+
  #set the ratio of tiles in square
  coord_equal()+
  #remove x and y axis labels
  labs(x="Distance (in Miles)",y="")+
  #define new breaks on y-axis
  scale_y_discrete(expand=c(0,0))+
  #define new breaks on x-axis
  scale_x_discrete(expand=c(0,0),
                   breaks=c("Miles 0","Miles 5","Miles 10","Miles 15","Miles 20","Miles 25","Miles 30"))+
  #custom colours for cut levels and na values
  scale_fill_manual(values = rev(brewer.pal(8,"Spectral")))+
#geom_vline(aes(xintercept = 11, size = 0.01, alpha = 5), show.legend = FALSE)
  theme_grey(base_size=12)+
  # Set legend title
  labs(fill = 'Density (Persons per sq. mi)',
       subtitle = '(Moving Outward From City Hall)',
       caption = 'Source: U.S. Census Bureau')+
  # Set plot tilte 
  ggtitle("Population Density in Selected Metropolitan Areas in the US in 2013")+
  #theme options. set text size of plot title
  theme(plot.title = element_text(size = 15),
        #set legend title text
        legend.text=element_text(colour="grey40",size=9,face="bold"),
        axis.text.x=element_text(size=10,colour="grey40"),
        axis.title.x = element_text(size=12, face="bold" ),
        axis.text.y = element_text(size = 10, colour = "grey40"),
        axis.ticks=element_line(size=1),
        legend.title = element_text(face = 'bold'),
        plot.caption = element_text(size=12, colour = "grey40",face="bold",hjust = 0.5))
  
plot(dfv)