#Tide pool volume experiment
#scripted by Julio Rosales
# edited on 10-23-18


# clear the working directory #####
rm(list = ls())


# load the libraries #####
library(tidyverse)
library(nlme)
library(purrr)

#load the data
tdata<-read.csv("Data/Tide_pool_vol_stdcurve_data.csv")
View(tdata)

#analyze data ####
tdata %>% # this is the dataframe
  group_by(Machine,DyeVolume) %>% #grouping by machine and dye volume
  ggplot(aes(x= Absorbance, y= Volume))+   #setup plot with x and y data
    geom_line() + #adding lines
    facet_wrap(~Machine*DyeVolume) #dividing plots by machine and dye volume 

#write a power function for each curve work in progress
pool.coefs<-tdata %>%
  #nest(-Machine) %>%
  group_by(Machine,DyeVolume) %>% #grouping by machine and dye volume
  mutate(fit =map(data, ~nls(Volume~b*Absorbance^z, start = list(b=0, z=1), data = .x)),
    tidied = map(fit,tidy))
  



map(data,test = 
