library('dplyr')
library(ggplot2)
#install.package('ggstatsplot')
#install.package('palmerpenguins')
library(ggstatsplot)
library(palmerpenguins)
library(grid)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)

train <- read.csv('/home/laucheuk/Desktop/DATA11002intro_to_ML/npf_train.csv')
train<- train %>% 
  mutate(class4= case_when(class4=="nonevent" ~ "nonevent"
                           TRUE ~ "event"))

#name:the title of final plot e.g. h2o,co2
#start,end: index of original training dataset
violin_plot<- function(name,start,end,unit){
  subset<- train[c(3,start:end)]
  plot_mean <- ggplot(data=melt(subset[c(1,seq(2,length(subset),by=2))]),
                      aes(y=value,x=variable,color=class4))+geom_violin()+labs(y=unit)
  plot_sd <- ggplot(data=melt(subset[c(seq(1,length(subset),by=2))]),
                    aes(y=value,x=variable,color=class4))+geom_violin()
  combine_plot<- ggarrange(plot_mean+rremove('xlab')+rremove('legend'),plot_sd+rremove('xylab'),labels=c('Mean','Standard deviation'))
  return(annotate_figure(combine_plot,top = text_grob(name,face = "bold", size = 14)))
}

density_plot<- function(name,start,end,units){
  subset<- train[c(3,start:end)]
  plot_mean <- ggdensity(melt(subset[c(1,seq(2,length(subset),by=2))]),x='value',
                         fill = 'variable',alpha=0.4)
  plot_sd <- ggdensity(melt(subset[c(seq(1,length(subset),by=2))]),x='value',
                       fill = 'variable',alpha=0.4)
  combine_plot<- ggarrange(plot_mean+rremove('xlab')+rremove('legend.title'),plot_sd+rremove('xylab')+rremove('legend.title'),labels=c('Mean','Standard deviation'))
  if(length(subset)==3){
    combine_plot<- ggarrange(plot_mean+rremove('xlab')+rremove('legend'),plot_sd+rremove('xylab')+rremove('legend'),labels=c('Mean','Standard deviation'))
  }
  return(annotate_figure(combine_plot,top = text_grob(name,face = "bold", size = 14),
                         bottom = text_grob(units)))
}

##Co2:carbon dioxide concentration in ppm-units
density_plot('Carbon dioxide concentration',5,12,'ppm-units')
violin_plot('Carbon dioxide concentration',5,12,'ppm-units')

#Glob: solar radiation in wavelength range 0.30 - 4.8 um in units W/m2
density_plot('Solar radiation',13,14,'W/m2')
violin_plot('Solar radiation',13,14,'W/m2')

#H2O : water vapour concentration in ppth-units
density_plot('Water vapour concentration',15,26,'ppm-units')
violin_plot('Water vapour concentration',15,26,'ppm-units')

#NET:net radiation in wavelength range 0.30 - 40 um in units W/m2
density_plot('Net radiation',27,28,'W/m2')
violin_plot('Net radiation',27,28,'W/m2')

#NO:nitrogen monoxide concentration in ppb-units
density_plot('Nitrogen monoxide concentration',29,40,'ppb-units')
violin_plot('Nitrogen monoxide concentration',29,40,'ppb-units')

#NOx :nitrogen monoxide + nitrogen dioxide concentration in ppb-units
density_plot('Nitrogen monoxide + nitrogen dioxide concentration',41,52,'ppb-units')
violin_plot('Nitrogen monoxide + nitrogen dioxide concentration',41,52,'ppb-units')

#Ozone
density_plot('Ozone concentration',53,62,'ppb-units')
violin_plot('Ozone concentration',53,62,'ppb-units')

#Pamb :air pressure in hPa
density_plot('Air pressure',63,64,'hPa')
violin_plot('Air pressure',63,64,'hPa')

#PAR:photosynthetically active radiation
density_plot('Photosynthetically active radiation',65,66,'umol/(m2s)')
violin_plot('Photosynthetically active radiation',65,66,'umol/(m2s)')

#PTG: potential temperature gradient in C/m
density_plot('Potential temperature gradient',67,68,'C/m')
violin_plot('Potential temperature gradient',67,68,'C/m')

#RGLOB:reflected solar radiation
density_plot('Reflected solar radiation',69,70,'W/m2')
violin_plot('Reflected solar radiation',69,70,'W/m2')

#RHIRGA
density_plot('RHIRGA',71,82,' ')
violin_plot('RHIRGA',71,82,' ')

#RPAR: reflected photosynthetically active radiation 
density_plot('Reflected photosynthetically active radiation',83,84,'umol/(m2s)')
violin_plot('Reflected photosynthetically active radiation',83,84,'umol/(m2s)')

#SO2: sulphur dioxide concentration  
density_plot('Sulphur dioxide concentration',85,86,'ppb-units')
violin_plot('Sulphur dioxide concentration',85,86,'ppb-units')

#SWS:  rain indicator signal in units mV
density_plot('Rain indicator signal',87,88,'mV')
violin_plot('Rain indicator signal',87,88,'mV')

#Temperature: air temperature in C
density_plot('Air temperature',89,98,'C')
violin_plot('Air temperature',89,98,'C')

#UV_A:ultraviolet radiation in wavelength range 0.32 - 0.40 um in units W/m2
density_plot('Ultraviolet radiation in wavelength range 0.32 - 0.40 um',99,100,'W/m2')
violin_plot('Ultraviolet radiation in wavelength range 0.32 - 0.40 um',99,100,'W/m2')

#UV-B ultraviolet radiation in wavelength range 0.28 - 0.32 um in units W/m2
density_plot('Ultraviolet radiation in wavelength range 0.28 - 0.32 um',101,102,'W/m2')
violin_plot('Ultraviolet radiation in wavelength range 0.28 - 0.32 um',101,102,'W/m2')

#CS: condensation sink in units of 1/s
density_plot('Condensation sink',103,104,'1/s')
violin_plot('Condensation sink',103,104,'1/s')
