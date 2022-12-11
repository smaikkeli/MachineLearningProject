library('dplyr')
library(ggplot2)
read_process <- function(filename,name){
  data <- read.csv(paste0('/home/laucheuk/Desktop/DATA11002intro_to_ML/MachineLearningProject-main/EDA/',filename,'.csv'))
  data['Approach']<-name
  colnames(data)[colnames(data) == "X"] = "Models"
 # data<- data[data['accuracy']==max(data['accuracy'])]
  return(data)
}
res_all<-read_process('res_all','All features')
res_correclated<-read_process('res_correlated','Highly correclated')
res_exCorreclated<-read_process('res_exCorrelated','Excluding highly correclated')
res_lasso<-read_process('res_lasso','Lasso')
res_lsvc<-read_process('res_lsvc','lsvc')
#res_RF<-read_process('res_RF','Random Forest')

t<-melt(dplyr::full_join(res_all,dplyr::full_join(res_correclated,dplyr::full_join(res_exCorreclated,dplyr::full_join(res_lasso,res_lsvc)))))

testmse<-ggplot(data = t[t$variable=='test_mse',],aes(x=Approach,y=value,group=Models,color=Models))+
  geom_line()+labs(title='Test squared error',fill='Models')+theme(legend.title = element_text(size=15),
                                                                   legend.text = element_text(size=15),
                                                                   legend.key.size = unit(1, 'cm'),
                                                                   axis.text.y = element_text(colour = 'black', size = 12), 
                                                                   axis.title.y = element_text(size = 20, 
                                                                                               hjust = 0.5, vjust = 0.2),
                                                                   axis.text.x = element_text(colour = 'black', size = 12), 
                                                                   axis.title.x = element_text(size = 20, 
                                                                                               hjust = 0.5, vjust = 0.2))
ggsave(file='testmse.png',path='/home/laucheuk/Desktop/DATA11002intro_to_ML/project',width=10,height=5)

csvmse<-ggplot(data = t[t$variable=='csv_mse',],aes(x=Approach,y=value,group=Models,color=Models))+
  geom_line()+labs(title='CV mean squared error',fill='Models')
ggsave(file='csvmse.png',path='/home/laucheuk/Desktop/DATA11002intro_to_ML/project',width=10,height=5)

accuracy<-ggplot(data = t[t$variable=='accuracy',],aes(x=Approach,y=value,group=Models,color=Models))+
  geom_line()+labs(title='Accuracy',fill='Models')
ggsave(file='acc.png',path='/home/laucheuk/Desktop/DATA11002intro_to_ML/project',width=10,height=5)
