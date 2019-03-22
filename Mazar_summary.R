###setting----
library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(mice)
library(ggplot2)
# files path
path='/Users/pei/Documents/💪Predicting /Mazar et al. (2008)'
setwd(path)
data.files = list.files(pattern = "*.xlsx")
### loading 25 files----
for (i in 1:25){
  if (i==1){
    nam=paste('data',i,sep='')
    tmp=assign(nam,read_excel(data.files[i],sheet='data'))
    full_data=read_excel(data.files[i],sheet='data')
    data_lists<-c(nam)
  }
  else{
    nam=paste('data',i,sep='')
    tmp=assign(nam,read_excel(data.files[i],sheet='data'))
    full_data=rbindlist(list(full_data, tmp), fill = TRUE)
    data_lists<-c(data_lists,nam)
  }
}
### select experient data ----
full_data_select <- full_data %>%
  .[,!c(11:49,55,69,70,75,76,138,145:177)]%>%   #remove other research variables  
  filter(inclusion=='inclusion both RRR'|inclusion=="inclusion Mazar only")%>% #select Mazar research
  filter(is.na(reason.excl)|reason.excl=='incompletion RRR')%>% #normal and "incompletion RRR" 
  mutate_at(c(15:24,33:93),as.integer)  %>%
  mutate_at(c(2,6,8,94,96),as.factor) 
### clean data_capitalize----
##clean data_lab.name
full_data_select$lab.name<-sapply(full_data_select$lab.name, str_to_title) #lab.name('Laine'|'laine' ) #lab.name('evans'|'Evans' )
##clean data_major
full_data_select$major<-sapply(full_data_select$major, str_to_title) #capitalize
##clean kept.sheet
full_data_select$kept.sheet<-sapply(full_data_select$kept.sheet, str_to_title) #capitalize
##clean language
full_data_select$language<-sapply(full_data_select$language, str_to_title) #capitalize

#write.csv(full_data_select,'Mazar data(clean data).csv')

#treatment group(commandments) and imputation----
full_data_T<-full_data_select %>%filter(maz.prime.cond=='commandments'&maz.cheat.cond=='cheat') %>% #treatment group
  mice(.,m=1,method = 'mean',seed = 188)%>% ## Missing Value-mean
  mice::complete()
#control group(books) and imputation----
full_data_C<-full_data_select %>%filter(maz.prime.cond=='books'& maz.cheat.cond=='cheat') %>% #treatment group
  mice(.,m=1,method = 'mean',seed = 188)%>% ## Missing Value-mean
  mice::complete()

full_data_all=rbind(full_data_T,full_data_C)

### clean data_sleep.hours----

#X(w/o major,kept.sheet,)
X_y<-full_data_select[c(2,6:7,16:24,33:93,15)]  #y:15
X<-colnames(full_data_select)[c(2,6:7,16:24,33:93)]
#y
y<-colnames(full_data_select)[15]
##reg model
fmla <- as.formula(paste(y,'~', paste(X, collapse= "+")))

#overall anovatest<-person_pred_both(full_data_all,full_data_T,full_data_C)
ATE <- lm(fmla,test)
anova(ATE)

full_data_all%>%
  ggplot()+
  aes(maz.prime.cond,fill=maz.prime.cond)+
  geom_density(col=NA,alpha=0.3)
