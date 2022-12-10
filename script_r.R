rm(list = ls()) 
library(haven)
u_data_raw<- read_dta("C:/Urban/Data/113848-V1/usa_00074_labeled.dta")

library(dplyr)
library(data.table)

u_data <- u_data_raw
setDT(u_data)

#First, generate the instrument;
u_data <- u_data[gq == 1|gq == 2]


u_data <- u_data[ !(year == 1900 | year == 1940)]

u_data <- u_data[, yob := year-age]

is.na(u_data$yrimmig)
u_data <- mutate(u_data, yrimmig = ifelse (!yrimmig == 0, yrimmig+1000,0))



u_data <- mutate(u_data, yrimmig = ifelse (yrimmig == 0, NA ,yrimmig))
u_data$immig==1 & is.na(u_data$yrimmig)
u_data <- mutate(u_data, immig = ifelse (bpld>=15000 & bpld<90000, 1 , 0))
u_data<- u_data[!(immig==1 & is.na(u_data$yrimmig))]
u_data<- u_data[!(immig==1 & yrimmig<yob)]



u_data$ageimmig <- u_data$yrimmig - u_data$yob

#Generates 3 immigrant categories: young, baby and old;
u_data$young_imm <- ifelse(u_data$ageimmig>=10 & u_data$ageimmig<=25 & u_data$immig==1 & u_data$sex==1 | u_data$ageimmig>=8 & u_data$ageimmig<=23 & u_data$immig==1 & u_data$sex==2, 1, 0)

u_data$baby_imm <- ifelse(u_data$ageimmig<10 & u_data$immig==1 & u_data$sex==1 | u_data$ageimmig<8  & u_data$immig==1 & u_data$sex==2, 1, 0)

u_data$old_imm <- ifelse(u_data$ageimmig>25 & u_data$immig==1 & u_data$sex==1 | u_data$ageimmig>23  & u_data$immig==1 & u_data$sex==2, 1, 0)


#Generates natives and second generations

u_data$secondgen <- ifelse (u_data$mbpld>=15000 & u_data$mbpld<90000 | u_data$fbpld>=15000 & u_data$fbpld<90000 & u_data$bpld<15000 , 1, 0)

u_data$secondgen <- ifelse (u_data$mbpld>=15000 & u_data$mbpld<90000 | u_data$fbpld>=15000 & u_data$fbpld<90000 & u_data$bpld<15000 , 1, 0)

u_data$native <- ifelse (u_data$bpld<15000 & u_data$mbpld<15000 & u_data$fbpld<15000, 1,0)

u_data<- u_data[!(u_data$native==0 & u_data$secondgen==0 & u_data$immig==0)]

#Generate ethnicities which are later on grouped into ethnic group according to Table C1

u_data$ethnic <- ifelse  (u_data$native==1|u_data$secondgen==1, 0, NA)

u_data$ethnic <- ifelse (u_data$bpld>=60000 & u_data$bpld<=60099,1, u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=16000 & u_data$bpld<=160601, 2,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=70000 & u_data$bpld<=70000, 3,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=45000 & u_data$bpld<=45080, 4,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==42000, 5,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=45200 & u_data$bpld<=45212, 6,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=15000 & u_data$bpld<= 15070, 7,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=15080 & u_data$bpld<= 15083, 8,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$ethnic==7 & u_data$mtongue==11, 8,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=21000 & u_data$bpld<= 21090, 9,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=50000 & u_data$bpld<= 50040, 10,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==41900 | u_data$bpld==42200 | u_data$bpld==42400 | u_data$bpld==43000 | u_data$bpld==43100 | u_data$bpld==43200| u_data$bpld==43500 | u_data$bpld==43700 |u_data$bpld==44000| u_data$bpld==45100| (u_data$bpld>=45700 & u_data$bpld<=45900)|u_data$bpld==41900|u_data$bpld==42900|u_data$bpld==44000|u_data$bpld==49900, 11,  u_data$ethnic)


u_data$ethnic <- ifelse (u_data$bpld==25000, 12,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==40000| u_data$bpld==40200, 13,  u_data$ethnic)

u_data$ethnic <- ifelse ((u_data$bpld>=41000 & u_data$bpld<=41020) | u_data$bpld==41300, 14,  u_data$ethnic)


u_data$ethnic <- ifelse (u_data$bpld==40100, 15,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==42100, 16,  u_data$ethnic)

u_data$ethnic <- ifelse ((u_data$bpld>=45300 & u_data$bpld<=45362) | (u_data$bpld>42100 & u_data$bpld<=42112), 17,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=43300 & u_data$bpld<=43330, 18,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==43300, 19,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=52100 & u_data$bpld<=52150, 20,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=41400 & u_data$bpld<=41410, 21,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==43400, 22,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==50100, 23,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==42300, 24,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==20000, 25,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==42500, 26,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==40400, 27,  u_data$ethnic)

u_data$ethnic <- ifelse (( u_data$bpld>=50200 &  u_data$bpld<=52000) | ( u_data$bpld>=52200 &  u_data$bpld<=54000) | ( u_data$bpld<=54300 &  u_data$bpld>=59900), 28,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=71000 & u_data$bpld<=71050, 30,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=45500 & u_data$bpld<=45530, 31,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=43600 & u_data$bpld<=43640, 32,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=45600 & u_data$bpld<=45610, 33,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=46000 & u_data$bpld<=46590, 34,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==41100 , 35,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=30000 & u_data$bpld<=30091, 36,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==43800, 37,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==40500, 38,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld== 42600, 39,  u_data$ethnic)

u_data$ethnic <- ifelse ((u_data$bpld>=53400 & u_data$bpld<=53440) | u_data$bpld==54100 | (u_data$bpld>=54200 & u_data$bpld<=54220), 40,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld==41200, 41,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=26000 & u_data$bpld<=26095, 42,  u_data$ethnic)

u_data$ethnic <- ifelse (u_data$bpld>=15000 & u_data$bpld<90000 & is.na(u_data$ethnic), 29,  u_data$ethnic)


attach(u_data)
u_data$momethnic<- ifelse (u_data$mbpld<15000, 0, NA)
u_data$momethnic <- ifelse (u_data$mbpld>=60000 & u_data$mbpld<=60099, 1,u_data$momethnic )
u_data$momethnic<- ifelse (u_data$mbpld>=16000 & u_data$mbpld<=16060,2,u_data$momethnic)
u_data$momethnic<- ifelse (u_data$mbpld>=70000 & u_data$mbpld<=70010,3,u_data$momethnic)
u_data$momethnic<- ifelse (u_data$mbpld>=45000 & u_data$mbpld<=45080,4,u_data$momethnic)
u_data$momethnic<-ifelse (u_data$mbpld==42000, 5,u_data$momethnic)
u_data$momethnic<- ifelse (u_data$mbpld>=45200 & u_data$mbpld<=45212, 6, u_data$momethnic);
u_data$momethnic<-ifelse (u_data$mbpld>=15000 & u_data$mbpld<=15070, 7, u_data$momethnic)
u_data$momethnic<- ifelse (u_data$mbpld>=15080 & u_data$mbpld<=15083, 8, u_data$momethnic);
u_data$momethnic<- ifelse (u_data$momethnic==7 & u_data$mmtongue==11,8,u_data$momethnic);
u_data$momethnic<- ifelse (u_data$mbpld>=21000 & u_data$mbpld<=21090, 9,u_data$momethnic);
u_data$momethni<- ifelse (u_data$mbpld>=50000 & u_data$mbpld<=50040, 10, u_data$momethnic);
u_data$momethnic<- ifelse (u_data$mbpld==41900 | u_data$mbpld==42200 | u_data$mbpld==42400 | u_data$mbpld==43000 | u_data$mbpld==43100 | u_data$mbpld==43200| u_data$mbpld==43500 | u_data$mbpld==43700 | u_data$mbpld==44000 | u_data$mbpld==45100|(u_data$mbpld>=45700 & u_data$mbpld<=45900)|u_data$mbpld==41900|u_data$mbpld==42900|u_data$mbpld==44000|u_data$mbpld==49900, 11, u_data$momethnic)
u_data$momethnic<- ifelse (u_data$mbpld==25000, 12, u_data$momethnic);
u_data$momethnic<- ifelse (u_data$mbpld==40000|u_data$mbpld==40200, 13, u_data$momethnic);
u_data$momethnic<- ifelse ((u_data$mbpld>=41000 & u_data$mbpld<=41020) | u_data$mbpld==41300, 14, u_data$momethnic);
u_data$momethnic<- ifelse (u_data$mbpld==40100, 15, u_data$momethnic);
u_data$momethni<- ifelse (u_data$mbpld==42100, 16, u_data$momethnic);
u_data$momethnic<- ifelse ((u_data$mbpld>=45300 & u_data$mbpld<=45362) |(u_data$mbpld>42100 & u_data$mbpld<=42112), 17, u_data$momethnic);
u_data$momethnic<- ifelse(u_data$mbpld>=43300 & u_data$mbpld<=43330, 18, u_data$mbpld)
u_data$momethnic <- ifelse (u_data$mbpld==45400, 19, u_data$mbpld)
u_data$momethnic<- ifelse (u_data$mbpld>=52100 & u_data$mbpld<=52150, 20, u_data$mbpld);
u_data$momethnic<- ifelse (u_data$mbpld>=41400 & u_data$mbpld<=41410, 21, u_data$mbpld)

# Used stata for the rest of the replace 
library(haven)
library(data.table)
library(dplyr)
replaced_data<- read_dta("C:/Urban/Data/113848-V1/df_replaced.dta")


#For second generation, use the ethnicity of the father if foreign-born and then of the mother if the father is native born

replaced_data$ethnic<- ifelse (is.na(replaced_data$ethnic)|replaced_data$ethnic==0, replaced_data$dadethnic, replaced_data$ethnic)

replaced_data$ethnic<- ifelse (is.na(replaced_data$ethnic)|replaced_data$ethnic==0, replaced_data$momethnic, replaced_data$ethnic)

#replaced_data <- replaced_data[!is.na(replaced_data$ethnic)]

replaced_data <- replaced_data %>% 
  filter(! is.na(ethnic))
sum(!is.na(replaced_data$ethnic))


replaced_data$female <- ifelse(replaced_data$sex==2,1,0)

replaced_data$male <- ifelse(replaced_data$sex==1,1,0)

replaced_data$male_young <- replaced_data$male * replaced_data$young_imm

replaced_data$female_young <- replaced_data$female * replaced_data$young_imm

replaced_data$male_baby <- replaced_data$male * replaced_data$baby_imm

replaced_data$male_old <- replaced_data$male * replaced_data$old_imm

replaced_data$male_sec <- replaced_data$male * replaced_data$secondgen

replaced_data$female_baby <- replaced_data$female * replaced_data$baby_imm

replaced_data$female_old <- replaced_data$female * replaced_data$old_imm

replaced_data$female_sec <- replaced_data$female * replaced_data$secondgen

#Avoid double counting by restricting the flow/stock variables to be measured only once;



# Forloop
#Group immigrants into cohorts of immigration (or births for individuals who immigrated as children)
replaced_data$period_imm<-5*(floor(replaced_data$yrimmig/5))
replaced_data$period_imm<- ifelse(replaced_data$young_imm==0 & replaced_data$old_imm==0, 5*(floor(yob/5))+15, replaced_data$period_imm)

replaced_data <-replaced_data %>% 
  filter(period_imm>=1900 & period_imm<=1925)

nar <-  function(x) ifelse(replaced_data$year==1910 & (replaced_data$period_imm<1900 | replaced_data$period_imm>=1910) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(young_imm, baby_imm, old_imm ,secondgen, native),nar))
replaced_data %>% 
 filter(replaced_data$year==1910 & (replaced_data$period_imm<1900 | replaced_data$period_imm>=1910)) %>% 
  select(young_imm, baby_imm, old_imm ,secondgen, native)

nar1 <-  function(x) ifelse(replaced_data$year==1920 & (replaced_data$period_imm<1910 | replaced_data$period_imm>=1920) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(young_imm, baby_imm, old_imm ,secondgen, native),nar1))
replaced_data %>% 
  filter(replaced_data$year==1920 & (replaced_data$period_imm<1910 | replaced_data$period_imm>=1920)) %>% 
  select(young_imm, baby_imm, old_imm ,secondgen, native)

nar2<-  function(x) ifelse(replaced_data$year==1930 & (replaced_data$period_imm<1920 | replaced_data$period_imm>=1930) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(young_imm, baby_imm, old_imm ,secondgen, native),nar2))
replaced_data %>% 
  filter(replaced_data$year==1920 & (replaced_data$period_imm<1910 | replaced_data$period_imm>=1920)) %>% 
  select(young_imm, baby_imm, old_imm ,secondgen, native)

year==1910 & period_imm>=1910

nar3<-  function(x) ifelse(replaced_data$year==1910 & (replaced_data$period_imm>=1910) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(male, female ,female_young, male_young ,female_baby ,
                  male_baby ,female_old ,male_old, male_sec ,female_sec),nar3))


nar4<-  function(x) ifelse(replaced_data$year==1920 & (replaced_data$period_imm>=1920) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(male, female ,female_young, male_young ,female_baby ,
                  male_baby ,female_old ,male_old, male_sec ,female_sec),nar4))

nar5<-  function(x) ifelse(replaced_data$year==1930 & (replaced_data$period_imm>=1930) ,NA,x)
replaced_data <- replaced_data %>% 
  as_tibble() %>% 
  mutate(across(c(male, female ,female_young, male_young ,female_baby ,
                  male_baby ,female_old ,male_old, male_sec ,female_sec),nar5))

 