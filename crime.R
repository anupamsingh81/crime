library(tabulizer)
library(gplots)
library(lme4)
library(tidyverse)
library(plm)
tabs <- extract_tables('2009.pdf')
tabs[[1]] # doesnt work directly have to select areas

m9= extract_areas('2009.pdf',1)[[1]]
m9 = as.data.frame(m9)

m10= extract_areas('2010.pdf',1)[[1]]
m10 = as.data.frame(m10)

m11= extract_areas('2011.pdf',1)[[1]]
m11 = as.data.frame(m11)

m12= extract_areas('2012.pdf',1)[[1]]
m12 = as.data.frame(m12)

m13= extract_areas('2013.pdf',1)[[1]]
m13 = as.data.frame(m13)

m16= extract_areas('2016.pdf',336)[[1]]
m16 = as.data.frame(m16)

m16$y2014 = as.integer(as.character(m16$V2))
m16$y2015 = as.integer(as.character(m16$V3))
m16$y2016 = as.integer(as.character(m16$V4))
m16$V2 = NULL
m16$V3 = NULL
m16$V4 = NULL

m16$y2014[1] = m16$y2014[1]+m16$y2014[25] # combining Andhra n Telangana

m16$y2015[1] = m16$y2015[1]+m16$y2015[25]

m16$y2016[1] = m16$y2016[1]+m16$y2016[25]

library(tidyverse)
m16.1= m16
m16 = m16 %>% rename(states = V1) %>% 
filter(!(states=="Telangana"))

m16$y2009 = as.integer(as.character(m9$V2))


m16$y2010 = as.integer(as.character(m10$V2))
m16$y2011 = as.integer(as.character(m11$V2))
m16$y2012 = as.integer(as.character(m12$V2))
m16$y2013 = as.integer(as.character(m13$V2))

mpop = extract_areas('2013.pdf',1)[[1]]
mpop = as.data.frame(mpop)
m16$SC_population = as.double(as.character(mpop$V1))

m16.2 =m16
m16 = m16.2 %>% gather(year,crime_reported,-states,-SC_population) # gather all except states and SC population
#
library(stringr)

str(m16$year)
m16$YEAR = str_replace(m16$year,"y","y_")
m16$year = NULL
m16 = m16 %>% separate(YEAR, c("y", "year")) 
  m16$y = NULL
  
  m16 %>% group_by(year) %>% summarise(total_crime_reported=sum(crime_reported))
#alter year

library(lme4)
lmer(crime_reported~SC_population+1|states,data=m16)

cumsum(as.integer(as.character(m9$V2)))

       cumsum(m16.2$y2014)
      
       
       cumsum(as.integer(as.character(m16.2$V2)))
       
       
       
       m1 = m16 %>% filter(crime_reported>50) %>% arrange(year) 
      m1= m1 %>% mutate(crime_rate = crime_reported/SC_population)
       library(gganimate)
       p <- ggplot(m1, aes(SC_population, crime_reported, size = SC_population, color = states, frame = year)) +
         geom_point() +
         scale_x_log10()
       p1 = ggplot(m1, aes(SC_population, crime_rate, size = SC_population, color = states, frame = year)) +
         geom_point() +
         scale_x_log10()
       
       gganimate(p1,interval=0.5)
       
       library(car)
       scatterplot(crime_rate~year|states, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=m1)
       
       
    ?scatterplot   
       
       p3 <- ggplot(m1, aes(crime_reported, crime_rate,color=states, frame = year)) +
         geom_path(aes(cumulative = TRUE, group = states)) +
         scale_x_log10() +
         facet_wrap(~states)
       
       gganimate(p3,interval =1)
       
       ?gganimate
       
       # percentage change
       
    m17=    m16.2 %>% arrange(year) %>% arrange(states) %>% group_by(states) %>% 
         mutate(percent_change = (crime_reported - lag(crime_reported))/lag(crime_reported) * 100, crime_rate= crime_reported/SC_population ) 
         
    m18 = m17 %>% filter( !year==2009,crime_reported>20) %>% group_by(states,year) %>% summarise(mean_percent_change= mean(percent_change))
    
    m17 %>% filter( !year==2009,crime_reported>100) %>% group_by(year) %>% summarise(mean_percent_change= mean(percent_change))
    
    
    
    write.csv(m17,"m17.csv")
    m19 = read.csv("m17.csv")
    m16.2$totalpopulation = popl$population
    m16.2$sc_percent = m16.2$SC_population/m16.2$totalpopulation
  
    m19 %>% filter(crime_reported>20) %>% group_by(party,year) %>% summarise(mean_crime_rate = mean(crime_rate), mean_crime_reported = mean(crime_reported))
    
   m20 = m19 %>% filter(crime_reported>200)
    summary(lm(crime_rate~as.factor(year)+as.factor(party),data=m20))
    
    t.test(m20$crime_reported~m20$party)
    
   
    popl = read_csv("census.csv")
    popl = popl %>% arrange(states)
    popl$population = popl$population/100000
    m22 = data.frame(states=m16.2$states,totalpopulation=popl$population)
   m19 =  left_join(m19,m22,by=c("states"))
   
   m19=m19 %>% mutate(totalpopulation = case_when(states=="Uttar Pradesh" ~1995.81,
                                                  states=="Uttarakhand" ~101.16,
                                                  TRUE ~ totalpopulation) )
   
    left_join(m19,m21,by=c("states"))
    m19$totalpopulation = popl$population
    m19$sc_percent = (m19$SC_population/m19$totalpopulation)*100
    
    m21 = m19 %>% filter(crime_reported>20)
    plotmeans(crime_rate ~ states, main="Heterogeineity across states", data=m21)
    
    plotmeans(crime_rate ~ year, main="Heterogeineity across years", data=m21)
  
    
    library(lme4)
    
    
    
    expl_model = lmer(crime_rate~party+year+(1|states),data=m21)
    expl_model1= lmer(crime_rate~party+year+(0+year|states),data=m21)
    expl_model2 = lmer(crime_rate~party+year+(1+year|states),data=m21)
    expl_model3 = lmer(crime_rate~party+(1+party|states),data=m21)
    expl_model4 = lmer(crime_rate~year+party+(1+party|states),data=m21)
    summary(expl_model)
    summary(expl_model3)
    summary(expl_model4)
    
    summary(m21$crime_rate)
    
    
    # panel data
    
    fixed <- plm(crime_reported~ SC_population+party, data=m21, index=c("states", "year"), model="within")
    random <- plm(crime_reported~ SC_population+party, data=m21, index=c("states", "year"), model="random")
    pool <- plm(crime_reported~ SC_population+party, data=m21, index=c("states", "year"), model="pooling") # regular ols(linear regression,all pooled)
    
    phtest(fixed,random)
    
    # here hausman test value more than p>0.05, hence random
    
    # Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
    plmtest(pool, type=c("bp"))
    # Thus her simple pooling shouldnt work, we do partial pooling..
    
    summary(random)
    
    # Here we see party upa effect inconsistent. crime 17 incr with incr 1 lakh of sc popln, p-value <0.05, upa no effect
    ranef(random)
    fixef(fixed)
    
    random1 <- plm(crime_reported~SC_population+totalpopulation+ party, data=m21, index=c("states", "year"), model="random")
    ranef(random1)
    summary(random1)
    
    random2 <- plm(crime_reported~SC_population+sc_percent+ party, data=m21, index=c("states", "year"), model="random")
    summary(random2)
    
    random4 <- plm(crime_reported~sc_percent+SC_population+murder_rate+ party, data=m21, index=c("states", "year"), model="random")
    summary(random4)
   
  cor.test(m21$murder_rate,m21$crime_rate)