library(tabulizer)

mr9= extract_areas('http://www.ncrb.gov.in/StatPublications/CII/CII2009/cii-2009/Table%203.1.pdf',1)[[1]]
mr9 = as.data.frame(mr9)


mr10= extract_areas('http://www.ncrb.gov.in/StatPublications/CII/CII2010/cii-2010/Table%203.1.pdf',1)[[1]]
mr10 = as.data.frame(mr10)

mr11= extract_areas('http://www.ncrb.gov.in/StatPublications/CII/CII2011/cii-2011/Table%203.1.pdf',1)[[1]]
mr11 = as.data.frame(mr11)

mr12= extract_areas('http://www.ncrb.gov.in/StatPublications/CII/CII2012/cii-2012/Table%203.1.pdf',1)[[1]]
mr12 = as.data.frame(mr12)


mr13= extract_areas('http://www.ncrb.gov.in/StatPublications/CII/CII2013/CII13-TABLES/Table%203.1.pdf',1)[[1]]
mr13 = as.data.frame(mr13)

mr16= extract_areas('2016.pdf',138)[[1]]
mr16 = as_data_frame(mr16)

mr16$V2 = as.numeric(mr16$V2)
mr16$V3 = as.numeric(mr16$V3)
mr16$V4 = as.numeric(mr16$V4)
mr16$V2[1] = mr16$V2[1]+mr16$V2[25] # combining Andhra n Telangana
mr16$V3[1] = mr16$V3[1]+mr16$V3[25] # combining Andhra n Telangana
mr16$V4[1] = mr16$V4[1]+mr16$V4[25] # combining Andhra n Telangana

mr16 = mr16 %>% filter(! (V1=="Telangana"))

mr16 = mr16 %>% rename(y_2014=V2,y_2015=V3,y_2016=V4) %>% 
  add_column(y_2009 = mr9$V2, y_2010 = mr10$V2,y_2011=mr11$V2,y_2012=mr12$V2,y_2013=mr13$V2) %>%
  gather(year,value,-V1) %>% 
  separate(year,c("y","YEAR"),sep="_") %>% 
  select(-y) %>% 
  rename(year=YEAR) %>% 
  arrange(year) %>% 
  rename(states=V1) %>% 
  arrange(states)


m19$murder = as.numeric(mr16$value)
m19$murder_rate= m19$murder/m19$totalpopulation

m19 %>% group_by(states) %>% summarise(mean_murder_rate=mean(murder_rate)) %>% arrange(mean_murder_rate) %>% View

popl$population


                  ?case_when