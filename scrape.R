library(rvest)

u = 'https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_GDP_per_capita'

gdpsc = read_html(u)


html_table(html_nodes(gdpsc, "table")[[2]])
library(dplyr)
u %>% html_node() %>% html_table()

/html/body/div[3]/div[3]/div[4]/div/table[3]
.wikitable

html_nodes(gdpsc,"table") ## see which table 
# we need table 3
#http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html

gdpcapita = gdpsc %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table() %>% .[[1]] %>% as_data_frame()

colnames(gdpcapita)[3]= "gdp_per_capita"
colnames(gdpcapita)[2]= "states"
library(tidyverse)
gdpcap1 = tibble(states =toupper(as.character(unique(m16$states))))

gdpcapita$states = toupper(gdpcapita$states)

gdpcap2 = right_join(gdpcapita,gdpcap1,by="states")  #(rightjoin removes columns which are not in matching column in this case columns of gdpcap1 only preserved

library(stringr)

k =str_extract_all(gdpcap2$gdp_per_capita, "\\([^()]+\\)")

k
k <- substring(k, 2, nchar(k)-1)

k=str_replace_all(k,"US","")

k <- str_extract_all(j, "\\([^()]+\\)")[[1]]
# Remove parenthesis
k <- substring(k, 2, nchar(k)-1)

k =  str_replace_all(k,"US\\$","")
k=str_replace_all(k,"\\,","")

str(k)

which(is.na(k))

k[10] = 1800

gdpcap2$gdp_per_capita = k

m19$gdp_per_capita= as.numeric(rep(gdpcap2$gdp_per_capita,each=8))




###literacy

ur = read_html('https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_literacy_rate')

lit= ur%>% html_nodes("table") %>% 
  .[2] %>%
  html_table() %>% .[[1]] %>% as_data_frame()

colnames(lit)[2] = "states"

lit$literacy_percent
lit$literacy_percent = lit$literacy_percent[!(str_replace_all(lit$literacy_percent,"\\[[a-z]+\\]"))]

k1 =str_replace_all(lit$literacy_percent,"\\[[a-z]+\\]","")

lit$literacy_percent = k1

lit$literacy_percent = as.numeric(lit$literacy_percent)

lit$states  = toupper(lit$states)
lit2 = right_join(lit,gdpcap1,by="states") 

m19$literacy_percent = as.numeric(rep(lit2$literacy_percent,each=8))

#(rightjoin removes columns which are not in matching column in this case columns of gdpcap1 only preserved

summary(m19$literacy_percent)
summary(m19$gdp_per_capita)
boxplot(m19$literacy_percent)
boxplot(m19$gdp_per_capita)

#Dividing data into quintile

m19 %>% mutate(income_status= ntile(gdp_per_capita,3),literacy_status=ntile(literacy_percent,3)) %>% View

m19 = m19 %>% mutate(income_status= ntile(gdp_per_capita,3),literacy_status=ntile(literacy_percent,3)) %>% 
  mutate(income_status = case_when( income_status==1 ~"Low",
                                    income_status==2 ~"Mid",
                                    TRUE ~"High"),literacy_status = case_when( literacy_status==1 ~"Low",
                                                                            literacy_status==2 ~"Mid",
                                                                            TRUE ~"High"))

m19 =m19 %>% mutate(gdp_per_capita = case_when(states=="Uttar Pradesh"~1100,
                                               states=="Uttarakhand"~2900,
                                               TRUE~gdp_per_capita)) 

m19= m19 %>% mutate(party=case_when(party=="n"~"NDA",
                               TRUE~"UPA"))

write.csv(m19,"crimerate.csv")

a1 = lm(crime_rate~sc_percent+SC_population+gdp_per_capita+literacy_percent+party+year,data=m19.2)

a=lm(crime_rate~sc_percent+SC_population+factor(states)+gdp_per_capita+literacy_percent+party+year,data=m19.2)

library(lme4)
b=lmer(crime_rate~sc_percent+SC_population+(1|states)+gdp_per_capita+literacy_percent+party,data=m19)

summary(a,digits=3)
library(arm)
display(a)
display(c)
display(f)
r
c= lmer(crime_rate~sc_percent+SC_population+(1|states)+(1|year)+gdp_per_capita+literacy_percent+party,data=m19.2,REML = FALSE)

d=lmer(crime_rate~sc_percent+SC_population+(SC_population|states)+gdp_per_capita+literacy_percent+party,data=m19.2,REML = FALSE)
e= lmer(crime_rate~sc_percent+SC_population+(SC_population|states)+(1|year)+gdp_per_capita+literacy_percent+party,data=m19.2,REML = FALSE)
f= lmer(crime_rate~sc_percent+SC_population+(SC_population|states)+gdp_per_capita+literacy_percent+party,data=m19.2,REML = FALSE)
AIC(a,a1,c,d,e,f)

# http://www.metafor-project.org/doku.php/tips:rma_vs_lm_lme_lmer

m19.2=m19 %>% filter(crime_reported>50)

#https://stackoverflow.com/questions/24019807/how-to-compare-a-model-with-no-random-effects-to-a-model-with-a-random-effect-us

