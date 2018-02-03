median(crime$crime_rate)

median(crime$crime_rate,na.rm = TRUE)

crime = m19
p13= crime %>% filter(year=="2013",crime_reported>50) %>% # select year 2009 and greater than 50 cases to avoid low_base states
  mutate(income_status = factor(income_status,levels=c("High","Mid","Low"))) %>% # reorder for preserving states in order
  ggplot(aes(x=crime_rate,y=reorder(states,crime_rate),color=party)) +  
  geom_vline(xintercept = z, color = "gray30") +
  geom_point(size = 2)+
  scale_color_manual(values = party_colors)+
  facet_wrap(~ income_status,ncol=1,scales="free_y")+
  guides(color=FALSE) + labs(x = "Crime rate against SC per Lakh of Population in 2013 ", y = "") +
  theme(axis.text=element_text(size=8))


p16=crime %>% filter(year=="2016",crime_reported>50) %>% # select year 2009 and greater than 50 cases to avoid low_base states
  mutate(income_status = factor(income_status,levels=c("High","Mid","Low"))) %>% # reorder for preserving states in order
  ggplot(aes(x=crime_rate,y=reorder(states,crime_rate),color=party)) +  
  geom_vline(xintercept = z, color = "gray30") +
  geom_point(size = 2)+
  scale_color_manual(values = party_colors)+
  facet_wrap(~ income_status,ncol=1,scales="free_y")+
  guides(color=FALSE) + labs(x = "Crime rate against SC per Lakh of Population in 2016 ", y = "") +
  theme(axis.text=element_text(size=8))

p16

