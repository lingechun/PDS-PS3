#1
#download and subset data
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%unique(primaryPolls$candidate_name),]
#plot by state
ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  facet_wrap(~ state, nrow=5)
#change to the minimial theme
pl <- ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  facet_wrap(~ state, nrow=5)
pl + theme_minimal()
#change the axis labels and legends
pl + labs(x="Start Date", y="Percentage") + theme(legend.position="bottom")
