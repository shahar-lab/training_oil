#get bonus
load("data/empirical_data/data_filtered/RL.rdata")
bonus=df%>%group_by(subject_id)%>%summarise(total_bonus=sum(bonus,na.rm=T))

load("data/empirical_data/data_raw/demographic.rdata")

#bonus_1=bonus%>%filter(subject_id%in%subjects_1$Participant.id)
bonus$subject_id=paste0(bonus$subject_id,",")
bonus$total_bonus=round(bonus$total_bonus,2)
write.csv(bonus,file="data/empirical_data/data_collected/demographic/bonus.csv")


