#get bonus
load("data/empirical_data/data_filtered/RL.rdata")
bonus=df%>%group_by(subject_id)%>%summarise(total_bonus=sum(bonus,na.rm=T))

session1=read.csv("data/empirical_data/data_collected/demographic/prolific_export_67644513403063068852f03f.csv")

subjects_1=session1%>%filter(Status=="APPROVED")%>%select(Participant.id)
bonus_1=bonus%>%filter(subject_id%in%subjects_1$Participant.id)
bonus_1$subject_id=paste0(bonus_1$subject_id,",")
bonus_1$total_bonus=round(bonus_1$total_bonus,2)
write.csv(bonus_1,file="data/empirical_data/data_collected/demographic/bonus.csv")


