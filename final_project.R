library(ggplot2)
#reading data from datasets
dat = read.csv("deliveries.csv", sep = ",", h = TRUE)
matches = read.csv("matches.csv", sep = ",", h = TRUE)

#highest runs by players
batting_runs<-aggregate(total_runs~batsman,data=dat,sum)
batting_runs<-batting_runs[order(batting_runs$total_runs, batting_runs$batsman,decreasing = TRUE) ,]
ggplot(batting_runs[1:10,],aes(x=batsman,y=total_runs, fill = batsman))+
  geom_bar(stat="identity")+
  scale_x_discrete("Top 10 batsman")+
  scale_y_continuous("Runs",breaks=seq(1000,5000,500)) +
  theme(axis.text.x=element_text(angle=75,vjust = 0.5))+
  ggtitle("Highest runs by player")   


#Batsman VS 4'sand 6's 
team_4 = subset(dat,  dat$batsman_runs == 4)
team_5 = data.frame(table(unlist(team_4$batting_team)))
team_6 = subset(dat,  dat$batsman_runs == 6)
team_7 = data.frame(table(unlist(team_6$batting_team)))
f = data.frame(Team_name = team_7$Var1,Total_6 = team_7$Freq, Total_4 = team_5$Freq)
ggplot(f, aes(fill = Total_6, y = Total_4,x = Team_name)) + geom_bar(position = "dodge", stat = "identity")+theme(axis.text.x = element_text(angle = 45, vjust = 0.2))


#Team_name VS winning_rate
win = matches$winner
team_win = data.frame(table(unlist(win)))
pie3D(team_win$Freq, labels = team_win$Var1,main = "Team Winning rate", explode = 0, cex = 0.1, labelcex = 0.7, las = 3, shade = 1.5,color=f5$Var1)


#Regression for extraruns Vs Win By Runs
w = aggregate(dat$extra_runs~dat$match_id, data = dat,FUN = sum)
da = data.frame(w$`dat$extra_runs`, matches$win_by_runs)
da1 = data.frame(table(unlist(da)))
x = c(da1$Var1)
y = c(da1$Freq)
relation = lm(y~x)
plot(x, y, col = y, main = "Extraruns and WinByRuns", abline(lm(y~x)), cex = 1.3, pch = 16, xlab = "ExtraRuns", ylab = "WinByRuns")
ggplot(da1[2:83,],aes(x=Var1,y=Freq, fill = Var1))+
  geom_bar(stat="identity")+
  scale_x_discrete("Extra Runs")+
  scale_y_continuous("Win By Runs",breaks=seq(0,300,10)) +
  theme(axis.text.x=element_text(angle=50,vjust = 0.2))+
  ggtitle("Win By Run Probability")   

#performance of the  one team when it  is in team1
p=subset(matches,matches$team1=="Kolkata Knight Riders")
p=table(p$winner)
p=data.frame(p)
qplot(p$Var1,p$Fre,data=p,color=Var1)

#......when iit is in the second team
p1=subset(matches,matches$team2=="Kolkata Knight Riders")
p1=data.frame(p1$team1,p1$team2,p1$winner)
p1=table(p1$p1.winner)
p1=data.frame(p1)
qplot(p1$Var1,p1$Fre,data=p1,color=Var1)
kkr=data.frame(p,p1)
kkr$winngrate=kkr$Freq+kkr$Freq.1
kkr1=data.frame(teams=kkr$Var1[2:14],winnings=kkr$winngrate[2:14])
qplot(kkr1$teams,kkr1$winnings,color=kkr1$teams)

#when another teamchecking
csk=subset(matches,matches$team1=="Chennai Super Kings")
csk1=data.frame(csk$team1,csk$team2,csk$winner)
csk_f=table(csk1$csk.winner)
csk_df=data.frame(csk_f)

###........in second team...........
csk_2=subset(matches,matches$team2=="Chennai Super Kings")
csk2=data.frame(csk_2$team1,csk_2$team2,csk_2$winner)
csk_f1=table(csk2$csk_2.winner)
csk_df1=data.frame(csk_f1)
csk_1=data.frame(csk_df,csk_df1)
csk_1$winngrate=csk_1$Freq+csk_1$Freq.1
ccsk=data.frame(teams=csk_1$Var1[2:14],winning=csk_1$winngrate[2:14])
comp=data.frame(team=ccsk$teams,cwining=ccsk$winning,kwinning=kkr1$winnings)
ggplot(comp,aes(x = team,y = cwining,fill=team))+
  geom_bar(stat="identity",width=1)+
  theme(axis.text.x = element_text(angle =35,vjust = 0.5))+
  ggtitle("ChennaiSuperKings VS AllTeams")
ggplot(comp,aes(x = team,y = kwinning,fill=team))+
  geom_bar(stat="identity",width=1)+
  theme(axis.text.x = element_text(angle =35,vjust = 0.5))+
  ggtitle("KnightRiders VS AllTeams")
