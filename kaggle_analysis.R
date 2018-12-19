library(dplyr)

player_play_punts <- read.csv("play_player_role_data.csv")
injuries <- read.csv("video_review.csv")
punts <- read.csv("play_information.csv")

punts$key = paste0(punts$GameKey,"-",punts$PlayID)
injuries$key = paste0(injuries$GameKey,"-",injuries$PlayID)
player_play_punts$key = paste0(player_play_punts$GameKey,"-",player_play_punts$PlayID)

### Find how many injury plays were 1. fair catches 2. out-of-bounds 3. touchbacks 4. downed punts and the same for all the punts in the dataset
## calculate the rate of concussions for each type of punts 

punts_data = read.csv("play_information.csv")
punts_data$key = paste0(punts_data$GameKey,"-",punts_data$PlayID)

punts_data$PlayDescription = as.character(punts_data$PlayDescription)
fc_punts = length(which(grepl("fair catch",punts_data$PlayDescription)))
oob_punts = length(which(grepl("out of bounds",punts_data$PlayDescription))) # these do not include returns where the returner was pushed out-of-bounds since those include "pushed ob" 
tback_punts = length(which(grepl("Touchback",punts_data$PlayDescription)))
downed_punts = length(which(grepl("downed",punts_data$PlayDescription)))

injuries_data = read.csv("video_footage-injury.csv")

injuries_data$PlayDescription= as.character(injuries_data$PlayDescription)

fc_injury = length(which(grepl("fair catch",injuries_data$PlayDescription)))
oob_injury = length(which(grepl("out of bounds",injuries_data$PlayDescription)))
tback_injury = length(which(grepl("Touchback",injuries_data$PlayDescription)))
downed_injury = length(which(grepl("downed",injuries_data$PlayDescription)))

sprintf("================Injury rates================")
sprintf("Fair Catch: %f%%",100*(fc_injury/fc_punts))
sprintf("Out-of-bounds: %f%%",100*(oob_injury/oob_punts))
sprintf("Touchbacks: %f%%",100*(tback_injury/tback_punts))
sprintf("Downed: %f%%",100*(downed_injury/downed_punts))
sprintf("Returned: %f%%",100*((dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury))/(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts))))

type = c("Fair Catch","Out-of-bounds","Touchback","Downed","Returned")
rate = c(100*(fc_injury/fc_punts),100*(oob_injury/oob_punts),100*(tback_injury/tback_punts),100*(downed_injury/downed_punts),100*((dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury))/(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts))))
rate.lower = 100*c(prop.test(x=fc_injury,n=fc_punts)$conf.int[1],prop.test(x=oob_injury,n=oob_punts)$conf.int[1],prop.test(x=tback_injury,n=tback_punts)$conf.int[1],prop.test(x=downed_injury,n=downed_punts)$conf.int[1],prop.test(x=(dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury)),n=(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)))$conf.int[1])
rate.upper = 100*c(prop.test(x=fc_injury,n=fc_punts)$conf.int[2],prop.test(x=oob_injury,n=oob_punts)$conf.int[2],prop.test(x=tback_injury,n=tback_punts)$conf.int[2],prop.test(x=downed_injury,n=downed_punts)$conf.int[2],prop.test(x=(dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury)),n=(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)))$conf.int[2])
samples = c(fc_punts,oob_punts,tback_punts,downed_punts,(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)))

bar.data <- data.frame(Type=type,Rate=rate,lower = rate.lower, upper = rate.upper, SampleSize = samples)

blue.bold.16 = element_text(face = "bold", color = "blue", size = 16)
ggplot(bar.data,aes(Type,Rate,fill=SampleSize))+geom_col()+geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"),legend.title=blue.bold.16)+ylab("Concussion Rate (%)")


## Aggregate returned/non-returned

aggr.data <- data.frame(Type = c("Returned","Non Returned"), Rate = c(100*((dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury))/(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts))),100*(fc_injury+oob_injury+tback_injury+downed_injury)/(fc_punts+oob_punts+tback_punts+downed_punts)), lower = c(100*prop.test(x=(dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury)),n=(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)))$conf.int[1],100*prop.test(x=fc_injury+oob_injury+tback_injury+downed_injury,n=fc_punts+oob_punts+tback_punts+downed_punts)$conf.int[1]),upper = c(100*prop.test(x=(dim(injuries_data)[1]-(fc_injury+oob_injury+tback_injury+downed_injury)),n=(dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)))$conf.int[2],100*prop.test(x=fc_injury+oob_injury+tback_injury+downed_injury,n=fc_punts+oob_punts+tback_punts+downed_punts)$conf.int[2]))

ggplot(aggr.data,aes(Type,Rate)+geom_col()+geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"),legend.title=blue.bold.16)+ylab("Concussion Rate (%)")

#### Extract the NGS for the returned punts and find y-coordinate at the time the returner receives the punt
'%!in%' <- function(x,y)!('%in%'(x,y))


# the NGS_punts.csv is a big file with all the NGS data. You can create this through a cat command: "cat NGS-201* > NGS_punts.csv"
ngs.data <- read.csv("NGS_punts.csv")

ngs.data$key = paste0(ngs.data$GameKey,"-",ngs.data$PlayID)

ind <- c(which(grepl("fair catch",punts_data$PlayDescription)), which(grepl("out of bounds",punts_data$PlayDescription)), which(grepl("Touchback",punts_data$PlayDescription)),which(grepl("downed",punts_data$PlayDescription)))

loc.punt.rec <- data.frame(key=c(),y=c())

for (i in 1:dim(punts_data)[1]){

	if (i %!in% ind){
		
		ngs.tmp <- ngs.data[which(ngs.data$key == punts_data[i,]$key),]		
		if ( dim(ngs.tmp)[1] > 0){
			# find the returer
			punts.roles.tmp <- player_play_punts[which(player_play_punts$key == punts_data[i,]$key),]
			pr = punts.roles.tmp[which(punts.roles.tmp$Role == "PR"),]$GSISID
			# find his y-coord during the reception of the punt "punt_received"
			y = ngs.tmp[which(as.integer(as.numeric(as.character(ngs.tmp$GSISID))) == pr & as.character(ngs.tmp$Event) == "punt_received"),]$y
			if (length(as.numeric(as.character(y))) > 0){
				loc.punt.rec <- rbind(loc.punt.rec,data.frame(key=punts_data[i,]$key,y=as.numeric(as.character(y))))
			}
		}

	}

}

# find the closest sideline 

d.side <- data.frame(key=c(),d = c())

for (i in 1:dim(loc.punt.rec)[1]){

	d = min(abs(53.3-loc.punt.rec[i,]$y),abs(loc.punt.rec[i,]$y-0)) # use abs because in 2 cases the y-coord is greater than 53.3 most probably due to either measurement error
	d.side <- rbind(d.side,data.frame(key=loc.punt.rec[i,]$key,d = d))

}

d.side <- d.side %>% mutate(inj = ifelse(key %in% injuries$key,1,0))

dsidinj.mod <- glm(inj~d,data=d.side,family="binomial")

distance <- c(0:25)

glm.plot <- data.frame(d=distance, prob = 100*predict(dsidinj.mod,newdata=data.frame(d=distance),type="response"),se = 100*predict(dsidinj.mod,newdata=data.frame(d=distance),type="response",se=T)$se.fit)


ggplot(glm.plot,aes(d,prob))+geom_line(color="blue")+geom_errorbar(aes(ymin=prob-se, ymax=prob+se), colour="black", width=.1)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"))+ylab("Concussion Incident Probability of a Returned Punt(%)") + xlab("Distance from the closest sideline (yards)")