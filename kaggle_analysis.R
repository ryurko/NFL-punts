library(dplyr)
library(qdapRegex)
library(stringr)
library(LearnGeom)

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
injuries_data$key = paste0(injuries_data$gamekey,"-",injuries_data$playid)
injuries_data$PlayDescription= as.character(injuries_data$PlayDescription)

fc_injury = length(which(grepl("fair catch",injuries_data$PlayDescription)))
oob_injury = length(which(grepl("out of bounds",injuries_data$PlayDescription)))
tback_injury = length(which(grepl("Touchback",injuries_data$PlayDescription)))
downed_injury = length(which(grepl("downed",injuries_data$PlayDescription)))

sprintf("================Concussion rates================")
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
			if (length(as.numeric(as.character(y))) > 0){ # making sure the coordinates are not "NAs" -- just double checking to avoid cases where the punts 
have not been correctly annotated 
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


### One of the possible "side-effects" is that a punt returner might try to catch on the fly more punts for a fair catch that other wise he would have let land and downed by the covering team. This might lead to more muffed punts so we need to examine what is the rate of concussions in muffed punts.

muffed_punts = length(which(grepl("MUFFS",punts_data$PlayDescription)))
muffed_injury = length(which(grepl("MUFFS",injuries_data$PlayDescription)))
muffed_rate.lower = c(prop.test(x=muffed_injury,n=muffed_punts)$conf.int[1])
muffed_rate.upper = c(prop.test(x=muffed_injury,n=muffed_punts)$conf.int[2])

sprintf("================Concussion rates================")
sprintf("Muffed Punts: %f%%",100*(muffed_injury/muffed_punts))
sprintf("Muffed Punts 95%% CI: [%f%%, %f%%]",100*muffed_rate.lower,100*muffed_rate.upper)

# but some muffed punts were signaled for fair catch and some were attempted to be returned. The NGS have annotations for the fair catch signal so we will find which muffed punts were signaled for fair catch and which were not (i.e., they would be attempted to be returned).

muffed_keys = punts_data[which(grepl("MUFFS",punts_data$PlayDescription)),]$key 

muffed_punts.data = data.frame(key=c(),fc=c())

for (i in 1:length(muffed_keys)){
	ngs.tmp <- ngs.data[which(ngs.data$key == muffed_keys[i]),]
	e = which(as.character(ngs.tmp$Event) == "fair_catch")
	if (length(e) > 0){
		muffed_punts.data = rbind(muffed_punts.data,data.frame(key=muffed_keys[i],fc = 1))
	}else{
		muffed_punts.data = rbind(muffed_punts.data,data.frame(key=muffed_keys[i],fc = 0))
	}
}

muffed_inj_keys = injuries_data[which(grepl("MUFFS",injuries_data$PlayDescription)),]$key

muffed_punts.data <- muffed_punts.data %>% mutate(inj = ifelse(as.character(key) %in% muffed_inj_keys,1,0))

muffed_punts_fc = length(which(muffed_punts.data$fc == 1))
muffed_punts_fc_inj = length(which(muffed_punts.data$fc == 1 & muffed_punts.data$inj == 1))
muffed_punts_nfc = length(which(muffed_punts.data$fc == 0))
muffed_punts_nfc_inj = length(which(muffed_punts.data$fc == 0 & muffed_punts.data$inj == 1))

muffed_punts_fc.lower = c(prop.test(x=muffed_punts_fc_inj,n=muffed_punts_fc)$conf.int[1])
muffed_punts_fc.upper = c(prop.test(x=muffed_punts_fc_inj,n=muffed_punts_fc)$conf.int[2])
muffed_punts_nfc.lower = c(prop.test(x=muffed_punts_nfc_inj,n=muffed_punts_nfc)$conf.int[1])
muffed_punts_nfc.upper = c(prop.test(x=muffed_punts_nfc_inj,n=muffed_punts_nfc)$conf.int[2])

muffed.rates.dat <- data.frame(type=c("All Muffed","Muffed FC","Muffed Non-FC"),rate=c(muffed_injury/muffed_punts,muffed_punts_fc_inj/muffed_punts_fc,muffed_punts_nfc_inj/muffed_punts_nfc),lower = c(muffed_rate.lower,muffed_punts_fc.lower,muffed_punts_nfc.lower),upper=c(muffed_rate.upper,muffed_punts_fc.upper,muffed_punts_nfc.upper))

# limiting the y-axis for visibility 

ggplot(muffed.rates.dat,aes(type,100*rate))+geom_col()+geom_errorbar(aes(ymin=100*lower, ymax=100*upper), colour="black", width=.1)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"),legend.title=blue.bold.16)+ylab("Concussion Rate (%)")+xlab("Punt Type")+ylim(c(0,5))

### Find the angle from the 0 degree line for the punts (returned, fair catches -- for downed punts we cannot really tell the angle since we do not know the place where the ball landed first, while for out of bounds punts we do not know which sideline they were out from) behind a team's own 30 yard line
## Steps: find the punts of interest, find the location of the punter at the time of punt and the location of the punt returner at the time of fair-catch/punt reception

ind.punt.RetFC <- c(which(grepl("out of bounds",punts_data$PlayDescription)), which(grepl("Touchback",punts_data$PlayDescription)),which(grepl("downed",punts_data$PlayDescription)))

loc.punt.RetFC <- data.frame(key=c(),yrdline=c(),x1=c(),y1=c(),x2=c(),y2=c(),theta=c())

## add an indicator in the punts_data on whether the punt is on the own territory behind the 30 yard line 

punts_data = cbind(punts_data, read.table(text = as.character(punts_data$YardLine), sep = " "))
punts_data <- punts_data %>% mutate(own30 = ifelse(as.numeric(punts_data$V2)<=30,1,0))

for (i in 1:dim(punts_data)[1]){

        if (i %!in% ind.punt.RetFC & punts_data[i,]$own30 == 1){

                ngs.tmp <- ngs.data[which(ngs.data$key == punts_data[i,]$key),]
                if ( dim(ngs.tmp)[1] > 0){
                        # find the returner
                        punts.roles.tmp <- player_play_punts[which(player_play_punts$key == punts_data[i,]$key),]
                        pr = punts.roles.tmp[which(punts.roles.tmp$Role == "PR"),]$GSISID
                        # find his xy-coord during the reception of the punt "punt_received" or the fair catch "fair_catch"
                        x2 = ngs.tmp[which(as.integer(as.numeric(as.character(ngs.tmp$GSISID))) == pr & (as.character(ngs.tmp$Event) == "punt_received" |as.character(ngs.tmp$Event) == "fair_catch" )),]$x
                        y2 = ngs.tmp[which(as.integer(as.numeric(as.character(ngs.tmp$GSISID))) == pr & (as.character(ngs.tmp$Event) == "punt_received"|as.character(ngs.tmp$Event) == "fair_catch" )),]$y
			# find the punter 
			punter = punts.roles.tmp[which(punts.roles.tmp$Role == "P"),]$GSISID
			# find his xy-coord during the punt (Event: "punt")
			x1 = ngs.tmp[which(as.integer(as.numeric(as.character(ngs.tmp$GSISID))) == punter & as.character(ngs.tmp$Event) == "punt"),]$x
			y1 = ngs.tmp[which(as.integer(as.numeric(as.character(ngs.tmp$GSISID))) == punter & as.character(ngs.tmp$Event) == "punt"),]$y
                        if (length(as.numeric(as.character(y1))) > 0 & length(as.numeric(as.character(y2))) ){ # making sure the coordinates are not "NAs" -- just double checking to avoid cases where the punts have not been correctly annotated 
				# find the angle between the straight line defined by the 0 degree line from the punter [(x1,y1),(x2,y1)] and the actual line of the punt [(x1,y1),(x2,y2)]
				A = c(as.numeric(as.character(x2)),as.numeric(as.character(y2)))
				B = c(as.numeric(as.character(x1)),as.numeric(as.character(y1)))
				C = c(as.numeric(as.character(x2)),as.numeric(as.character(y1)))
				theta = Angle(A, B,C)[[1]]
                                loc.punt.RetFC <- rbind(loc.punt.RetFC,data.frame(key=punts_data[i,]$key,yrdline=as.numeric(punts_data[i,]$V2),x1=as.numeric(as.character(x1)),y1=as.numeric(as.character(y1)),x2=as.numeric(as.character(x2)),y2=as.numeric(as.character(y2)),theta=theta))
                        }

                }

        }

}
