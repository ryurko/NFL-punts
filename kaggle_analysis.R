library(dplyr)
library(qdapRegex)
library(stringr)
library(LearnGeom)
library(latex2exp)

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


#ggplot(aggr.data,aes(Type,Rate))+geom_col(color="black")+geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"),legend.title=blue.bold.16)+ylab("Concussion Rate (%)")

ggplot(aggr.data,aes(Type,Rate,fill=Rate))+geom_col(color="black")+geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=.1)+scale_fill_gradient(low="grey",high="red")+labs(x="Punt Type",y="Concussion Rate (%)",title="Concussion Rates (%) for Different Types of Punts",caption="Pelechrinis, Yurko, Ventura (2019)")+theme_bw(base_size=17)

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


#### Find what is the distribution of the punt return yardage (including fair catches, out-of-bounds etc.) -- we basically want to see currently what is the distribution of the yardage gained by returning a punt. While the location of the line of scrimmage is important here (e.g., punting to a short field gives lower chances of getting a good return), we just care for the "average" case.  in 1:dim(punts_data)[1]){This can be a support for the 5-yards rule we propose
### NOTE: We do not account for any penalty that can push the line of scrimmage of the upcoming drive back

return_yardage <- c()

for (i in 1:dim(punts_data)[1]){

	if (i %!in% ind){
		if (str_detect(punts_data[i,]$PlayDescription,"for no gain")){
			return_yardage <- append(return_yardage,0)
		}else{
			if (str_detect(punts_data[i,]$PlayDescription,"for \\d+ yards")){
				y = as.numeric(rm_between(punts_data[i,]$PlayDescription,"\\sfor\\s ","\\syards",extract=TRUE)[[1]][1])
				#y = as.numeric(sub(".*for *(.*?) *yards.*", "\\1", punts_data[i,]$PlayDescription))
				return_yardage <- append(return_yardage,y)
			}
		}
	}

}

# add the fair catches
return_yardage_all <- c(return_yardage,rep(0,length(ind)))

return_yardage_all.df = data.frame(yrds = return_yardage_all)

ggplot(return_yardage_all.df, aes(x=yrds)) + geom_histogram(color="black", fill="white",binwidth=2) + geom_vline(aes(xintercept=mean(yrds,na.rm=T)),color="blue", linetype="dashed", size=1) + theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"))+ ylab("# of punts") + xlab("Return yardage")

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


### we want to get an estimate of the number of injuries that will be prevented by this rule
## we will perform simulations based on assumptions for the percentage of punts to not be returned and for the closeness to the sideline of the kicks that will be returned 
## this will provide us with a range of possibilities and then we can obtain a fairly realistic view of what to expect
## Currently ~47% of the punts are returned

reduction_rate = c(0.05,0.1,0.15,0.2) # this is an assumed reduction rate for the returned punts. Realistically we should not expect more than 20% reduction
yards_closer = c(1,2.5,5,7.5,10) # this is the assumed expected shift of the punt towards the sideline. Assume a normal distribution with variance 7 yards (this is the variance of the sideline distance in the punts observed) 

punts_returned = dim(punts_data)[1]-(fc_punts+oob_punts+tback_punts+downed_punts)
punts_notreturned = fc_punts+oob_punts+tback_punts+downed_punts

injuries_simulations = data.frame(red_rate = c(),yrds_closer = c(), xCR = c())

for (r in reduction_rate){

	for (y in yards_closer){
			
		# for each pair of these variables we will perform bootstrap simulations
		# we will bootstrap the distribution of the distances from the sidelines for the punts that are still returned
		# we will assume 1,000 punts and we will identify the number of concussions expected "per 1,000 exposures"
		# under no rule change we expect 47% of the punts to be returned -- this is the baseline rate (punts_returned/(punts_returned+punts+_notreturned))
		# first we will identify how many punts are going to be returned from the 1,000 
		xPR = 470 - round(r*470)
		xNPR = 530 + round(r*470)
		# first we have to deal with the expected concussions from non-returned punts
		# we will assume that the xNPR punts includes all the different types (i.e., out-of-bounds, touchback etc.) at the same proportion as the original sample 
		# we will use the concussion rate for each of these types of non-returned punts to estimate the expected number of concussions
		# downed
		xInjDowned = (downed_punts/(downed_punts+fc_punts+tback_punts+oob_punts))*xNPR*0.01*bar.data[4,]$Rate # the rates are already expressed in % so we need to multiply with 0.01
		# out-of-bounds
		xInjOob = (oob_punts/(downed_punts+fc_punts+tback_punts+oob_punts))*xNPR*0.01*bar.data[2,]$Rate
		# touchbacks
		xInjTback = (tback_punts/(downed_punts+fc_punts+tback_punts+oob_punts))*xNPR*0.01*bar.data[3,]$Rate
		# fair catch 
		xInkFC = (fc_punts/(downed_punts+fc_punts+tback_punts+oob_punts))*xNPR*0.01*bar.data[1,]$Rate
		# bootstrap the returned punts 
		boot_samples = rep(0,500)
		for (b in 1:500){
			returned_dis_sim = sample(d.side$d,xPR,replace=T)-rnorm(xPR,y,7)
			returned_dis_sim[which(returned_dis_sim<0)] = 1
			returned_dis_sim[which(returned_dis_sim>26)] = 26
			xInjRet = sum(predict(dsidinj.mod,newdata=data.frame(d=returned_dis_sim),type="response"))
			boot_samples[b] = xInjRet
		}
		xInjRet = mean(boot_samples)
		injuries_simulations <- rbind(injuries_simulations,data.frame(red_rate = r, yrds_closer= y,xCR = xInjDowned+xInjOob+xInjTback+xInkFC+xInjRet))
		

	}

} 

current_concussion_rate_per1Kexposures = (dim(injuries)[1]/dim(punts_data)[1])*1000

injuries_simulations$yrds_closer = as.factor(injuries_simulations$yrds_closer)

ggplot(injuries_simulations, aes(x = red_rate, y = xCR,color=yrds_closer))+geom_line() + geom_hline(yintercept=current_concussion_rate_per1Kexposures)+theme(axis.text.x = blue.bold.16,axis.text.y = blue.bold.16,axis.title=element_text(size=16,face="bold"),legend.title=blue.bold.16)+ylab("# of Concussions per 1,000 Exposures (Punts)")+xlab(TeX("r"))+scale_color_manual(TeX("$s_d$"),values=c("blue","brown","red","orange","purple"))
