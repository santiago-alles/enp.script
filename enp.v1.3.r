#### R Script: Laakso and Taagepera's
#### Effective Number of Parties
#### by Santiago Alles
#### PhD Student, Rice University

#### Updated to: 09/09/2014
#### version 1.3
#### Available at: http://www.santiago-alles.net/


enp.function<-function(data, votes = T, seats = T, save = F, plot = F){
  
  #### Checking
  #### Packages
  
  if("ggplot2" %in% rownames(installed.packages())==FALSE){
    return(print("Install 'ggplot2' package before running."))
  } else {
    if("doBy" %in% rownames(installed.packages())==FALSE){
      return(print("Install 'doBy' package before running."))
      
    } else {
      
      #### Loading
      #### Packages
      require(doBy)
      
      #### Setting
      #### Directories
      
      directory<-getwd()
      data.dir<-paste(directory, "/ENP store", sep="")
      plot.dir<-paste(directory, "/ENP graphs", sep="")
      
      #### Estimating
      #### ENP Votes
      
      if(votes==T){
        
        data.store<-summaryBy(votes ~ year  + chamber + district, 
                              FUN=sum, na.rm=T, data=data)
        colnames(data.store)<-c("year", "chamber", "district", "total.votes")
        
        data.enp<-merge(data, data.store,
                        by.x=c("year", "chamber", "district"),
                        by.y=c("year", "chamber", "district"))
        data.enp<-cbind(subset(data.enp, select=c(year, chamber, district)),
                        (data.enp[,"votes"]/data.enp[,"total.votes"])^2)
        colnames(data.enp)<-c("year", "chamber", "district", "sq.share")
        
        enpv.store<-summaryBy(sq.share ~ year + chamber + district, 
                              FUN=sum, na.rm=T, data=data.enp)
        enpv.store<-cbind(enpv.store[,1:3], 1/enpv.store[,4])
        colnames(enpv.store)<-c("year", "chamber", "district", "enp.votes")
        
        enpv.store<-enpv.store[order(enpv.store$chamber, enpv.store$year, enpv.store$district),]
        
        rm(data.enp, data.store)
        
        if(seats==F){
          
          enp.data<-enpv.store
          
          if(save==T){
            
            store.fun<-function(data, directory){
              
              dir.create(directory, showWarnings=F)
              setwd(directory)
              return(write.csv(data, paste("ENP Votes. ", Sys.Date(), ".csv", sep=""), row.names=F, na=""))
              
            }
            
            store.fun(enp.data, data.dir)
            print(paste("ENP Votes results were stored in:", data.dir, sep=" "))
            
          }
          
          rm(enpv.store)
          
        }
        
      }
      
      #### Estimating
      #### ENP Seats
      
      if(seats==T){
        
        data.store<-summaryBy(seats ~ year + chamber + district, 
                              FUN=sum, na.rm=T, data=data)
        colnames(data.store)<-c("year", "chamber", "district", "total.seats")
        
        data.enp<-merge(data, data.store,
                        by.x=c("year", "chamber", "district"),
                        by.y=c("year", "chamber", "district"))
        data.enp<-cbind(subset(data.enp, select=c(year, chamber, district)),
                        (data.enp[,"seats"]/data.enp[,"total.seats"])^2)
        colnames(data.enp)<-c("year", "chamber", "district", "sq.share")
        
        enps.store<-summaryBy(sq.share ~ year + chamber + district, 
                              FUN=sum, na.rm=T, data=data.enp)
        enps.store<-cbind(enps.store[,1:3], 1/enps.store[,4])
        colnames(enps.store)<-c("year", "chamber", "district", "enp.seats")
        
        enps.store<-enps.store[order(enps.store$chamber, enps.store$year, enps.store$district),]
        
        rm(data.enp, data.store)
        
        if(votes==F){
          
          enp.data<-enps.store
          
          if(save==T){
            
            store.fun<-function(data, directory){
              
              dir.create(directory, showWarnings=F)
              setwd(directory)
              return(write.csv(data, paste("ENP Seats. ", Sys.Date(), ".csv", sep=""), row.names=F, na=""))
              
            }
            
            store.fun(enp.data, data.dir)
            print(paste("ENP Seats results were stored in:", data.dir, sep=" "))
            
          }
          
          rm(enps.store)
          
        }
      }
      
      #### Building
      #### Dataset
      
      if(seats==T & votes==T){
        
        idx<-apply(subset(enpv.store, select=enp.votes), 1, function(x) all(is.finite(x)))
        enpv.store<-enpv.store[idx,]
        
        idx<-apply(subset(enps.store, select=enp.seats), 1, function(x) all(is.finite(x)))
        enps.store<-enps.store[idx,]
        
        enp.data<-merge(enpv.store, enps.store,
                        by.x=c("year", "chamber", "district"),
                        by.y=c("year", "chamber", "district"),
                        all.x=T, all.y=T)
        enp.data<-enp.data[order(enp.data$chamber, enp.data$year, enp.data$district),]
        
        rm(enps.store, enpv.store, idx)
        
        if(save==T){
          
          store.fun<-function(data, directory){
            
            dir.create(directory, showWarnings=F)
            setwd(directory)
            return(write.csv(data, paste("ENP Results. ", Sys.Date(), ".csv", sep=""), row.names=F, na=""))
            
          }
          
          store.fun(enp.data, data.dir)
          print(paste("ENP Results were stored in:", data.dir, sep=" "))
          
        }    
      }
      
      #### Building
      #### ENP Plots
      
      if(plot==T){
        if(votes==T | seats==T){
          
          #### Loading
          #### Packages
          require(ggplot2)
          
          #### Plot
          #### Data
          plot.data<-enp.data
          
          #### Plot
          #### FUN
          
          plot.fun<-function(plot.data, type){
            
            if(type=="votes"){
              loop.data<-subset(plot.data, year==unique(plot.data$year)[i] & chamber==unique(plot.data$chamber)[j],
                                select=c(district, enp.votes))
              plot.title<-c("Electoral Effective Number of Parties, by Electoral District")
              y.axis<-c(0,round(max(plot.data$enp.votes),0)+1)
              filename<-paste(unique(plot.data$chamber)[j], ".", unique(plot.data$year)[i], ".ENPv", ".png", sep="")
            } else {
              loop.data<-subset(plot.data, year==unique(plot.data$year)[i] & chamber==unique(plot.data$chamber)[j],
                                select=c(district, enp.seats))
              plot.title<-c("Legislative Effective Number of Parties, by Electoral District")
              y.axis<-c(0,round(max(plot.data$enp.seats),0)+1)
              filename<-paste(unique(plot.data$chamber)[j], ".", unique(plot.data$year)[i], ".ENPs", ".png", sep="")
            }
            
            loop.data<-loop.data[order(loop.data$district),]
            loop.data<-cbind(seq(1,nrow(loop.data)), loop.data)
            colnames(loop.data)<-c("rank", "district", "enp")
            
            plot.subtitle<-paste(unique(plot.data$chamber)[j], ", ", unique(plot.data$year)[i], sep="")
            
            y.breaks<-c(seq(from=0, to=max(y.axis), by=1))
            
            enp.plot<-ggplot(loop.data, aes(x=rank, y=enp))
            enp.plot<-enp.plot + geom_bar(stat="identity")
            
            enp.plot<-enp.plot + scale_x_discrete(limits=c(seq(from=1, to=length(unique(loop.data$district)), by=1)), 
                                                  labels=loop.data$district)
            enp.plot<-enp.plot + scale_y_continuous(limits=y.axis, breaks=y.breaks)
            
            enp.plot<-enp.plot + labs(y="ENP\n", x="Electoral District")
            enp.plot<-enp.plot + ggtitle(bquote(atop(bold(.(plot.title)), atop(.(plot.subtitle), ""))))
            
            enp.plot<-enp.plot + theme(text=element_text(size=13.5),
                                       axis.text.x=element_text(colour="black"),
                                       axis.text.y=element_text(colour="black")) 
            
            dir.create(plot.dir, showWarnings=F)
            setwd(plot.dir)
            
            return(ggsave(filename, enp.plot, dpi=800, width=12, height=7.5))
            
          }
          
          
          #### ENP Votes
          #### Grahps
          
          if(votes==T){
            
            for(i in 1:length(unique(plot.data$year))){
              for(j in 1:length(unique(plot.data$chamber))){
                
                plot.fun(plot.data, "votes")
                
              }
            }
            
            setwd(directory)
            
            rm(i, j)
            
          }
          
          
          if(seats==T){
            
            for(i in 1:length(unique(plot.data$year))){
              for(j in 1:length(unique(plot.data$chamber))){
                
                plot.fun(plot.data, "seats")
                
              }
            }
            
            setwd(directory)
            
            rm(i, j)
            
          }
          
          print(paste("ENP Plots were stored in:", plot.dir, sep=" "))
          
        }
      }
      
      
      #### Undefined
      #### Arguments
      
      if(votes==F & seats==F){
        
        enp.data<-c("Hey man, what are you trying to do? Both types of the ENP cannot be FALSE, at least one argument must be TRUE")
      }
      
      setwd(directory)
      return(enp.data)
      
    }
  }
  
  return(print())
  
}

