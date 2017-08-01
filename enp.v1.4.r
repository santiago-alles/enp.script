#### R Script: Laakso and Taagepera's
#### Effective Number of Parties
#### by Santiago Alles (PhD, Rice University)
#### Universidad del Rosario

#### Updated to: 08/01/2017
#### version 1.4
#### Available at: http://www.santiago-alles.net/


enp.FUN <- function( data, 
                     votes = "votes", seats = "seats",
                     year = "year", chamber = "chamber", district = "district",
                     path = getwd(),
                     enp_v = T, enp_s = T, 
                     save = F ){
  
  
  #### Required
  #### Packages
  
  #### Checking Packages
  packs <- c("doBy", "stringr")
  
  if(F %in% ( packs %in% installed.packages() )){
    
    packs[!packs %in% installed.packages()] -> to_install
    
    print(paste("Installing required packages:", to_install), quote = F)
    install.packages(to_install, verbose = F, quiet = T)
    
    rm(to_install)
    
  }
  
  #### Loading Packages
  lapply(packs, require, character.only = TRUE)
  
  rm(packs)
  
  
  #### Setting
  #### Directories
  
  data.dir <- paste(path, "ENP store", sep="/")
  plot.dir <- paste(path, "ENP graphs", sep="/")
  
  
  #### Estimation
  #### Dataset
  
  data[c(year, chamber, district, votes, seats)] -> data
  c("year", "chamber", "district", "votes", "seats") -> colnames(data)


  #### Estimating
  #### ENP Votes
  
  if(enp_v){
    
    data.store <- summaryBy( votes ~ year  + chamber + district, 
                             FUN=sum, na.rm=T, data=data )
    colnames(data.store) <- c("year", "chamber", "district", "total.votes")
    
    data.enp <- merge( data, data.store, by = c("year", "chamber", "district"))
    data.enp <- cbind( subset(data.enp, select=c(year, chamber, district)),
                       (data.enp[,"votes"]/data.enp[,"total.votes"])^2 )
    colnames(data.enp) <- c("year", "chamber", "district", "sq.share")
    
    enpv.store <- summaryBy( sq.share ~ year + chamber + district, 
                             FUN=sum, na.rm=T, data=data.enp )
    enpv.store <- cbind( enpv.store[,1:3], 1/enpv.store[,4] )
    colnames(enpv.store) <- c("year", "chamber", "district", "enp.votes")

    rm(data.enp, data.store)
    
    if(!enp_s){
      enp.data <- enpv.store
      rm(enpv.store)
      
    }
  }
  
  #### Estimating
  #### ENP Seats
  
  if(enp_s){
    
    data.store <- summaryBy( seats ~ year + chamber + district, 
                             FUN=sum, na.rm=T, data=data )
    colnames(data.store) <- c("year", "chamber", "district", "total.seats")
    
    data.enp <- merge( data, data.store, by = c("year", "chamber", "district"))
    data.enp <- cbind( subset(data.enp, select=c(year, chamber, district)),
                       (data.enp[,"seats"]/data.enp[,"total.seats"])^2 )
    colnames(data.enp) <- c("year", "chamber", "district", "sq.share")
    
    enps.store <- summaryBy( sq.share ~ year + chamber + district, 
                             FUN=sum, na.rm=T, data=data.enp )
    enps.store <- cbind(enps.store[,1:3], 1/enps.store[,4])
    colnames(enps.store) <- c("year", "chamber", "district", "enp.seats")

    rm(data.enp, data.store)
    
    if(!enp_v){
      enp.data <- enps.store
      rm(enps.store)
      
    }
  }
  
  
  #### Building
  #### Dataset  
    
  if(enp_v & enp_s){
    
    ### Cleaning Data
    enpv.store <- enpv.store[apply(subset(enpv.store, select=enp.votes), 1, function(x) all(is.finite(x))), ]
    enps.store <- enps.store[apply(subset(enps.store, select=enp.seats), 1, function(x) all(is.finite(x))), ]
    
    ### Merging Data
    enp.data <- merge(enpv.store, enps.store, by = c("year", "chamber", "district"), all = T)
    
    
    rm(enps.store, enpv.store)
    
  }
  
  ### Sorting Data
  if("enp.data" %in% ls()) orderBy( ~ year - chamber - district, data = enp.data) -> enp.data
  
  
  #### Saving
  #### Dataset  
  
  if(save){
    dir.create( data.dir, showWarnings=F, recursive = T )
    write.csv( enp.data, row.names=F, na="",
               paste(data.dir, paste("ENP Results v", str_replace_all(Sys.Date(), "-", ""), 
                                     ".csv", sep=""), sep="/") )
    print( paste("ENP Results were stored in:", data.dir, sep=" "), quote = F )
    
  }
  
  
  #### Results
  #### in df  
  
  return( enp.data )
  
}
