#### R Script: Laakso and Taagepera's
#### Effective Number of Parties
#### by Santiago Alles (PhD, Rice University)

#### Updated to: 01/05/2020
#### version 1.5
#### Available at: http://www.santiago-alles.net/


enp.FUN <- function( data, 
                     votes = "votes", seats = "seats",
                     year = "year", chamber = "chamber", district = "district",
                     path = getwd(),
                     enp_v = T, enp_s = T, 
                     save = F, target_file = '' ){
  
 
  #### Required
  #### Packages
  
  #### Checking Packages
  packs <- c("dplyr", "stringr")
  
  if(F %in% ( packs %in% installed.packages() )){
    
    packs[!packs %in% installed.packages()] -> to_install
    
    print( paste("Installing required packages:", paste(to_install, collapse = ', ')) , quote = F)
    install.packages(to_install, verbose = F, quiet = T)
    
    rm(to_install)
    
  }
  
  #### Loading Packages
  lapply(packs, require, character.only = TRUE)
  
  rm(packs)
  
  
  #### Setting
  #### Directories
  
  data.dir <- paste(path, "ENP store", sep = "/")
  plot.dir <- paste(path, "ENP graphs", sep = "/")
  
  
  #### Estimation
  #### Dataset
  
  if(!seats %in% colnames(data)){
    
    frame_names <- c(colnames(data), seats)
    data <- data.frame(data, NA) %>% tbl_df()
    colnames(data) <- frame_names
    
    rm(frame_names)
    
  }
  
  if(!votes %in% colnames(data)){
    
    frame_names <- c(colnames(data), votes)
    data <- data.frame(data, NA) %>% tbl_df()
    colnames(data) <- frame_names
    
    rm(frame_names)
    
  }
  
  data[c(year, chamber, district, votes, seats)] -> data
  c("year", "chamber", "district", "votes", "seats") -> colnames(data)
  
  if(!is.numeric(data$chamber))  data$chamber  <- as.character(data$chamber)
  if(!is.numeric(data$district)) data$district <- as.character(data$district)
  
  
  #### Estimating
  #### ENP Votes
  
  if(enp_v){
    
    data %>%
      group_by( year, chamber, district) %>%
      dplyr::summarize(total.votes = sum(votes, na.rm = T)) -> data.store
    
    data.enp <- merge( data, data.store, by = c("year", "chamber", "district")) %>% tbl_df()
    data.frame( data.enp %>% select(year, chamber, district), 
                sq.share = with(data.enp, (votes/total.votes)^2) ) %>% 
      tbl_df() -> data.enp
   
    data.enp %>%
      group_by(year, chamber, district) %>%
      dplyr::summarize(enp.votes = sum(sq.share, na.rm = T)) -> enpv.store
    
    data.frame(enpv.store[,1:3], enp.votes = 1/enpv.store$enp.votes) %>% 
      tbl_df() -> enpv.store
    
    
    rm(data.enp, data.store)
    
    if(!enp_s){
      
      enp.data <- enpv.store
      rm(enpv.store)
      
    }
  }
  
  
  #### Estimating
  #### ENP Seats
  
  if(enp_s){
    
    data %>%
      group_by( year, chamber, district) %>%
      dplyr::summarize(total.seats = sum(seats, na.rm = T)) -> data.store
    
    data.enp <- merge( data, data.store, by = c("year", "chamber", "district")) %>% tbl_df()
    data.frame( data.enp %>% select(year, chamber, district), 
                sq.share = with(data.enp, (seats/total.seats)^2) ) %>% 
      tbl_df() -> data.enp
    
    data.enp %>%
      group_by(year, chamber, district) %>%
      dplyr::summarize(enp.seats = sum(sq.share, na.rm = T)) -> enps.store
    
    data.frame(enps.store[,1:3], enp.seats = 1/enps.store$enp.seats) %>% 
      tbl_df() -> enps.store
    
    
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
    enpv.store <- enpv.store[apply(subset(enpv.store, select = enp.votes), 1, function(x) all(is.finite(x))), ]
    enps.store <- enps.store[apply(subset(enps.store, select = enp.seats), 1, function(x) all(is.finite(x))), ]
    
    ### Merging Data
    enp.data <- merge(enpv.store, enps.store, by = c("year", "chamber", "district"), all = T) %>% tbl_df()
    
    
    rm(enps.store, enpv.store)
    
  }
  
  
  ### Sorting
  ###  Data
  
  if("enp.data" %in% ls()) enp.data <- enp.data %>% arrange(year, chamber, district)
  
  
  #### Saving
  #### Dataset
  
  if('' %in% target_file ) target_file <- paste("ENP Results v", str_replace_all(Sys.Date(), "-", ""), ".csv", sep="")
  
  if(!'.csv' %in% substr(target_file, nchar(target_file)-3, nchar(target_file))) target_file <- paste(target_file, 'csv', sep = '.')
  
  
  if(save){
    
    dir.create( data.dir, showWarnings = F, recursive = T )
    write.csv( enp.data, row.names = F, na = "",
               paste(data.dir, target_file, sep = "/") )
    
    print( paste("ENP Results were stored in:", paste(data.dir, target_file, sep = "/"), sep=" "), quote = F )
    
  }
  
  
  #### Results
  #### in df  
  
  return( enp.data )
  
}