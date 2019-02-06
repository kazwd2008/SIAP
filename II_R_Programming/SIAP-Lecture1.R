  #If statement switch
  made.IDList <- FALSE
  made.sampleList <- FALSE
  
  #Setting conditions===============================================================================
  Sector.population <- c(1000, 2000, 1500, 0, 9000, 8000, 300, 40, 8000, 9500)
  Stage1.samplesize <- 100
  Stage2.samplerate <- 0.1
  #=================================================================================================
  
  number_of_Sector <- length(Sector.population)
  
  #Generating dummy population data
  #sector   ID
  #==============
  #     1    1
  #     1    2
  #   ...  ...
  #     2    1
  #   ...  ...
  
  for (sec in 1:number_of_Sector){
    
    elements <- c(rep(sec, times=Sector.population[sec]), 1:Sector.population[sec])
    sector.IDList <- matrix(elements, nr=Sector.population[sec], nc=2)
    if(made.IDList==FALSE){
      
      IDList <- sector.IDList
      made.IDList <- TRUE
      
    }else{
      
      IDList <- rbind(IDList, sector.IDList)    # Population
      
    }
    
  }
  
  IDList <- as.data.frame(IDList)
  colnames(IDList) <- c("sector", "ID")
  table(IDList$sector)
  #IDList is dummy population ID data
  
  #==================================================================================================
  
  #Sample Selection

  for (sec in 1:number_of_Sector){
    
    #Sample Selection in each sector
    sector.popID <-  IDList$ID[which(IDList$sector==sec)]   # Population by sector

    #Stage1
    if (length(sector.popID)>0){#Skipping sampling in the sector which has no members
      
      if(length(sector.popID)<=Stage1.samplesize){ 
        
        sector.sample <- sector.popID 
        # select all population in the sector as samples = Stage2 skip

      }else{
        
        sector.sample1 <- sample(sector.popID, Stage1.samplesize, replace=F)
        sector.remaining.popID <- setdiff(sector.popID, sector.sample1)  # unsampled population
        
        #Stage2
        Stage2.samplesize <- ceiling(length(sector.remaining.popID)*Stage2.samplerate)
        print(Stage2.samplesize)
        sector.sample2 <- sample(sector.remaining.popID, Stage2.samplesize, replace=F)
        sector.sample <- union(sector.sample1, sector.sample2) 
        
      }
      
      sector.sampleList <- IDList[which(IDList$sector==sec & IDList$ID %in% sector.sample), ]
      
      #Binding the sample list of all sector
      if(made.sampleList == FALSE){
        
        sampleList <- sector.sampleList
        made.sampleList <- TRUE
        
      }else{
        
        sampleList <- rbind(sampleList, sector.sampleList)
        
      }
      
    }
    
  }
  
  table(sampleList$sector)
  sampleList <- unique(sampleList) #duplication check
  table(sampleList$sector)
