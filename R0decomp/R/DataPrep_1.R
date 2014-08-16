

library(DemogBerkeley)
library(data.table)
library(parallel)
username  <- userInput()
password  <- userInput()
CountriesHMD <- getHMDcountries()
CountriesHFD <- getHFDcountries()

(Countries <- intersect(CountriesHMD, CountriesHFD))

# all countries with temporal overlap

Data <- do.call(rbind,mclapply(Countries, function(XYZ, .username, .password){
     births     <- readHFDweb(CNTRY = XYZ, item = "birthsRR", username = .username, password = .password)
     fltper     <- readHMDweb(CNTRY = XYZ, item = "fltper_1x1", username = .username, password = .password)
     Expos      <- readHMDweb(CNTRY = XYZ, item = "Exposures_1x1", username = .username, password = .password)
     BirthsSex  <- readHMDweb(CNTRY = XYZ, item = "Births", username = .username, password = .password)
     
     # fallbacks, don't kill the loop
     if (class(births) == "try-error"){
         message(XYZ, "error in readHFDweb()")
         return(NULL)
     }
     if (class(fltper) == "try-error"){
         message(XYZ, "error in readHMDweb()")
         return(NULL)
     }
     if (class(Expos) == "try-error"){
         message(XYZ, "error in readHMDweb()")
         return(NULL)
     }
     if (class(BirthsSex) == "try-error"){
         message(XYZ, "error in readHMDweb()")
         return(NULL)
     }
     
     BirthsSex$PF <- BirthsSex$Female / BirthsSex$Total
     BirthsSex    <- BirthsSex[,c("Year","PF")]
     
     births$OpenInterval <- NULL # remove open fert
     fltper$OpenInterval <- NULL # remove open mort
     Expos$OpenInterval  <- NULL # remove open mort
     Expos               <- Expos[,c("Year","Age","Female")]
     
     years <- sort(unique(c(unique(births$Year),
                        unique(fltper$Year),
                        unique(Expos$Year))))
    
     births$Total[is.na(births$Total)] <- 0
     
     .births    <- data.table(births)
     .fltper    <- data.table(fltper)
     .Expos     <- data.table(Expos)
     .BirthsSex <- data.table(BirthsSex)
     
     setnames(.births, "Total", "Births")
     setnames(.Expos, "Female", "Exposure")
     
     # index for merging
     setkey(.births, Year, Age)
     setkey(.fltper, Year, Age)
     setkey(.Expos, Year, Age)
     setkey(.BirthsSex, Year)
     
     Dat        <- merge(.fltper, .births, all.x = TRUE)
     Dat        <- merge(Dat, .Expos, all.x = TRUE)
     Dat        <- merge(Dat, .BirthsSex, by = "Year")
    
     Dat$Fx     <- Dat$Births / Dat$Exposure
     Dat$Fxf    <- Dat$Fx * Dat$PF
     Dat$Code   <- XYZ
  
     Dat        <- Dat[Dat$Year %in% years, ]
     
     Dat
        },.username = username, .password = password))

# pre-men / post-men ages are NA, make 0
Data$Fx[is.na(Data$Fx)]   <- 0
Data$Fxf[is.na(Data$Fxf)] <- 0

save(Data, file = "/home/triffe/workspace/R0decomp/Data/HMDHFD.Rdata")


