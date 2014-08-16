library(compiler)
library(reshape2)
library(data.table)
# decompose by SRB, mx, and Fx 
Data <- local(get(load("/home/triffe/workspace/R0decomp/Data/HMDHFD.Rdata")))

# strange, need to remove rows with no fertility data
Data  <- data.table(Data)
Data[ , TFR := sum(Fx), by = list(Code, Year)]
Data  <- Data[Data$TFR > 0, ]

#hist(Data$PF)
#abline(v=.4886,col="red")
#abline(v=mean(Data$PF),col="blue")
#(1-.4886) / .4886
AKm02q0 <- cmpfun(function(m0,constant,slope){
            -1 / slope / m0 * (-m0 +  (m0 * constant) - 0.1e1 + sqrt(((m0 ^ 2) - 2 * constant * (m0 ^ 2) + 2 * m0 + (constant ^ 2) * (m0 ^ 2) - 2 *  (m0 * constant) + 1 - 4 * slope * (m0 ^ 2)))) / 2
        })
AKm02a0 <- cmpfun(function(m0,sex="m"){
            sex <- rep(sex,length(m0))
            ifelse(sex == "m",
                    ifelse(m0 < 0.02306737, 0.1493 - 2.0367 * AKm02q0(m0, 0.1493, -2.0367),
                            ifelse(m0 < 0.0830706, 0.0244 + 3.4994 * AKm02q0(m0, 0.0244, 3.4994), .2991)),
                    ifelse(m0 < 0.01725977, 0.1490 - 2.0867 * AKm02q0(m0, 0.1490, -2.0867),
                            ifelse(m0 < 0.06919348, 0.0438 + 4.1075 * AKm02q0(m0, 0.0438, 4.1075), 0.3141))
            )
        })

mx2Lx <- cmpfun(function(mx, sex = "f"){
            mx        <- as.matrix(mx)
            N         <- nrow(mx)
            ax        <- mx * 0 + .5
            ax[1,]    <- AKm02a0(mx[1, ], sex)
            qx        <- mx / (1 + (1 - ax) * mx)
            
            px        <- 1 - qx
            lx        <- apply(px, 2, function(px., N ){      
                                   c(1, cumprod(px.[1:(N - 1)]))
                               }, N = N)
            
            dx        <- lx * qx
            Lx        <- lx - (1 - ax) * dx 
            Lx[N,]    <- lx[N, ] * mx[N, ]/1
            rownames(Lx) <- 1:N - 1
            Lx
        })



# assumes everything in conformable matrices...
getR0M <- function(mx, Fx, PF = .4886){
    mx <- as.matrix(mx)
    Fx <- as.matrix(Fx)
    PF <- as.matrix(PF)
    colSums(mx2Lx(mx) * Fx * PF)
}

getR0DT <- function(SD){
    mx <- acast(SD, Age~Year, value.var = "mx")
    Fx <- acast(SD, Age~Year, value.var = "Fx")
    PF <- acast(SD, Age~Year, value.var = "PF")
    R0 <- colSums(mx2Lx(mx) * Fx * PF)
    data.table(Year = as.integer(names(R0)),R0 = R0)
}


R0s <- Data[, getR0DT(.SD), by = list(Code)]

head(R0s)

R0M <- acast(R0s, Year ~ Code, value.var = "R0")

matplot(R0M, type = 'l', lty = 1, col = "#00000050")


sum(!is.na(R0M)) # number of cases.

# Maybe bring in more lifetables and










