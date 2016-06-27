## 2014-11-12 
## Mira River Length Weight Equation (from Fisheries Project)

 morph <- read.csv("~/Desktop/Data/Cape Breton/Tagging/MiraRiver_morphsheet.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",", dec = ".")

#####Reworking lw relationship according to Ricker 1975 - mm & kg #### 
weight <- morph
morph$TL <- morph$TL*10

ricker <- morph[which(morph$TL != "NA"),]
ricker <- ricker[which(ricker$WT != "NA"),]


ricker$logtl <- log10(ricker$TL)
ricker$logwt <- log10(ricker$WT)


#sums and means (s, m):

stl <- sum(ricker$logtl) # 
mtl <- stl/length(ricker$logtl) # 

swt <- sum(ricker$logwt) # 
mwt <- swt/length(ricker$logwt) # 

## sums of squares and cross producst (ss, cp)
sstl <- sum(ricker$logtl^2) # 
sswt <- sum(ricker$logwt^2) # 
cp <- sum(ricker$logtl*ricker$logwt) # 

## sums of squares and cross products measured from means (ssm, cpm)

ssmtl <- (sstl - stl^2/length(ricker$logtl))

ssmwt <- (sswt - swt^2/length(ricker$logwt))

cpm <- cp - stl*swt/length(ricker$logwt)


## regressions and correlation: (p 214 Ricker 1975)

# ordinary regression of log w on log l (or)
or <- cpm/ssmtl # 

# std dev from reg line (sdevrl)
sdevrl <- ((sswt - (cp)^2/sstl)/(length(ricker$logwt)-2))^(1/2) #

# std err of ord reg ordinary reg (se)

se <- sdevrl/ssmtl^(1/2) # 

#intercept on log w axis of ordinary regression (intor)

intor <- mwt - or*mtl #

#correlation coefficient (cc)

cc <- cpm/(ssmtl*ssmwt)^(1/2) # .9915

#GM regression of log w on log l (gmr)

gmr <- or/cc #2.9286

# intercept on log w for gm (intgmr) # -7.759726
intgmr = mwt - gmr*mtl 
  

### CI based on Ricker ####

#varience 

var1 <- (ssmwt - cpm^2/ssmtl)/(ssmtl*length(ricker$TL)-2)
var2 <- (ssmwt - cpm^2/ssmtl)/(ssmtl*length(ricker$TL)-1)
var3 <- (ssmwt - cpm^2/ssmtl)/(ssmtl*1)
#f <- var1/var2
f.2 <- var3/var1

#confidence limits
#B <- f*(1-cc^2)/(length(ricker$tl)-2)
B.2 <- f.2*(1-cc^2)/(length(ricker$TL)-2)

gmr*((B.2+1)^(1/2)) # 2.953 ## use this 

gmr*(sqrt(B.2)) # 0.381 ## use this

# 2.95 ± 0.381

#gmr*((B+1)^(1/2)) # 3.4162  

#gmr*(sqrt(B)) # .03645

#### Log w = -4.83 + 2.929*log(l) R^2 = .992  ####

#### acoustics morphometrics ####

 ## Cleaning 
 morph <- morph[morph$ACOUSTIC_TAG != "n",]
 morph <- morph[morph$ACOUSTIC_TAG != "N",]
 morph <- morph[!morph$RECAP == "y",]
 morph <- morph[-(15:16),]
 morph <- morph[-(36:61),]
 morts <- c("193","194") 
 morph <- morph[!morph$UID %in% morts,]

 ## calculations
 morph$TL <- as.numeric(morph$TL)
 meanTL <- mean(morph$TL)                       #### mean 69.4 cm
 minTL <- min(morph$TL)                         #### min  31.6 cm
 maxTL <- max(morph$TL)                         #### max  125.0 cm
 sdTL <- sd(morph$TL)                           #### sd   17.0 cm

 ### number of each tags used:
    # Tag   n     Year  Battery Life (days), size range (mean ± sd)
 # V13-1L - 6 - 2012 - BATTERY: 793           61.4 - 125.0 (82.1 ± 23.1)
 #        - 5 - 2013 - BATTERY: 881           63.2 - 90.0  (72.3 ± 11.0)
 # V13P-1H - 9 - 2012- BATTERY: 162           63.6  - 85.6 (76.7 ± 8.3)
 #        - 10 - 2013                         49.7 - 82.5  (60.9 ± 12.8)
 #        -  3* - 2014 ------------------>    31.6 - 61.0  (48.8 ± 15.3) 
 # *One tag deployed at Marion Bridge 
 