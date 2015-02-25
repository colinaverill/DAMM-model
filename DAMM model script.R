##coding the DAMM model###
##Davidson et al. 2012. Global Change Biology. 18: 371-384.

#Vmax parameters
#a.Sx, Ea and Km.S have been calibrated to the field data presented in Davidson et al. 2012.
R    <- 0.008314472 #universal gas constant kJ / K / mol
a.Sx <- 5.38*10^10  #pre-exponential factor of Vmax, mg C / cm3 / h
Ea   <- 72.26       #activation energy for calculating temperature sensitive Vmax kJ/mol
Km.S <- 9.95*10^-7  #Km of substrate g C / cm3
Km.O <-0.121        #Km of oxygen

#substrate and diffusion parameters
Stot <- 0.048    #total soil C concentration, %C
p    <- 0.000414 #fraction of total C pool that is soluble
S.sol<- Stot*p   #actual substrate concentration, mg C / cm3
D.liq<- 3.17     #diffusion coefficient of substrate in liquid phase
D.gas<- 1.67     #diffusion coefficient for O2
BD   <- 0.8      #bulk density g/cm3
PD   <- 2.52     #particle density g/cm3
depth<- 10       #soil depth in centimeters
  
#data
#replace these with your own raw temperature and moisture data.
temp   <- 23    #temperature
moist  <- .4    #volumentric water content of soil


#model equations- built as an R function of temperature and moisture.
DAMM.Cflux<- function(temp,moist){
Vmax = a.Sx * exp(-Ea/(R*(temp + 273.15)))
S = S.sol * D.liq * moist
a = 1 - (BD/PD) - moist
oxygen = D.gas * 0.209 * a^(4/3)
R = Vmax * (S / (Km.S + S)) * (oxygen /(Km.O + oxygen))
R.scaled<- R*depth*10000
return(R.scaled)
  }

DAMM.Cflux(23,.05)
