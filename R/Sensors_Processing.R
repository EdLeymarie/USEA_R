#**************************************************

# Data Processing

#**************************************************

#**************************************************
# Compute do
#**************************************************

# Inputs : C1phase,C2phase,temp, Pres : data from the Optode. tempCTD,salCTD, PRESCTD : Data from the CTD

Process_DO_Bittig <- function(C1phase,C2phase,temp, Pres,tempCTD,salCTD, PRESCTD,COEF=NULL) {
  
  #tempCTD <- approx(PRESCTD, tempCTD, Pres, rule=2)$y #Approx tempCTD on DO pressure
  tempCTD <- temp #We use the temperature from the Optode
  
  #Approx tempCTD on DO pressure
  salCTD <- approx(PRESCTD,salCTD, Pres, rule=2,ties = "mean")$y 
  
  
  
  #COEF if NULL (for plotting)
  if (is.null(COEF)){
    COEF <- c(5.6725661e-03,8.2915275e-05,1.0033795e-06,6.2236942e-02,-9.3470722e-05,-1.4554620e-02,1.2110645e-03) # From Henry
  }
  
  
  TCPhase <-  (C1phase - C2phase) 
  
  KSV <-  COEF[1] + (COEF[2]  * temp) + (COEF[3] * temp * temp)
  
  #print(KSV[20:100])
  
  P0 <- COEF[4]  + COEF[5] * temp
  
  #print(P0[20:100])
  
  PC <- COEF[6]  + COEF[7] * TCPhase
  
  #print(PC[20:100])
  pO2 <- ((P0/PC)-1) / KSV
  
  
  rhumid=1
  atm_press=1.01325
  
  pH2O = rhumid * (exp(24.4543-(67.4509*(100/(tempCTD+273.15)))-(4.8489*log(((273.15+tempCTD)/100)))-0.000544*salCTD))
  
  th0=1-(0.999025+0.00001426*tempCTD-0.00000006436*tempCTD^2)
  sca_T=log((298.15-tempCTD)/(273.15+tempCTD))
  
  
  oxy_sol=((exp(2.00856+3.224*sca_T+3.99063*sca_T^2+4.80299*sca_T^3+0.978188*sca_T^4+1.71069*sca_T^5+salCTD*(-0.00624097-0.00693498*sca_T-0.00690358*sca_T^2-0.00429155*sca_T^3)-0.00000031168*salCTD^2))/0.022391903)
  oxy_sol1 <- oxy_sol 
  
  #oxy_sol=oxy_sol*P_atm*(((1-pH2O/P_atm)*(1-th0*P_atm))/((1-pH2O)*(1-th0)));
  # pressure corrected O2 solubility / umol atm / l
  oxy_sol  = oxy_sol *  atm_press*(((1-pH2O/atm_press)*(1-th0*atm_press))/((1-pH2O)*(1-th0)))
  oxy_sol2 <- oxy_sol 
  
  # oxygen concentration in umol/l (salinity corrected)
  O2conc_sal=(pO2*oxy_sol)/((atm_press-pH2O)*0.20946*1013.25)
  O2conc_sal3 <- O2conc_sal 
  
  # correcion de pression
  O2conc_sal = O2conc_sal * (1+ ((3.2)/100 *(Pres/1000)))
  
  return(O2conc_sal)
  
}



#**************************************************
# Compute pH
#**************************************************

# Inputs : 

Process_pH_SBE<-function(data,k0=-1.392151,k2=-1.0798E-03,coefsp=c(2.5064E-05,-4.4107E-08,4.7311E-11,-2.8822E-14,9.2132E-18,-1.1965E-21)){
  phtot=NULL
  
  indCTD<-data$SensorType==0
  indpH<-data$SensorType==22
  
  if ((length(data$`pH [mV]`[indpH])>10) & (length(data$`Temperature [deg. C.]`[indCTD])>10)){
    #il y a assez de data
    
    #interpolation des donnees CTD
    t<-approxfun(data$`Pressure [dbar]`[indCTD],data$`Temperature [deg. C.]`[indCTD],rule = 2)
    S<-approxfun(data$`Pressure [dbar]`[indCTD],data$`Salinity [PSU]`[indCTD],rule = 2)
    
    #calcul
    Press<-data$`Pressure [dbar]`[indpH]
    Vrs<-data$`pH [mV]`[indpH]
    Temp<-t(Press)
    Tk<-273.15+Temp #degrees Kelvin
    Salt<-S(Press)
    
    #P<-10*p #passage en bar
    
    # ************************************************************************
    #  SET SOME CONSTANTS
    # ************************************************************************
    #Universal gas constant, (R) , http://physics.nist.gov/cgi-bin/cuu/Value?r
    R    = 8.31446 # J/(mol K) 
    Fa    = 96485  #Faraday constant Coulomb / mol
    
    ln10 = log(10) #natural log of 10
    
    # ************************************************************************
    #  CALCULATE PHYSICAL AND THERMODYNAMIC DATA
    # Dickson, A. G., Sabine, C. L., & Christian, J. R. (2007). Guide to best
    # practices for ocean CO2 measurements.
    # ************************************************************************
    
    # IONIC STRENGTH OF SEAWATER (mol / kg H2O)
    # Varified units by comparing to Dickson et al. 2007: Chap 5, p10 Table 2
    # Dickson et al. 2007: Chap 5, p13 Eq 34
    IonS = 19.924 * Salt / (1000 - 1.005 * Salt)
    
    # MEAN SEAWATER SULFATE CONCENTRATION (mol / kg solution)
    # This wants to be mol/kg sw  as KHSO4 is on that scale
    # Dickson et al. 2007: Chap 5, p10 Table 2
    Stotal = (0.14 / 96.062) * (Salt / 1.80655)
    
    # MEAN SEAWATER CHLORIDE CONCENTRATION  (mol / kg H20)
    # this wants to be mol/kg H2O as activity is on mol/kg H2O scale
    # Dickson et al. 2007: Chap 5, p10 Table 2
    Cltotal = 0.99889 / 35.453 * Salt / 1.80655 # %(mol / kg solution)
    Cltotal = Cltotal /(1 - 0.001005 * Salt) #  % (mol / kg H20)
    
    # BISULFIDE DISSCIATION CONSTANT AT T,S AND IONIC STRENGTH(mol/kg solution)
    # Dickson et al. 2007: Chap 5, p12 Eq 33
    Khso4 = exp(-4276.1 / Tk + 141.328 - 23.093 * log(Tk) + 
                  (-13856 / Tk + 324.57 - 47.986 * log(Tk)) * IonS^ 0.5 + 
                  (35474 / Tk - 771.54 + 114.723 * log(Tk)) * IonS - 
                  2698 / Tk * IonS^ 1.5 + 1776 / Tk * IonS ^ 2 + 
                  log(1 - 0.001005 * Salt))
    
    # Millero 1983 Chemical Oceanography vol 8
    # partial molar volume and compressibility of HSO4 in seawater. 
    deltaVHSO4 = -18.03 + 0.0466 * Temp + 0.000316 * Temp^ 2
    KappaHSO4 = (-4.53 + 0.09 * Temp) / 1000
    
    #  Press changed from dbar to bar here by / 10
    lnKhso4fac = (-deltaVHSO4 + 0.5 * KappaHSO4 * (Press / 10)) * (Press / 10) / (R * 10 * Tk)
    
    # bisulfate association constant at T, S, P
    Khso4TPS = Khso4 * exp(lnKhso4fac)
    
    # GAMMA +/- HCl, activity coefficient of HCl at T/S, P=1
    # ADH is the Debye Huckel constant, calcualted as a polynomial 
    # fit to data in Khoo et al. 1977, doi:10.1021/ac50009a016
    # See Martz et al. 2010, DOI 10.4319/lom.2010.8.172, p175
    # Typo in paper 2nd term should be e-4 not e-6
    
    ADH = (3.4286e-6 * Temp^ 2 + 6.7524e-4 * Temp + 0.49172143)
    
    log10gammaHCl = -ADH * sqrt(IonS) / (1 + 1.394 * sqrt(IonS)) +  (0.08885 - 0.000111 * Temp) * IonS;
    # Millero 1983 partial molar volume of HCl in seawater
    deltaVHCl = 17.85 + 0.1044 * Temp - 0.001316 * Temp^ 2
    
    # effect of pressure on activity coefficient of HCl, divide by 2 because
    # its a mean activity coefficient, divide by 10 for units in the cm3 to F
    # conversion.
    
    log10gammaHCLtP = log10gammaHCl + deltaVHCl*(Press/10)/(R*Tk*ln10)/2/10
    
    #  Sensor reference potential
    
    # ************************************************************************
    k0T = k0 + k2 * Temp # % Temp  in deg C
    
    # CALCULATE PRESSURE CORRECTION (POLYNOMIAL FUNCTION OF PRESSURE)
    # ALL SENSORS HAVE A PRESSURE RESPONSE WHICH IS DETERMINED IN THE LAB
    # AND CONTAINED IN THE POLYNOMIAL Pcoefs
    f<-function(p){coefsp[1]*p*1+coefsp[2]*(p*1)^2+coefsp[3]*(p*1)^3+coefsp[4]*(p*1)^4+coefsp[5]*(p*1)^5+coefsp[6]*(p*1)^6}
    pcorr = f(Press)
    k0TP  = k0T + pcorr
    
    
    #  pH on free scale then corrected to get to pH total on mol/kg sw scale
    #    pHinsituFree = (Vrs - k0TP) / (R * Tk / F * ln10) + ...
    #                   log(Cltotal) / ln10 + 2 * log10gammaHCLtP
    #  this will be mol kg H2O  need to convert to mol/kg sw
    phfree = (Vrs - k0TP) / (R * Tk / Fa * ln10) + log(Cltotal) / ln10 + 2 * log10gammaHCLtP # %mol/kg-H2O scale
    
    # CONVERT TO mol/kg-sw scale - JP 2/4/16
    phfree = phfree - log10(1 - 0.001005 * Salt)  #mol/kg-sw scale
    
    # convert to total proton scale
    phtot = phfree - log10(1 + Stotal / Khso4TPS)
    
    
    
  }
  
  return(phtot)
  
}

