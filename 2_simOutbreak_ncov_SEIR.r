## To simulate n_SEIR_simSEIR outbreaks

nsim = 200 # number of simulations. Orig: 200

# set.seed(123)
r0postCrI = r0posterior # all observed R0 values from 2020-01-01 to 2020-01-23
hist(r0postCrI)
summary(r0postCrI)
R0est = sample(x = r0postCrI,size = nsim)
# print(R0est)

dI = c(3, 7, 14)
I0 = c(200)
ages_plot = c(3, 12) # c(3, 12) - 10-15, 55-60; 1:16 - all ages


## To simulate n_SEIR_sim SEIR outbreaks: duration of infection = 3 days, initial infected  n=~200 infected

for (curr_dI in dI)
{
  for (curr_I0 in I0)
  {
    print(paste0('Begin SEIR, dI = ', curr_dI, ', I0 = ', curr_I0))
    
    epi_SEIR_doNothingDurInf = vector('list',nsim)
    epi_SEIR_baseDurInf = vector('list',nsim)
    epi_SEIR_marchDurInf = vector('list',nsim)
    epi_SEIR_aprilDurInf = vector('list',nsim)
    # epi_SEIR_MayDurInf = vector('list',nsim)
    start = Sys.time()
    durInfSim = curr_dI
    initialI = curr_I0 * 10^-6
    for(sim in 1:nsim)
    {
      # do nothing
      epi_SEIR_doNothingDurInf[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] , rho = rep(0.5, 3660), dateStartSchoolClosure = as.Date('2019-11-01'),
                                                                     dateStartIntenseIntervention = as.Date('2019-11-01'), dateEndIntenseIntervention = as.Date('2019-11-01'),
                                                                     pWorkOpen = c(1,1,1,1),numWeekStagger = c(0,0,0),pInfected=initialI,durInf = durInfSim)
      # base 
      epi_SEIR_baseDurInf[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] , rho = rep(0.5, 3660), dateEndIntenseIntervention = as.Date('2020-01-31'),pWorkOpen = c(0.1,0.75,1,1),
                                                                numWeekStagger = c(10/7,10/7,10/7),pInfected=initialI,durInf = durInfSim)
      # March
      epi_SEIR_marchDurInf[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] , rho = rep(0.5, 3660), dateEndIntenseIntervention = as.Date('2020-03-01'),
                                                                 pInfected=initialI,durInf = durInfSim)
      # April
      epi_SEIR_aprilDurInf[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] , rho = rep(0.5, 3660), dateEndIntenseIntervention = as.Date('2020-04-01'),
                                                                 pInfected=initialI,durInf = durInfSim)
      # # May
      # epi_SEIR_MayDurInf[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] ,dateEndIntenseIntervention = as.Date('2020-05-01'),
      #                                                            pInfected=initialI,durInf = durInfSim)
      if(sim%%10==0) print(paste0('Done with simulation ',sim))
    }
    end = Sys.time()
    print(end-start)
    
    covid_SEIR_SDurInfsc = list() 
    covid_SEIR_SDurInfsc[[1]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_SEIR_doNothingDurInf)
    covid_SEIR_SDurInfsc[[2]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_SEIR_baseDurInf)
    covid_SEIR_SDurInfsc[[3]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_SEIR_marchDurInf)
    covid_SEIR_SDurInfsc[[4]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_SEIR_aprilDurInf)
    # covid_SEIR_SDurInfsc[[5]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_SEIR_MayDurInf)
    
    covid_SEIR_IDurInfsc = list() 
    covid_SEIR_IDurInfsc[[1]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_doNothingDurInf)
    covid_SEIR_IDurInfsc[[2]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_baseDurInf)
    covid_SEIR_IDurInfsc[[3]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_marchDurInf)
    covid_SEIR_IDurInfsc[[4]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_aprilDurInf)
    # covid_SEIR_IDurInfsc[[5]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_MayDurInf)
    
    peaktime_SEIR_DurInfsc = list()
    peaktime_SEIR_DurInfsc[[1]] = summarisePeakTimePeakSize(SIMS = epi_SEIR_doNothingDurInf)
    peaktime_SEIR_DurInfsc[[2]] = summarisePeakTimePeakSize(SIMS = epi_SEIR_baseDurInf)
    peaktime_SEIR_DurInfsc[[3]] = summarisePeakTimePeakSize(SIMS = epi_SEIR_marchDurInf)
    peaktime_SEIR_DurInfsc[[4]] = summarisePeakTimePeakSize(SIMS = epi_SEIR_aprilDurInf)
    # peaktime_SEIR_DurInfsc[[5]] = summarisePeakTimePeakSize(SIMS = epi_SEIR_MayDurInf)
    
    covid_SEIR_DurInfsc = list() 
    covid_SEIR_DurInfsc[[1]] = summariseSimulations_mid(CI = 50,SIMS = epi_SEIR_doNothingDurInf)
    covid_SEIR_DurInfsc[[2]] = summariseSimulations_mid(CI = 50,SIMS = epi_SEIR_baseDurInf)
    covid_SEIR_DurInfsc[[3]] = summariseSimulations_mid(CI = 50,SIMS = epi_SEIR_marchDurInf)
    covid_SEIR_DurInfsc[[4]] = summariseSimulations_mid(CI = 50,SIMS = epi_SEIR_aprilDurInf)
    # covid_SEIR_DurInfsc[[5]] = summariseSimulations_mid(CI = 50,SIMS = epi_SEIR_MayDurInf)
    
    AGEcovid_SEIR_IDurInfsc = list()
    AGEcovid_SEIR_IDurInfsc[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_doNothingDurInf)
    AGEcovid_SEIR_IDurInfsc[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_baseDurInf)
    AGEcovid_SEIR_IDurInfsc[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_marchDurInf)
    AGEcovid_SEIR_IDurInfsc[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_aprilDurInf)
    # AGEcovid_SEIR_IDurInfsc[[5]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_SEIR_MayDurInf)
    
    epi_SEIR_FirstSimDurInfsc = list(epi_SEIR_doNothingDurInf = epi_SEIR_doNothingDurInf[[1]],
                                         epi_SEIR_baseDurInf= epi_SEIR_baseDurInf[[1]],
                                         epi_SEIR_marchDurInf = epi_SEIR_marchDurInf[[1]],
                                         epi_SEIR_aprilDurInf = epi_SEIR_aprilDurInf[[1]])
    # epi_SEIR_MayDurInf = epi_SEIR_MayDurInf[[1]])
    
    paste0('outputs/SEIR/epi_SEIR_FirstSim_dI=', curr_dI, '_I0=', curr_I0, ".rdata")
    # save results
    save(covid_SEIR_IDurInfsc,file = paste0('outputs/SEIR/covid_SEIR_I_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    save(covid_SEIR_SDurInfsc,file = paste0('outputs/SEIR/covid_SEIR_S_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    
    save(peaktime_SEIR_DurInfsc,file = paste0('outputs/SEIR/peaktime_SEIR_DurInf_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    
    save(covid_SEIR_DurInfsc,file = paste0('outputs/SEIR/covid_SEIR_Mid_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    
    save(AGEcovid_SEIR_IDurInfsc,file = paste0('outputs/SEIR/AGEcovid_SEIR_I_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    
    save(epi_SEIR_FirstSimDurInfsc,file = paste0('outputs/SEIR/epi_SEIR_FirstSim_dI=', curr_dI, '_I0=', curr_I0, ".rdata"))
    
    # rm(epi_SEIR_doNothingDurInf,epi_SEIR_baseDurInf,epi_SEIR_marchDurInf,epi_SEIR_aprilDurInf)
    
    
    ## make plots
    
    colors = c('purple', 'blue', 'red', 'orange') # , 'green')
    
    # ##################################################################
    # if (length(ages_plot) <= 3)
    # {
    #   par(mfrow=c(1,length(ages_plot) * 2))
    # } else if (length(ages_plot) > 3 & length(ages_plot) <= 12){
    #   par(mfrow=c(2,ceiling(length(ages_plot))))
    # } else {
    #   par(mfrow=c(4,ceiling(length(ages_plot) / 2)))
    # }
    # 
    # 
    # # plot daily cases and cumulative cases (percent) for all senarios
    # for (agegp in ages_plot) 
    # {
    #   
    #   # cumulative incidence over time
    #   plot(epi_SEIR_doNothingDurInf[[1]]$time, (epi_SEIR_doNothingDurInf[[1]]$N_age[agegp]-epi_SEIR_doNothingDurInf[[1]]$S[,agegp])/epi_SEIR_doNothingDurInf[[1]]$N_age[agegp], lwd=1,type='l', 
    #        main=paste0("Cum incidence for age [",(agegp-1)*5,',',agegp*5,')'), sub=(paste0("dI=", curr_dI, ", I0=", curr_I0, ", SEIR")),
    #        xlab="Time(days)", ylab="Cum incidence",ylim = c(0,1), col='purple');
    #   lines(epi_SEIR_baseDurInf[[1]]$time, (epi_SEIR_baseDurInf[[1]]$N_age[agegp]-epi_SEIR_baseDurInf[[1]]$S[,agegp])/epi_SEIR_baseDurInf[[1]]$N_age[agegp],lwd=1,col='blue')
    #   lines(epi_SEIR_marchDurInf[[1]]$time, (epi_SEIR_marchDurInf[[1]]$N_age[agegp]-epi_SEIR_marchDurInf[[1]]$S[,agegp])/epi_SEIR_marchDurInf[[1]]$N_age[agegp],lwd=1,col='red')
    #   lines(epi_SEIR_aprilDurInf[[1]]$time, (epi_SEIR_aprilDurInf[[1]]$N_age[agegp]-epi_SEIR_aprilDurInf[[1]]$S[,agegp])/epi_SEIR_aprilDurInf[[1]]$N_age[agegp],lwd=1,col='orange')
    #   # lines(epi_SEIR_MayDurInf[[1]]$time, (epi_SEIR_MayDurInf[[1]]$N_age[agegp]-epi_SEIR_MayDurInf[[1]]$S[,agegp])/epi_SEIR_MayDurInf[[1]]$N_age[agegp],lwd=1,col='green')
    #   legend(0.25, 0.98, legend=c("Do Nothing", "Base","Lockdown->March","Lockdown->April"),
    #          col=colors, bty='n',lty=array(1, 4),lwd=array(2, 4), cex=0.7)
    #   
    #   # incidence over time
    #   plot(epi_SEIR_doNothingDurInf[[1]]$time, epi_SEIR_doNothingDurInf[[1]]$incidence[,agegp], type='l', lwd=1,
    #        main=paste0("Incidence for age [",(agegp-1)*5,',',agegp*5,')'), sub=(paste0("dI=", curr_dI, ", I0=", curr_I0, ", SEIR")),
    #        xlab="Time(days)", ylab="Daily no. of infections", ylim=c(0, 14000), col='purple');
    #   lines(x=epi_SEIR_baseDurInf[[1]]$time,y=epi_SEIR_baseDurInf[[1]]$incidence[,agegp],lwd=1,col='blue')
    #   lines(x=epi_SEIR_marchDurInf[[1]]$time,y=epi_SEIR_marchDurInf[[1]]$incidence[,agegp],lwd=1,col='red')
    #   lines(x=epi_SEIR_aprilDurInf[[1]]$time,y=epi_SEIR_aprilDurInf[[1]]$incidence[,agegp],lwd=1,col='orange')
    #   # lines(x=epi_SEIR_MayDurInf[[1]]$time,y=epi_SEIR_MayDurInf[[1]]$incidence[,agegp],lwd=1,col='green')
    #   legend(0.25, 14000*0.98, legend=c("Do Nothing", "Base","Lockdown->March","Lockdown->April"),
    #          col=colors, bty='n',lty=array(1, 4),lwd=array(2, 4), cex=0.7)
    #   
    # }
    # 
    # #############################################################
    # cases averted
    # par(mfrow=c(1, 1))
    # 
    # end_day <- 428 # 405 (2020-12-31), 428 (orig)
    # 
    # # plot cases averted
    # total_case = vector('list', 5)
    # averted = vector('list', 5)
    # 
    # for (case in 1:4)
    # {
    #   total_case[[case]] <- covid_SEIR_DurInfsc[[case]]$S_age$med[1,] - covid_SEIR_DurInfsc[[case]]$S_age$med[end_day,]
    #   averted[[case]] <- (total_case[[1]] - total_case[[case]]) / total_case[[1]]
    # }
    # 
    # x <- seq(0, 75, by=5)
    # plot(x, averted[[2]], type="s", xlim=c(0, 80), ylim=c(0, 1), xlab="age", ylab="cases averted (%)", main=c(expression("SEIR")), sub=(paste0("dI=", curr_dI, ", I0=", curr_I0, ", SEIR")), col="blue");
    # lines(x, averted[[3]], type="s", col="red")
    # lines(x, averted[[4]], type="s", col="orange")
    # # lines(x, averted[[5]], type="s", col="green")
    # legend(0.25, 0.98, legend=c("Base","Lockdown->March","Lockdown->April"),
    #        col=colors[2:length(colors)], bty='n',lty=array(1, 4),lwd=array(2, 4), cex=0.7)
    # 
    # rm(total_case, averted)
    # #################################################################################
    par(mfrow=c(1, 1))

    plot(covid_SEIR_SDurInfsc[[1]]$Sim1$time, (sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[1]]$summary$median) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age), type='l', lwd=1,
         main=("Median of all simulations with IQR"), sub=(paste0("dI=", curr_dI, ", I0=", curr_I0, ", SEIR")),
         xlab="Time(days)", ylab="Cumulative incidence (%)", ylim=c(0, 1), col='purple');
    for (case in 2:4)
    {
      lines(x=covid_SEIR_SDurInfsc[[1]]$Sim1$time,y=(sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[case]]$summary$median) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age),lwd=1,col=colors[case])
    }
    legend(0.25, 0.98, legend=c("Do Nothing", "Base","Lockdown->March","Lockdown->April"),
           col=colors, bty='n',lty=array(1, 4),lwd=array(2, 4), cex=0.7)

    ################################################################################
    # # media + IQR
    # par(mfrow=c(1, 4))
    # 
    # sen = c("No intervention", "baseline", "Lockdown->March", "Lockdown->April")
    # 
    # for (case in 1:4)
    # {
    #   plot(covid_SEIR_SDurInfsc[[1]]$Sim1$time, (sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[case]]$summary$median) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age), type='l', lwd=1,
    #        main=(sen[case]), # sub=(paste0("dI=", curr_dI, ", I0=", curr_I0, ", SEIR")),
    #        xlab="Time(days)", ylab="Cumulative incidence (%)", ylim=c(0, 1), col=colors(case));
    #   
    #   lines(x=covid_SEIR_SDurInfsc[[1]]$Sim1$time,y=(sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[case]]$summary$median) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age),lwd=1,col=colors[case])
    #   lines(x=covid_SEIR_SDurInfsc[[1]]$Sim1$time,y=(sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[case]]$summary$lci) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age),lwd=1,col=colors[case])
    #   lines(x=covid_SEIR_SDurInfsc[[1]]$Sim1$time,y=(sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age) - covid_SEIR_SDurInfsc[[case]]$summary$uci) / sum(covid_SEIR_SDurInfsc[[1]]$Sim1$N_age),lwd=1,col=colors[case])
    # }
    # 
  }
}

