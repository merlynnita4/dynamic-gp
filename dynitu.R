data <- read.delim("C:/Users/R. Nita/Desktop/timeseries.txt")
    
attach(data)

functionSet1 <- functionSet("+","*","-","/","exp")

constantFactorySet1 <- constantFactorySet(function() rnorm(1))
inputVariableSet1 <- inputVariableSet("time")
#----------------------------------------------------------------------------------  
initial_big_window_size=10
initial_small_window_size=8
prediction_window_size=4

window_move_distance=2

window_increase_size=2
window_decrease_size=2

threshold_stable_process=4
threshold_process_change=4

min_rmse=50
#----------------------------------------------------------------------------------

x_big=1
y_big=initial_big_window_size

x_small=2
y_small=initial_small_window_size

rmse_1=0
rmse_2=0

expansion_variable=0  # used to detect continuous window expansion to determine stable environments
contract_variable=0   # used to detect continuous window contracttions to determine process change

for (i in 1:length(sensex))
{
  print("LOOP NUM:")
  print(i)
  if(i==1) 
   { 
     big_window=data[x_big:y_big,]
     small_window=data[x_small:y_small,]
     
     x_pred=y_big+1
     y_pred=y_big+prediction_window_size
     
     pred_window=data[x_pred:y_pred,]

     fitnessFunction1 <- function(f) rmse(Vectorize(f)(big_window$time), (big_window$sensex))
     fitnessFunction2 <- function(f) rmse(Vectorize(f)(small_window$time), (small_window$sensex))

     gpResult1 <- geneticProgramming(populationSize=100,restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction1,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
     gpResult2 <- geneticProgramming(populationSize=100,restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction2,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
     
     best1 <- gpResult1$population[[which.min(sapply(gpResult1$population, fitnessFunction1))]]
     best2 <- gpResult2$population[[which.min(sapply(gpResult2$population, fitnessFunction2))]]
     
     #-------------- sort of initialising the dormant population
     dormant_population=best1
     best_50<-gpResult1$population
     #----------------------------------------------------------
     
     big_pred=best1(pred_window$time)
     small_pred=best2(pred_window$time)

     rmse_1= rmse(big_pred,pred_window$sensex)
     rmse_2= rmse(small_pred,pred_window$sensex)
     
   }
   if(i>1)
   {
     # moving the window forward---------------------------------
     x_big=x_big+window_move_distance
     y_big=y_big+window_move_distance
     
     x_small=x_small+window_move_distance
     y_small=y_small+window_move_distance
     
     #-----------------------------------------------------------
     
        if(rmse_1<=rmse_2)
          {
            # if big window has lesser error, increase window size----------
            y_big=y_big+window_increase_size
           y_small=y_small+window_increase_size
            #----------------------------------------------------------------
            
            expansion_variable=expansion_variable+1
            contract_variable=0
              # continuous 2 window increase is treated as stable environment 
            if(expansion_variable>=threshold_stable_process)
              {
                # extracting 50 best functions from population
                print("STABLE PROCESS")
                # evaluates the fitness of all the solutions
                fittness_eval=sapply(gpResult1$population, fitnessFunction1)
                
                z= which.min(fittness_eval)
                
                best_50<-gpResult1$population[[z]]
                
                fittness_eval[z]=1/0
                for(j in 1:49)
                 {
                   z= which.min(fittness_eval)
                   fittness_eval[z]=1/0
                   best_50=c(best_50,gpResult1$population[[z]])
                 }
              }
          }
        if(rmse_1>rmse_2)
          {
            # resseting the expansion variable
            expansion_variable=0
            contract_variable=contract_variable+1
            
            # if small window has lesser error----------------------------
            if(length(small_window)>window_decrease_size)
              {
                # decreasing the window sizes only if small window length > window_decrease_size---
                y_big=y_big-window_decrease_size
               y_small=y_small-window_decrease_size        
                #---------------------------------------------------------------
              }
            #-------------------------------------------------------------  
          }
          
     big_window=data[x_big:y_big,]
     small_window=data[x_small:y_small,]
     
     x_pred=y_big+1
     y_pred=y_big+prediction_window_size
     pred_window=data[x_pred:y_pred,]

     fitnessFunction1 <- function(f) rmse(Vectorize(f)(big_window$time), (big_window$sensex))
     fitnessFunction2 <- function(f) rmse(Vectorize(f)(small_window$time), (small_window$sensex))

     #-------contract_variable<2 means no process change
     if(contract_variable<threshold_process_change)
       {
        
        gpResult1 <- geneticProgramming(restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction1,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
        gpResult2 <- geneticProgramming(restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction2,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
       }
     #----------------------------------------------------
     
     #------ contract_variable>=2 signifies a process change
     if(contract_variable>=threshold_process_change)
       {
         print("PROCESS CHANGE")
         
        dormant_population= c(dormant_population,best_50)
        #-- when the process is changing using the previous dormant populations as start
        gpResult1 <- geneticProgramming(dormant_population,restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction1,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
        gpResult2 <- geneticProgramming(dormant_population,restartStrategy=makeLocalRestartStrategy(),functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction2,selectionFunction=makeTournamentSelection(tournamentSize=50,selectionSize=ceiling(25),tournamentDeterminism=1, vectorizedFitness=FALSE), stopCondition=makeFitnessStopCondition(min_rmse))
       }
    #-----------------------------------------------------
       
     best1 <- gpResult1$population[[which.min(sapply(gpResult1$population, fitnessFunction1))]]
     best2 <- gpResult2$population[[which.min(sapply(gpResult2$population, fitnessFunction2))]]

     big_pred=best1(pred_window$time)
     small_pred=best2(pred_window$time)

     rmse_1= rmse(big_pred,pred_window$sensex)
     rmse_2= rmse(small_pred,pred_window$sensex)      
   }
     if(y_pred==length(sensex))
     {
       break
     }

}



