library(Rcpp)
library(deSolve)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(scales)

getwd()
#set Working Directory (Change to your Working Directory)
setwd("C:/Hep-c/HepC-betaweb/HepC-betaweb")


#parses the specified C++ file
sourceCpp('p1_backup.cpp')


#Parameters
parms <- list(
  K=66785001,
  P0=61623140, #popstat(YEAR=1999)
  r =0.16,
  S0= 0.0305853,
  standard_start = 2004,
  new_start = 2015,
  
  
  #cost = treated1+treated2
  total_HCC=0,
  total_HCV=631000,
  
  FI=10^-8,
  
  f0f1=0.117,
  f1f2=0.085,
  f2f3=0.12,
  f3c1=0.116,
  c1c2=0.044,
  c2c3=0.076,
  c3c4=0.076,
  c1bA=0.0068,
  c1bB=0.0099,
  c1bC=0.0029,
  c1bD=0.0068,
  
  c2bA=0.0068,
  c2bB=0.0099,
  c2bC=0.0029,
  c2bD=0.0068,
  
  c3bD=0.0664,
  c4bD=0.0664,
  deathc1=0.01,
  deathc2=0.2,
  deathc3=0.57,
  deathc4=0.57,
  deathbA=1/(36/12),
  deathbB=1/(16/12),
  deathbC=1/(6/12),
  deathbD=1/(3/12),
  deathtrn=1/(240/12),
  tranc4=0.0015,
  tranbA=0.0015,
  tranbB=0.0015,
  #;newcase=17000,
  cover = 5,
  natdeath=0.0424,
  
  beta=0.327,
  std_cureF0=0.7,
  std_cureF1 =0.7,
  std_cureF2 =0.7,
  std_cureF3 =0.7,
  std_cureC1 =0.7,
  std_cureC2=0.7,
  new_cureF0=0.985,
  new_cureF1=0.985,
  new_cureF2 =0.985,
  new_cureF3 =0.985,
  new_cureC1 =0.985,
  new_cureC2 =0.985,
  new_cureC3 =0.985,
  new_cureC4 =0.985,
  std_dist = c(0.05,0.05,0.3,0.3,0.3)
  
)

S <- parms$S0* parms$P0;

#Initial
inits <- c(
  S = S, #;0.01*#pop_since1960(TIME=1)
  F0=0.2825*S,#;genotype1
  F1=0.2825*S,
  F2=0.184*S,
  F3=0.124*S,
  C1=0.03175*S,# ;CirA
  C2=0.03175*S,#; CirA
  C3=0.03175*S,#; CirB
  C4=0.03175*S,#;CirC
  
  HCC_A=0,
  HCC_B=0,
  HCC_C=0,
  HCC_D=0,
  
  C1std_cured=0,
  C1new_cured=0,
  C2new_cured=0,
  C3new_cured=0,
  C4new_cured=0,
  
  death = 0,
  deathHCC = 0,
  deathC14 = 0
  
)
times <- seq(1999, 2016, by = 0.01)

## model
outputs <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")

outputs

plot(outputs)


outputs_df <- as.data.frame(outputs)
time_year <- outputs_df["time"] %% 1 ==0
outputs_df_year <- outputs_df[time_year,]

#Choose column
x <- outputs_df[colnames(outputs_df) %in% c("time","S")]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("F0","F1","F2","F3"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1","C2","C3","C4"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("HHC_A","HHC_B","HHC_C","HHC_D"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("C1std_cured","C1new_cured"
                                            ,"C2new_cured","C3new_cured","C4new_cured"))]
x <- outputs_df[colnames(outputs_df) %in% c("time",c("death","deathHCC","deathC14"))]

#The melt function takes data in wide format 
#and stacks a set of columns into a single column of data.
x_melt <-melt(x, id="time")

#plot graph(ggplot2) 
ggplot(data = x_melt) + 
  geom_line(mapping = aes(x = time, y = value,color = variable),size = 1.5)
