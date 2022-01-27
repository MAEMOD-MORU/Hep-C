//saralamba@gmail.com

#include <Rcpp.h>
#include <math.h> 
using namespace Rcpp;

double popfn(double time, double K, double P0, double r){
  return(K*P0*exp(r*(time-1998))/(K+P0*(exp(r*(time-1998))-1)));
}

// [[Rcpp::export]]
List PanHepC(double time, NumericVector state, List parms){

  //get the value of each parameter from parms
  double K = parms["K"];
  double P0 = parms["P0"];
  
  double new_start = parms["new_start"];
  double standard_start = parms["standard_start"];
  double cover = parms["cover"];
  
  double r = parms["r"];
  double FI = parms["FI"];
  
  double f0f1 = parms["f0f1"];
  double f1f2 = parms["f1f2"];
  double f2f3 = parms["f2f3"];
  double f3c1 = parms["f3c1"];
  double c1c2 = parms["c1c2"];
  double c2c3 = parms["c2c3"];
  double c3c4 = parms["c3c4"];
  double c1bA = parms["c1bA"];
  double c1bB = parms["c1bB"];
  double c1bC = parms["c1bC"];
  double c1bD = parms["c1bD"];
  
  double c2bA = parms["c2bA"];
  double c2bB = parms["c2bB"];
  double c2bC = parms["c2bC"];
  double c2bD = parms["c2bD"];
  
  double c3bD = parms["c3bD"];
  double c4bD = parms["c4bD"];
  double deathc1 = parms["deathc1"];
  double deathc2 = parms["deathc2"];
  double deathc3 = parms["deathc3"];
  double deathc4 = parms["deathc4"];
  double deathbA = parms["deathbA"];
  double deathbB = parms["deathbB"];
  
  double deathbC = parms["deathbC"];
  double deathbD = parms["deathbD"];
  double deathtrn = parms["deathtrn"];
  double tranc4 = parms["tranc4"];
  double tranbA = parms["tranbA"];
  double tranbB = parms["tranbB"];

  double natdeath = parms["natdeath"];
          
  SEXP stddist = parms["std_dist"];
  NumericVector std_dist(stddist);

  
  double beta = parms["beta"];
  
  double std_cureF0 = parms["std_cureF0"];
  double std_cureF1 = parms["std_cureF1"];
  double std_cureF2 = parms["std_cureF2"];
  double std_cureF3 = parms["std_cureF3"];
  double std_cureC1 = parms["std_cureC1"];
  //double std_cureC2 = parms["std_cureC2"];
  
  double new_cureF0 = parms["new_cureF0"];
  double new_cureF1 = parms["new_cureF1"];
  double new_cureF2 = parms["new_cureF2"];
  double new_cureF3 = parms["new_cureF3"];
  double new_cureC1 = parms["new_cureC1"];
  double new_cureC2 = parms["new_cureC2"];
  double new_cureC3 = parms["new_cureC3"];
  double new_cureC4 = parms["new_cureC4"];
  
  //get the state values  
  double S = state["S"];
  double C1 = state["C1"]; 
  double C2 = state["C2"];
  double C3 = state["C3"];
  double C4 = state["C4"];
  double F0 = state["F0"];
  double F1 = state["F1"];
  double F2 = state["F2"];
  double F3 = state["F3"];
  
  double HCC_A = state["HCC_A"];
  double HCC_B = state["HCC_B"];
  double HCC_C = state["HCC_C"];
  double HCC_D = state["HCC_D"];
  
  double C1std_cured = state["C1std_cured"];
  double C1new_cured = state["C1new_cured"];
  double C2new_cured = state["C2new_cured"];
  double C3new_cured = state["C3new_cured"];
  double C4new_cured = state["C4new_cured"];
  
  double death = state["death"];
  double deathHCC = state["deathHCC"];
  double deathC14 = state["deathC14"];
   
  double pop;
  pop = popfn(time, K, P0, r);
    
  double treated1;
  if((new_start>time) && (time >= standard_start)) 
    treated1 = (1000+(time-standard_start)*200);
  else
    treated1 = 0;
  
  double treated2;
  if(new_start > time)
    treated2 = 0;
  else
    treated2 = cover*(3000+(time-new_start)*200);
  
  
  NumericVector std_tr = treated1*std_dist;
 
  NumericVector new_dist(8);  
  //F0
  if(C4 > 0)
    new_dist[0] = 0.05;
  else if(C3 > 0 )
    new_dist[0] = 0.05;
  else if(C2 > 0)
    new_dist[0] = 0.05;
  else if(C1 > 0)
    new_dist[0] = 0.05;
  else if(F3 > 0 )
    new_dist[0] = 0.05;
  else if(F2 > 0)
    new_dist[0] = 0.05;
  else if(F1 > 0)
    new_dist[0] = 0.05;
  else if(F0 > 0)
    new_dist[0] = 1;
  else
    new_dist[0] = 0;
  
  //F1
  if(C4 > 0)
    new_dist[1] = 0.05;
  else if(C3 > 0)
    new_dist[1] = 0.05;
  else if(C2 > 0)
    new_dist[1] = 0.05;
  else if(C1 > 0)
    new_dist[1] = 0.05;
  else if(F3 > 0)
    new_dist[1] = 0.05;
  else if(F2 > 0)
    new_dist[1] = 0.05;
  else if(F1 > 0)
    new_dist[1] = 0.9+0.05;
  else 
    new_dist[1] = 0;
  
  //F2
  if(C4 > 0)
    new_dist[2] = 0.15;
  else if(C3 > 0)
    new_dist[2] = 0.15;
  else if(C2 > 0)
    new_dist[2] = 0.15;
  else if(C1 > 0)
    new_dist[2] = 0.15;
  else if(F3 > 0)
    new_dist[2] = 0.15;
  else if(F2 > 0)
    new_dist[2] = 0.9;
  else
    new_dist[2] = 0;
  
  //F3
  if(C4 > 0)
    new_dist[3] = 0.15;
  else if(C3 > 0)
    new_dist[3] = 0.15;
  else if(C2 > 0)
    new_dist[3] = 0.15;
  else if(C1 > 0)
    new_dist[3] = 0.15;
  else if(F3 > 0)
    new_dist[3] = 0.15*5;
  else 
    new_dist[3] = 0;

  //C1
  if(C4 > 0)
    new_dist[4] = 0.15;
  else if(C3 > 0)
    new_dist[4] = 0.15;
  else if(C2 > 0)
    new_dist[4] = 0.15;
  else if(C1 > 0)
    new_dist[4] = 0.15+0.15+0.15+0.15;
  else
    new_dist[4] = 0;
  
  //C2
  if(C4 > 0)
    new_dist[5] = 0.15;
  else if(C3 > 0)
    new_dist[5] = 0.15;
  else if(C2 > 0)
    new_dist[5] = 0.15+0.15+0.15;
  else 
    new_dist[5] = 0;
  

  //C3
  if(C4 > 0)
    new_dist[6] = 0.15;
  else if(C3 > 0)
    new_dist[6] = 0.15+0.15;
  else
    new_dist[6] = 0;
  
  //C4
  if(C4 > 0)
    new_dist[7] = 0.15;
  else
    new_dist[7] = 0;

  NumericVector new_tr = new_dist*treated2;

  
  double flowin = FI;
  double infect = (F0+F1+F2+F3+C1+C2+C3+C4+HCC_A+HCC_B+HCC_C+HCC_D);

  double lam0 = beta*infect/pop;

  double dS = flowin*pop-lam0*S-natdeath*S;
    
  double dF0 = -f0f1*F0+lam0*S-(std_tr[0]*std_cureF0)-(new_tr[0]*new_cureF0)-natdeath*F0;
    
  double dF1 =  f0f1*F0 -f1f2*F1-std_tr[1]*std_cureF1-new_tr[1]*new_cureF1-natdeath*F1;
    
  double dF2 = f1f2*F1-f2f3*F2-std_tr[2]*std_cureF2-new_tr[2]*new_cureF2-natdeath*F2;
    
  double dF3 = f2f3*F2-f3c1*F3-std_tr[3]*std_cureF3-new_tr[3]*new_cureF3-natdeath*F3;
    
  double dC1 = f3c1*F3-deathc1*C1-c1c2*C1-std_tr[4]*std_cureC1-new_tr[4]*new_cureC1-(c1bA+c1bB+c1bC+c1bD)*C1-natdeath*C1;
    
  double dC2 = c1c2*C1-deathc2*C2-c2c3*C2-new_tr[5]*new_cureC2-(c2bA+c2bB+c2bC+c2bD)*C2-natdeath*C2;
    
  double dC3 = c2c3*C2-deathc3*C3-c3c4*C3-new_tr[6]*new_cureC3-c3bD*C3-natdeath*C3;
    
    
  double dC4 = c3c4*C3-new_tr[7]*new_cureC4-deathc4*C4-c4bD*C4-natdeath*C4;
    
  
  double dHCC_A = c1bA*(C1+C1std_cured+C1new_cured)+c2bA*(C2+C2new_cured)-deathbA*HCC_A-tranbA*HCC_A-natdeath*HCC_A;
    
  double dHCC_B = c1bB*(C1+C1std_cured+C1new_cured)+c2bB*(C2+C2new_cured)-deathbB*HCC_B-tranbB*HCC_B-natdeath*HCC_B;
    
  double dHCC_C = c1bC*(C1+C1std_cured+C1new_cured)+c2bC*(C2+C2new_cured)-deathbC*HCC_C-natdeath*HCC_C;
    
  double dHCC_D = c1bD*(C1+C1std_cured+C1new_cured)+c2bD*(C2+C2new_cured)-deathbD*HCC_D+c3bD*(C3+C3new_cured)+c4bD*(C4+C4new_cured)-natdeath*HCC_D;
    
  double ddeath =  deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4+deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;

    
  double ddeathHCC = deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;
    
  double ddeathC14 = deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4;
  
  
  double dC1std_cured = std_tr[4]*std_cureC1-natdeath*(C1std_cured)-(c1bA+c1bB+c1bC+c1bD)*C1std_cured;
    
  double dC1new_cured = new_tr[4]*new_cureC1-natdeath*(C1new_cured)-(c1bA+c1bB+c1bC+c1bD)*C1new_cured;
    
  double dC2new_cured = new_tr[5]*new_cureC2-natdeath*(C1new_cured)-(c2bA+c2bB+c2bC+c2bD)*C2new_cured;
    
  double dC3new_cured = new_tr[6]*new_cureC3-natdeath*(C3new_cured)-c3bD*C3new_cured;
    
  double dC4new_cured = new_tr[7]*new_cureC4-natdeath*(C4new_cured)-c4bD*C4new_cured;
    
  double new_tranLiv=tranc4*((C4-new_tr[7])+new_tr[7]*(1-new_cureC4))+tranbA*HCC_A+tranbB*HCC_B;
    
  double new_HCV_HCC=c1bA*C1+c2bA*C2+c1bB*C1+c2bB*C2+c1bC*C1+c2bC*C2+c1bD*C1+c2bD*C2;
  
  
  
  double new_death = deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4+deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;
  
  double HCC = HCC_A+HCC_B+HCC_C+HCC_D;
  
  double incHCC = c1bA*(C1+C1std_cured+C1new_cured)+c2bA*(C2+C2new_cured)+c1bB*(C1+C1std_cured+C1new_cured)+c2bB*(C2+C2new_cured)+c1bC*(C1+C1std_cured+C1new_cured)+c2bC*(C2+C2new_cured)+c1bD*(C1+C1std_cured+C1new_cured)+c2bD*(C2+C2new_cured)+c3bD*(C3+C3new_cured)+c4bD*(C4+C4new_cured);
  
  
  
  NumericVector propF(4);  
  propF[0] = F0/(F0+F1+F2+F3);
  propF[1] = F1/(F0+F1+F2+F3);
  propF[2] = F2/(F0+F1+F2+F3);
  propF[3] = F3/(F0+F1+F2+F3);
      
  double prev = 100*(infect/pop);
  
  double total_HCC = HCC_A+HCC_B+HCC_C+HCC_D;
  double total_HCV = F0+F1+F2+F3+C1+C2+C3+C4;
    

  NumericVector compartments(21);
  // compartments["S"] = dS;
  // compartments["F0"] = dF0;
  // compartments["F1"] = dF1;
  // compartments["F2"] = dF2;
  // compartments["F3"] = dF3;
  // compartments["C1"] = dC1;
  // compartments["C2"] = dC2;
  // compartments["C3"] = dC3;
  // compartments["C4"] = dC4;
  // compartments["HCC_A"] = dHCC_A;
  // compartments["HCC_B"] = dHCC_B;
  // compartments["HCC_C"] = dHCC_C;
  // compartments["HCC_D"] = dHCC_D;
  // compartments["death"] = ddeath;
  // compartments["deathHCC"] = ddeathHCC;
  // compartments["deathC14"] = ddeathC14;
  // compartments["C1std_cured"] = dC1std_cured;
  // compartments["C1new_cured"] = dC1new_cured;
  // compartments["C2new_cured"] = dC2new_cured;
  // compartments["C3new_cured"] = dC3new_cured;
  // compartments["C4new_cured"] = dC4new_cured;
  compartments[0] = dS;
  compartments[1] = dF0;
  compartments[2] = dF1;
  compartments[3] = dF2;
  compartments[4] = dF3;
  compartments[5] = dC1;
  compartments[6] = dC2;
  compartments[7] = dC3;
  compartments[8] = dC4;
  compartments[9] = dHCC_A;
  compartments[10] = dHCC_B;
  compartments[11] = dHCC_C;
  compartments[12] = dHCC_D;
  compartments[13] = ddeath;
  compartments[14] = ddeathHCC;
  compartments[15] = ddeathC14;
  compartments[16] = dC1std_cured;
  compartments[17] = dC1new_cured;
  compartments[18] = dC2new_cured;
  compartments[19] = dC3new_cured;
  compartments[20] = dC4new_cured;
  
  List outlist(3);
  outlist[0] = compartments;
  outlist[1] = prev;
  outlist[2] = incHCC;
      
  
  return outlist;
  
  
}


/*** R

*/
