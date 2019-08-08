
#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

// for new R-devel
void R_init_vennplot(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

void loss(double& gammar, NumericVector& deltaloss,
          double& alphadelta, NumericVector& cn,double& alpha,
          NumericMatrix& xy, int i,double& lambda,
          NumericVector& radius, NumericMatrix& ED, bool ThreeD,
          NumericVector& tmp) {
  double deltaloss1 = 0.0;
  double deltaloss2 = 0.0;
  double deltaloss3 = 0.0;
  double X2 = 0.0;
  double total = 0.0;
  double totalminus = 0.0;
  int m = xy.nrow();
  for(int j = 0; j<m; j++){
    tmp = xy.row(i) - xy.row(j) + alpha*cn;
    X2 = sum(tmp*tmp);
    total = radius[i]+radius[j];
    totalminus = fabs(radius[i]-radius[j]);
    if(ED(i,j)>= total  && sqrt(X2)>= total){
      continue;
    }
    else if(ED(i,j)<= totalminus && sqrt(X2)<= totalminus){
      continue;
    }
    else{
      gammar += (lambda*X2 - ED(i,j)*ED(i,j)) * (lambda*X2 - ED(i,j)*ED(i,j));
      deltaloss1 += 4.0*lambda*(lambda*X2 - ED(i,j)*ED(i,j))*(xy.row(i)[0] - xy.row(j)[0]+alpha*cn[0]);
      deltaloss2 += 4.0*lambda*(lambda*X2 - ED(i,j)*ED(i,j))*(xy.row(i)[1] - xy.row(j)[1]+alpha*cn[1]);
      if(ThreeD == true){
        deltaloss3 += 4.0*lambda*(lambda*X2 - ED(i,j)*ED(i,j))*(xy.row(i)[2] - xy.row(j)[2]+alpha*cn[2] );}
    }

  }
  deltaloss[0] = deltaloss1;
  deltaloss[1] = deltaloss2;
  alphadelta = deltaloss1*cn[0]+deltaloss2*cn[1];
  if(ThreeD == true){
    deltaloss[2] = deltaloss3;
    alphadelta = deltaloss1*cn[0]+deltaloss2*cn[1] + deltaloss3*cn[2];
  }
  return;
}

void Loss(double& loss2,
          double& alphadelta, NumericVector& cn, double& alpha,
          NumericMatrix& xy,NumericVector& radius, double& lambda,
          NumericMatrix& ED, bool ThreeD){
  int m = xy.nrow();
  NumericVector loss1(m);
  NumericVector deltaloss(xy.ncol());
  NumericVector tmp(xy.ncol());
  for(int j = 0; j<m; j++){
    //loss1[j] = loss(m = m, xy = xy ,j , radius = radius, ED = ED , ThreeD = ThreeD);
    deltaloss = xy.row(j);
    loss(loss1[j], deltaloss, alphadelta,  cn, alpha,xy, j,lambda, radius, ED, ThreeD, tmp);

  }
  loss2 = sum(loss1);
  return;
}
// [[Rcpp::export("lossCpp")]]
List loop_R(NumericMatrix xy, double lambda,
            NumericVector radius, NumericMatrix ED, bool ThreeD, 
            double ToleranceofLoss, int maximumStep, double ToleranceofStepsize, bool proportional, 
            double ALPHA, bool Bool) {
  int m = xy.nrow();
  NumericMatrix xy1(clone(xy));
  NumericMatrix xyn(clone(xy));
  NumericVector deltax0(xy.ncol());
  NumericVector tmp(xy.ncol());
  NumericMatrix sn(clone(xy));
  int xigma = 0;
  double gammar = 0.0;
  double alphadelta = 0.0;
  NumericVector cn(xy.ncol());
  cn[0] = 0;cn[1] = 0;
  if(ThreeD){cn[2] = 0;}
  double alpha = 0.0;
  double betan = 0.0;
  double betan1 = 0.0;
  double tt = 0.0;
  double ts = 0.0;
  double gam = 0.0;
  double alpha0 = 1.0;
  double alpha1 = 1.0;
  NumericVector det(xy.ncol());
  for(int i=0;i<m;i++){
    loss(gammar, deltax0, alphadelta,cn, alpha, xy1, i, lambda,radius, ED, ThreeD, tmp);
    deltax0 = -deltax0;
    if(Bool){
      for(int k = 0; k<maximumStep; k++ ){
        loss(gam, det, alphadelta, deltax0 ,alpha0, xy1, i,lambda, radius, ED, ThreeD, tmp);
        alpha1 = alpha0 - gam/alphadelta;
        if(fabs(gam/alphadelta) < ToleranceofStepsize){
          if(alpha1<0){
            alpha1 = ToleranceofStepsize;
          }
          break;
        }
        else if(std::isnan(alpha1) || std::isinf(alpha1) || k == (maximumStep-1)){
          alpha1 = ToleranceofStepsize;
          break;}
        else{ alpha0 = alpha1;}
      }
      xy1.row(i) = xy.row(i) + alpha1*deltax0;
    } else {
      xy1.row(i) = xy.row(i) + ALPHA*deltax0;
    }
  }
  double f1 = 0.0;
  double f2 = 0.0;
  Loss(f1, alphadelta,cn, alpha, xy1, radius,lambda,ED, ThreeD);
  NumericVector deltaxn(xy.ncol());
  NumericVector deltaxn_1(xy.ncol());
  while(f1 > ToleranceofLoss){
    if(xigma == 0){
      for (int i=0;i<m;i++){
        loss(gammar, deltaxn,alphadelta,cn, alpha,xy1, i, lambda,radius, ED, ThreeD, tmp);
        loss(gammar, deltaxn_1,alphadelta,cn, alpha,xy, i, lambda, radius,ED, ThreeD, tmp);
        deltaxn = -deltaxn;
        deltaxn_1 = -deltaxn_1;
        ts = sum(deltaxn*(deltaxn-deltaxn_1));
        tt = sum(deltaxn_1*deltaxn_1);
        betan = ts/tt;
        if(std::isnan(betan) || std::isinf(betan)){betan1 = 0;}
        else if(betan>0){betan1 = betan;}
        else{betan1 = 0;}
        sn.row(i) = deltaxn + betan1*deltaxn_1;
        NumericVector xn = sn.row(i);
        if(Bool){
          for(int k = 0; k<maximumStep; k++ ){
            loss(gam, det, alphadelta, xn ,alpha0, xy1, i,lambda, radius, ED, ThreeD, tmp);
            alpha1 = alpha0 - gam/alphadelta;
            if(fabs(gam/alphadelta) < ToleranceofStepsize ){
              if(alpha1<0){
                alpha1 = ToleranceofStepsize;
              }
              break;
            }
            else if(std::isnan(alpha1) || std::isinf(alpha1) || k == (maximumStep-1) ){
              alpha1 = ToleranceofStepsize;
              break;}
            else{
              alpha0 = alpha1;}
          }
          xyn.row(i) = xy1.row(i) + alpha1*sn.row(i);
        } else {
          xyn.row(i) = xy1.row(i) + ALPHA*sn.row(i);
        }
      }
      xy = xy1;
      xy1 = xyn;
    }
    else{
      NumericMatrix xy11(clone(xyn));
      for (int i=0;i<m;i++){
        loss(gammar, deltaxn,alphadelta,cn, alpha, xy11, i, lambda,radius, ED, ThreeD, tmp);
        loss(gammar, deltaxn_1, alphadelta,cn, alpha, xy, i, lambda,radius, ED, ThreeD, tmp);
        deltaxn = -deltaxn;
        deltaxn_1 = -deltaxn_1;
        ts = sum(deltaxn*(deltaxn-deltaxn_1));
        tt = sum(deltaxn_1*deltaxn_1);
        betan = ts/tt;
        if(std::isnan(betan) || std::isinf(betan)){betan1 = 0;}
        else if(betan>0){betan1 = betan;}
        else{betan1 = 0;}
        sn.row(i) = deltaxn + betan1*sn.row(i);
        NumericVector xn = sn.row(i);
        if (Bool) {
          for(int k = 0; k<maximumStep; k++ ){
            loss(gam, det, alphadelta, xn ,alpha0, xy1, i,lambda, radius, ED, ThreeD, tmp);
            alpha1 = alpha0 - gam/alphadelta;
            if(fabs(gam/alphadelta)<ToleranceofStepsize){
              if(alpha1<0){
                alpha1 = ToleranceofStepsize;
              }
              break;
            }
            else if(std::isnan(alpha1) || std::isinf(alpha1) || k == (maximumStep-1)){
              alpha1 = ToleranceofStepsize;
              break;}
            else{ alpha0 = alpha1;}
          }
          xyn.row(i) = xy11.row(i) + alpha1*sn.row(i);
        }else {
          xyn.row(i) = xy11.row(i) + ALPHA*sn.row(i); 
        }
      }
      Loss(f1, alphadelta,cn, alpha, xyn, radius,lambda,ED, ThreeD);
      Loss(f2, alphadelta,cn, alpha, xy11, radius,lambda,ED, ThreeD);
      if(proportional){
        if(f1>f2 || fabs(f1 - f2)/f2 < ToleranceofLoss){break;} 
      } else {
        if(f1>f2 || fabs(f1 - f2) < ToleranceofLoss){break;} 
      }
      NumericMatrix xy1(clone(xy11));
      xy = xy1;
    }
    xigma++;
  }
  return List::create(Named("xy") = xyn,
                      Named("loss") = f1);
}

void transR(NumericVector& xyvec,NumericMatrix& xy, NumericVector& radius,
            double& radiusvec, NumericVector& radiusall) {
  bool out = true;
  while (out==true) {
    xyvec[0] = R::runif(min(xy.column(0))-2*max(radiusall),max(xy.column(0))+2*max(radiusall));
    xyvec[1] = R::runif(min(xy.column(1))-2*max(radiusall),max(xy.column(1))+2*max(radiusall));
    if(xy.ncol()==3){
      xyvec[2] = R::runif(min(xy.column(2))-2*max(radiusall),max(xy.column(2))+2*max(radiusall));
    }
    NumericVector ed(xy.nrow());
    NumericVector ra(xy.nrow());
    for(int i = 0; i < xy.nrow(); i++){
      if(xy.ncol()==2){
        ed[i] = std::pow((xy.row(i)[0] - xyvec[0]),2)+std::pow((xy.row(i)[1] - xyvec[1]),2);
      }else{
        ed[i] = std::pow((xy.row(i)[1] - xyvec[1]),2)+std::pow((xy.row(i)[2] - xyvec[2]),2)+std::pow((xy.row(i)[0] - xyvec[0]),2);
      }
      ra[i] = radius[i] + radiusvec;

    }
    if(is_true(all(ed>ra))){out = false;}else{out = true;}
  }
  return;
}

// [[Rcpp::export("transCpp")]]
NumericVector trans_R(NumericMatrix xy, NumericVector radius, double radiusvec,NumericVector radiusall){
  NumericVector xyvec(xy.ncol());
  transR(xyvec,xy,radius,radiusvec,radiusall);
  return xyvec;
}


void alldis(int& out, NumericMatrix& xy1,NumericMatrix& xy2, NumericVector& radius1,
            NumericVector& radius2, double& delta){
  NumericVector ed(xy2.nrow());
  NumericVector r1(xy2.nrow());
  NumericVector r2(xy2.nrow());
  NumericMatrix xy(clone(xy1));
  NumericMatrix transxy(clone(xy2));
  out = 1;
  for(int i=0;i<xy.nrow();i++){
    for(int j=0; j<transxy.nrow();j++){
      if(xy.ncol()==2){
        ed[j] = sqrt(std::pow((xy.row(i)[0] - transxy.row(j)[0]),2)+std::pow((xy.row(i)[1] - transxy.row(j)[1]),2));
      }else{
        ed[j] = sqrt(std::pow((xy.row(i)[0] - transxy.row(j)[0]),2)+std::pow((xy.row(i)[1] - transxy.row(j)[1]),2)+std::pow((xy.row(i)[2] - transxy.row(j)[2]),2));
      }
      NumericMatrix xy(clone(xy1));
      NumericMatrix transxy(clone(xy2));
      r1[j] = radius1[i] + radius2[j]+2*delta;
      r2[j] = radius1[i] + radius2[j]+delta;
    }
    if(is_true(all(ed > r1))){continue;}
    else if(is_true(any(ed <= r1))){
      if(is_true(all(ed > r2))){out = 2;}
      else{out = 0;break;}
    }
  }
  return;
}

// [[Rcpp::export("allDisjointCpp")]]
int alldis_R(NumericMatrix xy1,NumericMatrix xy2, NumericVector radius1, NumericVector radius2, double delta){
  int out;
  alldis(out,xy1,xy2,radius1,radius2,delta);
  return out;
}

// [[Rcpp::export("closeCpp")]]
List close_R(NumericMatrix xy1,NumericMatrix xy2, NumericVector radius1,
             NumericVector radius2, double delta, NumericVector direc){
  int out;
  NumericMatrix xy3(clone(xy2));
  alldis(out, xy1,xy3,radius1,radius2,delta);
  while(out!=2){
    for(int i=0; i<xy3.nrow();i++){
      xy3.row(i)[0] = xy3.row(i)[0]+direc[0];
      xy3.row(i)[1] = xy3.row(i)[1]+direc[1];
      if(xy3.ncol()==3){
        xy3.row(i)[2] = xy3.row(i)[2]+direc[2];
      }
    }
    alldis(out, xy1,xy3,radius1,radius2,delta);
  }
  return List::create(Named("out")=out,
                      Named("xy") = xy3);
}

// [[Rcpp::export]]
NumericMatrix binaryIndexCpp(NumericMatrix M, NumericMatrix xy, NumericVector radius,
                          int k ,double yuan, double xuan, int num){
  k = k-1;
  for(int i=0 ; i<num; i++){
    for(int j=0 ; j<num; j++){
      double detect = ((i+1)*xuan+min(xy.column(0))-max(radius) - xy.row(k)[0]) *
        ((i+1)*xuan+min(xy.column(0))-max(radius) - xy.row(k)[0]) +
        ((j+1)*yuan+min(xy.column(1))-max(radius) - xy.row(k)[1]) *
        ((j+1)*yuan+min(xy.column(1))-max(radius) - xy.row(k)[1]);

      if(detect <= radius[k]*radius[k]){
        M(i,j) = 1;
      }
    }
  }
  return M;
}

// [[Rcpp::export]]
NumericMatrix goThroughPixelCpp(Rcpp::List myList, int m, int num) {
  NumericMatrix M(num*num,m);
  for(int i=0;i<num;i++){
    for(int j=0;j<num;j++){
      NumericVector L(m);
      for(int k=0; k<m; k++){
        NumericMatrix N = myList[k];
        L[k] = N(i,j);
      }
      M.row(num*i+j) = L;
    }
  }
  return M;
}


// [[Rcpp::export]]
NumericVector countCpp(NumericMatrix M, NumericMatrix Me){
  NumericVector Len(Me.nrow());
  for(int i=0;i<Me.nrow();i++){
    int k = 0;
    for(int j = 0;j<M.nrow();j++){
      if(is_true(all(Me.row(i)==M.row(j)))){
        k++;
      }
    }
    Len[i] = k;
  }
  return Len;
}

// [[Rcpp::export]]
NumericVector getRidofZeroCpp(NumericMatrix M) {
  int m = M.nrow();
  NumericVector n(m);
  for(int i=0;i<m;i++){
    if(sum(M.row(i))!=0){
      n[i] = i+1;
    }
  }
  return n;
}

// [[Rcpp::export]]
List binaryIndexThreeDCpp(Rcpp::List myList, NumericMatrix xy, NumericVector radius,
                       int k ,double yuan, double xuan, double zuan, int num){
  k = k-1;
  for(int l=0; l<num; l++){
    NumericMatrix M = myList[l];
    for(int i=0 ; i<num; i++){
      for(int j=0 ; j<num; j++){
        double detect = ((i+1)*xuan+min(xy.column(0))-max(radius) - xy.row(k)[0]) *
          ((i+1)*xuan+min(xy.column(0))-max(radius) - xy.row(k)[0]) +
          ((j+1)*yuan+min(xy.column(1))-max(radius) - xy.row(k)[1]) *
          ((j+1)*yuan+min(xy.column(1))-max(radius) - xy.row(k)[1]) +
          ((l+1)*zuan+min(xy.column(2))-max(radius) - xy.row(k)[2]) *
          ((l+1)*zuan+min(xy.column(2))-max(radius) - xy.row(k)[2]);
        if(detect <= radius[k]*radius[k]){
          M(i,j) = 1;
        }
      }
    }
  }
  return myList;
}
// [[Rcpp::export]]
NumericMatrix goThroughPixelThreeDCpp(Rcpp::List list, int m, int num) {
  NumericMatrix M(num*num*num,m);
  for(int l=0;l<num;l++){
    for(int i=0;i<num;i++){
      for(int j=0;j<num;j++){
        NumericVector L(m);
        for(int k=0; k<m; k++){
          List ThreeDList = list[k];
          NumericMatrix N = ThreeDList[l];
          L[k] = N(i,j);
        }
        M.row(l*num*num+num*i+j) = L;
      }
    }
  }
  return M;
}

// [[Rcpp::export]]
bool allConnectedCpp(NumericMatrix xy, NumericVector radius, bool ThreeD){
  int m = xy.nrow();
  bool boolean = true;
  for(int i = 0; i< m; i++){
    NumericVector out(m, false);
    for(int j = 0; j < m;j++){
      if(i == j){continue;}
      double dsquare;
      if(ThreeD){
        dsquare = (xy.row(i)[0] - xy.row(j)[0])*(xy.row(i)[0] - xy.row(j)[0])+
          (xy.row(i)[1] - xy.row(j)[1])*(xy.row(i)[1] - xy.row(j)[1])+
          (xy.row(i)[2] - xy.row(j)[2])*(xy.row(i)[2] - xy.row(j)[2]);
      }else{
        dsquare = (xy.row(i)[0] - xy.row(j)[0])*(xy.row(i)[0] - xy.row(j)[0])+
                  (xy.row(i)[1] - xy.row(j)[1])*(xy.row(i)[1] - xy.row(j)[1]);
      }
      if(dsquare  <= (radius[i] +radius[j])*(radius[i] +radius[j]) ){
        out[j] = true;
        break;
      }
    }
    if(is_true(all(out == false )) ){
      boolean = false;
      break;
    }
  }
  return(boolean);
}

//[[Rcpp::export]]
NumericMatrix distanceCpp(double r1, double r2, NumericVector theta1, NumericVector theta2,
                   double S, bool ThreeD){
  int n1 = theta1.size();
  int n2 = theta2.size();
  NumericMatrix R(n1,n2);
  double pi = 3.14159265;
  double f1;
  double f2;
  for(int i=0; i<n1; i++){
    for(int j=0; j<n2; j++){
      if(ThreeD){
        f1 = pi/3*r1*r1*r1*(1-cos(theta1[i]))*(1-cos(theta1[i]))*(2+cos(theta1[i]))+
             pi/3*r2*r2*r2*(1-cos(theta2[j]))*(1-cos(theta2[j]))*(2+cos(theta2[j])) - S;
        f2 = r1*sin(theta1[i])-r2*sin(theta2[j]);
      } else {
        f1 = theta1[i]*r1*r1 - sin(2*theta1[i])*r1*r1/2+
             theta2[j]*r2*r2 - sin(2*theta2[j])*r2*r2/2-S;
        f2 = r1*sin(theta1[i])-r2*sin(theta2[j]);
      }
      R(i,j) = fabs(f1)+fabs(f2);
    }
  }
  return(R);
}

//[[Rcpp::export]]
bool BoolScaleNMCpp(bool proportional, double value, NumericVector LAMBDA, NumericVector STRESS){
  bool B = false;
  if(proportional == false){
    if(((max(LAMBDA) - min(LAMBDA)) > value) && (max(STRESS) - min(STRESS)) > value ) {
      B = true; 
    }
  } else {
    if(((max(LAMBDA) - min(LAMBDA))/max(LAMBDA) > value) && (max(STRESS) - min(STRESS))/max(STRESS) > value) {
      B = true; 
    }
  }
  return(B);
}

//[[Rcpp::export]]
bool BoolScaleLCpp(bool proportional, double value, double stress_n, double stress){
  bool B = false;
  if(proportional == false){
    if( fabs(stress_n - stress)< value ) {
      B = true; 
    }
  } else {
    if( fabs(stress_n - stress)/stress < value) {
      B = true; 
    }
  }
  return(B);
}

//[[Rcpp::export]]
bool BoolDistanceCpp(bool proportional, double value, double f1, double f2, NumericMatrix thetanew, NumericMatrix theta){
  bool B = false;
  double N1,N2;
  N1 = thetanew(0,0) - theta(0,0);
  N2 = thetanew(1,0) - theta(1,0);
  if(proportional== false){
    if( fabs(f1) + fabs(f2) > value || fabs(N1+N2) > value) {
      B = true; 
    }
  } else {
    if( fabs(f1) + fabs(f2) > value || fabs(N1+N2)/(theta(0,0)+theta(1,0)) > value) {
      B = true; 
    }
  }
  return(B);
}