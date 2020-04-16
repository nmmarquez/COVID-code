#include <TMB.hpp>
#include <Eigen/Sparse>
#include <vector>
using namespace density;
using Eigen::SparseMatrix;

template<class Type>
Type objective_function<Type>::operator() (){
    
    DATA_VECTOR(y);
    
    PARAMETER_VECTOR(phi);
    PARAMETER(log_sigma);
    // printf("%s\n", "Parameters set.");
    // 
    Type sigma = exp(log_sigma);
    // 
    int N = y.size();        // number of locations
    int M = phi.size();        // number of ages
    // 
    // printf("%s\n", "Transform parameters.");
    // 
    // // Initiate log likelihood
    Type nll = Type(0.0);
    
    // Probability of random effects
    for(int m = 1; m < M; m++) {
        nll -= dnorm(phi[m], phi[m-1], sigma, true);
    }
    // printf("%s\n", "Random Effects evaled.");
    // 
    for(int n = 1; n < N; n++) {
        if(y[n] >= y[n-1]){
            nll -= dpois(y[n]-y[n-1], y[n-1] * exp(phi[n]), true);
        }
    }
    // 
    // printf("%s\n", "Eval likelihood.");
    // 
    // REPORT(phi);
    
    return nll;
}
