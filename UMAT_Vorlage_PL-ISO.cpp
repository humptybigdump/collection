#include <iostream>
#include <cmath>
using namespace std;

extern "C" void umat_(double *stress, double *statev, double *ddsdde, double *sse, double *spd,
		double *scd, double *rpl, double *ddsddt, double *drplde, double *drpldt,
		double *stran, double *dstran, double *time, double *dtime, double *temp,
		double *dtemp, double *predef, double *dpred, char *cmname, int *ndi, int *nshr,
		int *ntens, int *nstatv, double *props, int *nprops, double *coords, double *drot,
		double *pnewdt, double *celent, double *dfgrd0, double *dfgrd1, int *noel, int *npt,
		int *layer, int *kspt, int *kstep, int *kinc, short cmname_len){

// material properties
	double E = 1.*props[0]; 			//E-Modulus
	double NU = 1.*props[1]; 			//
	double G = E/2.0/(1.0 + NU); 			// shear modulus, 2nd lame constant
	double lambda = 2.0*G*NU/(1.0 - 2.0*NU); // 1st lame constant
	double h = 1.*props[2];  		// 
	double sigmaF0 = 1.*props[3]; 	//initial yield stress
	double theta0 = 1.*props[4]; 		// 
	double thetaInf = 1.*props[5];         // 
	double sigmaFInf = 1.*props[6]; 	//maximum yield Stress

// state variables
	double q = 1.*statev[0]; 		// accumulated plastic strain, epsilon_pv in document
	double stranP[6]; 			// plastic strain
	for (int i = 0; i<6; i++){
		stranP[i] = 1.*statev[i+1];
	}
	
// user defined variables
	double stranFin [6];	//updated strain tensor
	double stressTrPr [6]; //stress deviator
	double normSTr = 0; // norm of stress deviator
	double NDev [6]; // normalized stress deviator (stressTrPr/normSTr)
	double spStranFin = 0; //trace of updated strain tensor
	double sphstranFinEntry = 0; // diagonal entries of spherical strain tensor
	bool yield = false; // if material yields = true
	double sigmaF; // yield stress
	double sigmaFPrime; // time derivative of yield stress
	double f; // yield criterion, phi in document
	double sqrt23 = sqrt(2./3.); //square root of 2/3, needed multiple times throughout the algorithm
	int hardening = 1; // 0: linear hardening, 1: nonlinear hardening
	
// variables for newton scheme
	int iter = 0; // iteration counter for newton
	int maxIter = 20; //max iterations for newton
	double res = f; // residual
	double dGamma = 0.; // delta(stranFinilon_pv), difference of plastic comparative strain between time ststranFin
	double ddGamma = 0.; // delta(delta(stranFinilon_pv))
	double tol = 1e-10; // tolerance for residual after newton

// update strain
	
	
// trace of strain tensor
	
// trial stress, it's deviator and norm
	
//	compute yield Stress
	switch(hardening){
		case 0:
			sigmaF = sigmaF0+h*q;
			break;
		case 1:
			sigmaF = sigmaF0 + (sigmaFInf-sigmaF0) * (1.0-exp(-(h0-hInf)*q*1./(sigmaFInf-sigmaF0)))+hInf*q;
			break;
	}

//	isotropic elastic tangent
	for(int i = 0; i < 3; i++){
		ddsdde[6*i+i] = 2.0*G;
		ddsdde[6*(i+3)+(i+3)] = G;
		for(int j = 0; j < 3; j++){
			ddsdde[6*i+j] += lambda;
		}
	}

//	yield criterion
	f = normSTr - sqrt23*sigmaF;
	if( f < 0){
		if(yield == false){ // no yield

			for(int i = 0; i < 3; i++){
				stress[i]   = 0;
				stress[i+3] = 0;
			}
		}
	}
	else{ // yield

		// newton
		while(abs(res)>tol && iter < maxIter){
			//	compute sigmaFPrime
			switch(hardening){
				case 0:
					sigmaFPrime = h;
					break;
				case 1:
					sigmaFPrime = (h0-hInf) * exp(-(h0-hInf)*q/(sigmaFInf-sigmaF0)) + hInf;
					break;
			}
			ddGamma = 0;  // Newton increment
			dGamma += 0;  // update delta gamma
			q += 0;       // update accumulated plastic strain
			
			// compute yield stress
			switch(hardening){
				case 0:
					sigmaF = sigmaF0+h*q;
					break;
				case 1:
//					sigmaF = sigmaF0 + (sigmaFInf-sigmaF0) * ( 1.0 - exp(-(h0-hInf)*q*1.0/(sigmaFInf-sigmaF0)) )+hInf*q;
					break;
			}
			res = 0; // update residual
			iter += 1; // increase iteration counter
			
		};
		
		// check newton for convergence
		if(abs(res) > tol){
			cout << "UMAT_WARNING: NEWTON NOT CONVERGED! Iterations: " << iter << " Residual: " << res << endl;
		}
		
		// calculate normalized stress deviator
		for(int i = 0; i < 6; i++){
			NDev[i] = stressTrPr[i]/normSTr;
		}
		
		// update plastic strain, keeping ABAQUS notation in mind
		for(int i = 0; i < 3; i++){
			stranP[i] += dGamma*NDev[i];
			stranP[i+3] += 2.*dGamma*NDev[i+3];
		}
		// update stress
		
		for(int i = 0; i < 3; i++){
			stress[i]   = 0;
			stress[i+3] = 0;
		}
		
		// algorithmically consistent tangent
		// P1 part
		
		// P2 part
		
		// N dyadic N part
		
	}
// symmetrize tangent

	for(int i = 0; i < 6; i++){
		for(int j = 0; j < i; j++){
			ddsdde[6*i+j] = ddsdde[6*j+i];
		}
	}

//	specific elastic strain energy
	*spd = sigmaF0*dGamma;  // specific plastic dissipation
	*scd = 0.;              // specific creep dissipation
	*sse = 0.;              // specific strain energy
	for(int i = 0; i < 6; i++){
		*sse += 0.5*stress[i]*(stranFin[i]-stranP[i]);
	}
	
	// update state variables
	statev[0] = q;
	for(int i = 0; i < 6; i++){
		statev[i+1] = stranP[i];
	}
}







