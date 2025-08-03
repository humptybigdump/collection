//Loesen eines LGS mit einem iterativen Verfahren:
//Gauss-Seidel Verfahren (auch genannt Einzelschrittverfahren):
#include<iostream>
#include<math.h>
using namespace std;
int N;

int ZSK(double **A, int N){		//Ueberpruefung ob Zeilensummenkriterium verletzt wird
	double s;
	int i, k, q;
	q=1;
	i=0;
	while ((i<=N-1) && (q==1)){
		s=0;
		for (k=0;k<=N-1;k++){
			if (k!=i) s=s+fabs(A[i][k]);
		}
		if (fabs(A[i][i])>s) q=1;
		else q=0;
		i++;
	}
	return q;
}

int ABK(double *x, double *y,double eps, int N){	//Abbruchkriterium
	int i, res;
	double w, max;
	max=-1;
	for (i=0;i<=N-1;i++){
		w=fabs(y[i]-x[i]);
		if (w>max) max=w;
	}	
	if (max<eps) res=1;
	else res=0;
	return res;
}

void Einzelschritt(double **A, double *b, double *xalt, double *xneu, int N){	//Iterationsvorschrift:
	int i, j;
	double s1, s2;
	for (i=0;i<=N-1;i++){
		s1=0;
		for (j=0;j<=i-1;j++) s1+=A[i][j]*xneu[j];
		s2=0;
		for (j=i+1;j<=N-1;j++) s2+=A[i][j]*xalt[j];
		xneu[i]=(b[i]-s1-s2)/A[i][i];
	}
}

void Liesvektor(double *b, int N){
	int i;
	cout << "Bitte den Vektor eingeben: "<< endl;
	for (i=0;i<=N-1;i++)
	cin>>b[i];
}	


void Liesmatrix(double **A, int N){
	int i, j;
	cout << "Bitte die Matrix eingeben (zeilenweise): "<< endl;
	for (i=0;i<=N-1;i++){
		for (j=0;j<=N-1;j++){
		cin>>A[i][j];
	}
	}
}


void drucke_Matrix(double **A,int N){
	int i, j;
	cout << "die Matrix lautet: "<< endl;
	for (i=0;i<=N-1;i++){
		for (j=0;j<=N-1;j++){ 
		cout<<" "<<A[i][j];}
		cout<<endl;
	}
}


void drucke_rvektor(double *b, int N){
	int i;
	for (i=0;i<=N-1;i++) cout<<b[i]<<" ";
	cout<<endl;
}



int main(){
	int N;
	double **A;
	double *xalt, *xneu, *b;
	int i, j, zehn, err,l;
	double eps;
	cout << "Bitte die Dimension und die Genauigkeit eingeben "<< endl;
	cin>>N;
	cin>>eps;
	
	//Speicher allokieren:
	A=new double* [N];
	for (i=0;i<=N-1;i++) { A[i]=new double[N];}
	xalt=new double[N];
	xneu=new double[N];
	b=new double[N];
	
	for (j=1;j<=2;j++){
		Liesmatrix (A,N);
		Liesvektor (b,N);
		drucke_Matrix(A,N);
		cout<<endl;
		
		cout<<" Rechte Seite b:"<<endl;
		drucke_rvektor(b,N);
		cout<<endl;
		
		if (ZSK(A,N)==1) zehn=0;
		else zehn=1;
		
		for (i=0;i<=N-1;i++) xneu[i]=b[i]/A[i][i];
		if (zehn==1) cout<<"Kriterium verletzt!";
		cout<<endl;
		
		cout<<" Startvektor :"<<endl;
		drucke_rvektor(xneu,N);
		
		for (l=0;l<=N-1;l++) {
			xalt[l]=xneu[l];
		}
		
		i=0;
		xalt[0]=xalt[0]+1;
		while (((zehn==0)||(i<10)) && ((zehn==1)||(ABK(xalt,xneu,eps,N)==0))){
			for (l=0;l<=N-1;l++) xalt[l]=xneu[l];
			i++;
			Einzelschritt(A,b,xalt,xneu,N);
			if (zehn==1){
				cout<<i<<"ter Vektor : "<<endl;
				drucke_rvektor(xneu,N);
			}
		}
		
		//wenn die Genauigkeit erreicht wurde:
		if (zehn==0){
			cout<<i<<"ter und letzter Vektor : "<<endl;
			drucke_rvektor(xneu,N);
		}
	}
	
	cout<<endl;

	//Speicher wieder freigeben
	delete [] xalt;
	delete [] xneu;
	delete [] b;
	for (i=0;i<=N-1;i++) {delete [] A[i];}
	delete [] A;
	return 0;
}
