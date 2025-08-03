/* test code to demonstrate how to debug with multiple         
   instances of e.g. "ddd", one per mpi-process.               
   
   1)  compile with:                                           
      mpif90 -g -O0 testDDDparProgDebugging.cpp

   2) run the executable:                                      
      mpirun.openmpi -np 2 a.out                                 

   3) start debugger, attaching to running processes:          
      ps -aef | grep a.out                                        
      ddd a.out <PID0>                                            
      ddd a.out <PID1>                                            
      
m.uhlmann june 2020 
*/
#include <iostream>
#include <string>
#include <mpi.h>
using namespace std;

void debugMPIMasterInput(int size, int rank);

int main (int argc, char *argv[]) {
  int myrank, size, ierr;
  /* Initialize MPI       */
  ierr=MPI_Init(&argc, &argv);
  /* Get my rank          */
  ierr=MPI_Comm_rank(MPI_COMM_WORLD, &myrank); 
  /* Get the total number of processors */
  ierr=MPI_Comm_size(MPI_COMM_WORLD, &size);
  if (myrank==0){
    cout << "MPI Version: "<<MPI_VERSION<<"."<<MPI_SUBVERSION<<endl;
    cout << myrank<<": # of tasks="<<size<<endl;
  }
  /* wait for user to attach to running process:*/
  debugMPIMasterInput(size, myrank);

  /* ... (arbitrary code to be debugged) ... */
  
  /* Terminate MPI    */
  ierr=MPI_Finalize();                         
  return ierr;
}
void debugMPIMasterInput(int size, int rank){
  /* has the rank-0 process read input from stdin, while others wait
     on a barrier -> allows to attach to a running process      */
  int ierr;
  string sdummy;
  if(rank==0){
    //master: read from stdin
    cout << "[" << rank << "] expecting some input..."<< endl;
    cin >> sdummy;
  }
  //all: wait
  cout << "[" << rank << "] waiting at barrier..." << endl;
  ierr=MPI_Barrier(MPI_COMM_WORLD);
  return;
}
