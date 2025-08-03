import numpy as np
class source:
  def __init__(self, location, typeofsource, magnitude,f0,nt,dt,Mxx=0,Mzz=0,Mxz=0,Mzx=0,F_x=0,F_z=0,inciangle=0):
    self.location = location
    self.magnitude = magnitude
    self.typeofsource = typeofsource
    self.f0=f0
    self.dt=dt
    self.nt=nt
    self.Mxx=Mxx
    self.Mzz=Mzz
    self.Mxz=Mxz
    self.Mzx=Mzx
    self.F_x=F_x
    self.F_z=F_z
    self.f0=f0
    self.inciangle=inciangle
  def singleforce(self):
    F_z=self.F_z*self.magnitude*self.sourcetf()
    F_x=self.F_x*self.magnitude*self.sourcetf()
    #print('Here is the single force')
    return F_x,F_z
  def moment(self):
    Mxx=self.Mxx*self.magnitude*self.sourcetf()
    Mxz=self.Mxz*self.magnitude*self.sourcetf()
    Mzz=self.Mzz*self.magnitude*self.sourcetf()
    Mzx=self.Mzx*self.magnitude*self.sourcetf()
    #print('Here is the moment tensor source')
    return Mxx,Mxz,Mzz,Mzx
  def sourcetf(self):
    t = (np.arange(self.nt) * self.dt)                             # initialize time axis
    T0 = 1. / self.f0                                           # period
    a = 4. / T0                                            # half-width (so called sigma)
    t0 = T0 / self.dt
    tmp = np.zeros(self.nt)
    for it in range(self.nt):
        t = (it - t0) * self.dt
        tmp[it] = -2 * a * t * np.exp(-(a * t) ** 2)       # derivative of Gaussian (so called sigma)
    src = np.zeros(self.nt)                                     # source
    src[0:len(tmp)] = tmp
    return src

    

