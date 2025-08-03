% Aufgabe 1: Rotationsmatrizen

R = [0.36,0.48,-0.8; -0.8,0.6,0; 0.48,0.64,0.6]

p = SO3(R)

p.torpy()
p.toeul()
p.toangvec()

p.UnitQuaternion()


% Aufgabe 2: Homogene Transformationsmatrizen

T = [0,0,1,5; 0,1,0,0; -1,0,0,0; 0,0,0,1]
v = [1; 2; 3; 1]

p = SE3(T)

p.SO3.torpy('deg')
p.t


% Aufgabe 3: Transformationsmatrizen

T0 = [1,0,0,5; 0,1,0,3; 0,0,1,0; 0,0,0,1]
T01 = trotz(90, 'deg')
T12 = transl(4, 0, 0)
T23 = transl(2, 3, 0) * trotz(-45, 'deg')

T0 * T01 * T12 * T23

% Aufgabe 4: Translations- und Rotationsdifferenz

Ttcp = [0,-1,0,1; 1,0,0,2; 0,0,1,3; 0,0,0,1]
ptcp = SE3(Ttcp)

Tgoal = [0,0,-1,7; 0,1,0,6; 1,0,0,5; 0,0,0,1]
pgoal = SE3(Tgoal)

dt = pgoal.t - ptcp.t
norm(dt)

Rdiff = ptcp.SO3.inv * pgoal.SO3
Rdiff.toangvec('deg')


% Aufgabe 5: Quaternionen

p = [5,1,7]
v = Quaternion(0, p)

phi = 90
a = [0,0,1]
q = UnitQuaternion.angvec(phi/180*pi, a)

qc = q.conj()

vr = q * v * qc
vr.v

% SLERP
a1 = [1,0,0]
q1 = UnitQuaternion.angvec(pi, a1)
a2 = [0,1,0]
q2 = UnitQuaternion.angvec(pi, a2)

q2.interp(q1, 0.5)
