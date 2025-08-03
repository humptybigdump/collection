function [p,q] = svHarmonicOscillator(p,q,tau,w)
% Apply one step of Störmer Verlet on the Harmonic oscillator

p = p - tau/2 * w^2*q;
q = q + tau * p;
p = p - tau/2 * w^2*q;

end