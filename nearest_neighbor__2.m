function [idx,D] = nearest_neighbor (Q,R)
% function [idx,D] = nearest_neighbor (Q,R)
%
% This function interprets Q and R as two sets of d-dimensional row vectors
% and calculates for each point in Q its closest neighbor in R (with
% respect the the Euclidean distance). It returns in idx for each vector in
% Q the index of the closest neighbor and in D the minimal distance.
%
% Based on an implementation by Yi Cao at Cranfield University on 25 March 2008
% Adapted by Martin Lauer at Karlsruhe Institute of Technology 

[N,M] = size(Q);
L = size(R,1);

% Initialize idx and D
idx = zeros(N,1);
D = idx;

% Loop for each point in Q
for k=1:N
    d = zeros(L,1);
    for t=1:M
        d = d+(R(:,t)-Q(k,t)).^2;  % calculate squared Euclidean distances
    end
    [D(k), idx(k)] = min(d);  % find closest point
end
D = sqrt(D);
end
