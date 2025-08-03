%% Numerische Mathematik
%  Matlab Uebung 2
%  Kate Clintworth, Maike Zoller, Niklas Pfeffer
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc
%clear all
close

%% a) einlesen

[H, a, URL] = load_data("math_kit.dat");


%% b)

% damping constant
alpha = 0.80;


% Potenzmethode
v0 = ones(9843,1);
tol = 10^-6;

eigenvector = power_method(H, a, alpha, v0, tol)

%power_method(H, a, alpha, v0, tol)

%% Ausgabe

[~,idx]=sort(eigenvector,'descend');

ten_highest_idx = idx(1:10);

page_rank_values = eigenvector(ten_highest_idx)';

Urls = pad(URL(ten_highest_idx))';

tmp = [Urls;num2cell(page_rank_values)];

formatSpec = 'Url: %s  Pagerank: %f \n';
fprintf(formatSpec,tmp{:})


%% b) page rank mult
function G_T_v = page_rank_mult(H, a, alpha, v)
    
    n = length(H);
    
    % Seite 34 unten
    %G = alpha * H + alpha * a + ((1-alpha)/n)*ones(n,n);
    
    einser = ones(9843,1);
    
    G = alpha*(H + 1/n * a * einser') + (1-alpha)*1/n * einser * einser';
    
    G_T_v = G'*v;
    % gibt für einen v Wert den betragsgrößten Eigenvektor der Google
    % Matrix = G'v zurück
    % G*v=v
end

%% c) power method, gibt v zurück
function eigenvector = power_method(H, a, alpha, v0, tol)
    for i=1:1000
        
        % normieren
        v0 = v0/norm(v0,1);
        
        % G'*v = v (da Eigenvektor zum Eigenwert 1)
        v = page_rank_mult(H, a, alpha, v0);
        
        err = (norm((v-v0),1));

        % Abbruchbedingung
        fprintf('Error: %d \n', err)
        if err < tol
            break
        end
        
        v0 = v;
    end
    eigenvector = v;
end


%% Funktion zum Einlesen
function [H,a,URL] = load_data(fname)

% [H,a,URL] = load_mv(filename, verbose)
%
% loads a .dat file of webgraph
%
% filename is the file name of the .dat file
%
% output 
%  H is the hyperlink matrix where the diagonal Elements are all zero 
%    (no self links). The data format is sparse.
%
%  a is the vector where a_i is 1 if site i contains no link and otherwise 0
%
%  URL is a cell array of the pages urls (unified resource identifier),
%      which are the 'http://www.domain.de/directory/file.html' like strings.
%
% examples
%  [H,a,URL] = load_mv('na_math_kit.dat');
%  [H,a,URL] = load_mv('math_kit.dat');


file   = fopen(fname,'r');
dim    = fscanf(file,'%d',1); % dimension (number of pages)
nnzero = fscanf(file,'%d',1); % number of nonzero entries (links)
fclose(file);

% read data block of urls
[URL]   = textread(fname,'%*d%s',dim,'headerlines',1);
% read data block of web graph (adjacence matrix)
[ii,jj] = textread(fname,'%d%d', nnzero,'headerlines',1+dim);
% size(ii)
% size(jj)
% nnzero

%create adjacence matrix
% put the date into a sparse matrix format
A       = sparse(ii,jj,ones(nnzero,1),dim,dim);
% remove self linking
A       = A-spdiags(diag(A), 0, dim,dim); 

%Create vector a and hyperlink matrix H  
s = sum(A,2);
a = (s==0);
s = s + a; % to avoid division by zero (0/0)
H = spdiags(1./s, 0, dim,dim) * A; % save memory via loop here
clear('A');


return;
end

