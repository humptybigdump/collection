% Depending on the boundary labels we have to put the data on the inner
% boundary together.
% e(5,:) indicates to which boundary the edges belong
all_of_e = e(5,:);
% already ordered
all_edge = [];
for ii = [6,7,8,5]%[7,8,5,6]%[8,9,6,7]
    % only on the boundary ii
    nr_ii = all_of_e==ii;
    only_ii_e = e(:,nr_ii);
    % sort them in a counter clockwise order(when looking at the domain)
    [a,b] = sort(only_ii_e(3,:));
    % put 'em together
    all_edge = [all_edge, only_ii_e(:,b)];
end
