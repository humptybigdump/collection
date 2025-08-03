function [ distance ] = euclidean_distance(u, v)
    distance_vector = u - v;
    distance = euclidean_length(distance_vector);
end

