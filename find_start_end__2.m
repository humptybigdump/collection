function [start_end] = find_start_end(n, c, pixellist)
    d = n(1) * pixellist(:,1) + n(2) * pixellist(:,2) + c;
    pixellist_proj = pixellist - [n(1) * d, n(2) * d];
    
    [~, min_i] = min(pixellist_proj(:,1));
    [~, max_i] = max(pixellist_proj(:,1));
    
    start_end = [pixellist_proj(min_i, 1), pixellist_proj(min_i, 2);
                 pixellist_proj(max_i, 1), pixellist_proj(max_i, 2)];
end