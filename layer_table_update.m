function handles = layer_table_update(handles)
laynum = length(handles.uitable_layer_system.Data(:,1));
wl = handles.slider_wavelength_select.Value;
for jl=1:laynum
    if handles.uitable_layer_system.Data{jl,5}
        handles.uitable_layer_system.Data{jl,3}=interp1(handles.nkRawdata{jl}(:,1),handles.nkRawdata{jl}(:,2),wl);
        if length(handles.nkRawdata{jl}(1,:))==3
            handles.uitable_layer_system.Data{jl,4}=interp1(handles.nkRawdata{jl}(:,1),handles.nkRawdata{jl}(:,3),wl);
        end
    end
    if handles.uitable_layer_system.Data{jl,7}
        handles.uitable_layer_system.Data{jl,4}=0;
    end
end

handles.uitable_layer_system.Data{1,2}=Inf;
handles.uitable_layer_system.Data{laynum,2}=Inf;
    
handles=plotnk(handles);