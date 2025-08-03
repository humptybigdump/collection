function handles = find_poles(handles)

min_real_neff = str2double(handles.edit_realneff_start.String);
max_real_neff = str2double(handles.edit_realneff_stop.String);

min_imag_neff = str2double(handles.edit_imagneff_start.String);
max_imag_neff = str2double(handles.edit_imagneff_stop.String);

axes(handles.axes_neff)
set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
hold on

discr_scl = str2double(handles.edit_contour_discretization.String);
nf_cell_size = str2double(handles.edit_min_loop_size.String);

wl=handles.slider_wavelength_select.Value;
d_sweep=handles.thickness_slider.Value;
omega=2*pi/wl;

handles.findmode.rectangles={};

try
    handles.findmode.TE_plot;
    for jpl=1:length(handles.findmode.TE_plot)
        delete(handles.findmode.TE_plot{jpl})
    end
    for jpl=1:length(handles.findmode.TM_plot)
        delete(handles.findmode.TM_plot{jpl})
    end
end
handles.findmode.TE_plot={};
handles.findmode.TM_plot={};

tolerance = str2double(handles.edit_detection_thresh.String);

d = [handles.uitable_layer_system.Data{:,2}]';
d(1)=0;d(end)=0;
n = [handles.uitable_layer_system.Data{:,3}]';
k = [handles.uitable_layer_system.Data{:,4}]';
strc = [d,n+1i*k];


for pol=1:2
    % initialize pole finding structure
    pole_cells{pol}(1).loop = [min_real_neff+1i*min_imag_neff , max_real_neff+1i*max_imag_neff];
    pole_cells{pol}(1).ready = false;
    pole_cells{pol}(1).empty = false;
    pole_cells{pol}(1).kp_loc = [];
    
    lpcnt = 0;
    
    % update plot
    lp = pole_cells{pol}(1).loop; 
    xywh = [ real(lp(1)),imag(lp(1)), real(lp(2)-lp(1)), imag(lp(2)-lp(1))];
    handles.findmode.temp_rectangle = rectangle('Position',xywh, 'FaceColor','white');
    set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
    
    while lpcnt<1e4;
        
        % check if all poles have been found already
        exitflg=true;
        for jc=1:length(pole_cells{pol})
            exitflg = (exitflg & pole_cells{pol}(jc).ready);
        end
        if exitflg
            break
        end
        
        % select cell
        for jc=1:length(pole_cells{pol})
            if ~pole_cells{pol}(jc).ready
                break
            end
        end
        
        % update figure
        lp = pole_cells{pol}(jc).loop; 
        xywh = [ real(lp(1)),imag(lp(1)), real(lp(2)-lp(1)), imag(lp(2)-lp(1))];
        delete(handles.findmode.temp_rectangle);
        handles.findmode.temp_rectangle = rectangle('Position',xywh, 'FaceColor','white');
        drawnow
        set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
        
        % define contour
        re_res = real( pole_cells{pol}(jc).loop(2)-pole_cells{pol}(jc).loop(1) ) / discr_scl;
        im_res = imag( pole_cells{pol}(jc).loop(2)-pole_cells{pol}(jc).loop(1) ) / discr_scl;
        
        re_vec = (0 : re_res : real(pole_cells{pol}(jc).loop(2)-pole_cells{pol}(jc).loop(1)) );
        im_vec = 1i*(0 : im_res : imag(pole_cells{pol}(jc).loop(2)-pole_cells{pol}(jc).loop(1)) );
        
        nf_cntr = [ pole_cells{pol}(jc).loop(1)+re_vec , ...
            pole_cells{pol}(jc).loop(1) + re_vec(end) + im_vec , ...
            pole_cells{pol}(jc).loop(1) + re_vec(end:-1:1) + im_vec(end), ...
            pole_cells{pol}(jc).loop(1) + im_vec(end:-1:1)];
        
        kp_cntr =  nf_cntr*omega;
        
        % get the reflectivity and transition matrices
        S12=kp_cntr-kp_cntr;

        for jk = 1:length(kp_cntr)
            S = layer_S_matrix(pol,strc,kp_cntr(jk),wl);
            S12(jk)=S(1,2);
        end
        
        S12_loop = trapz(kp_cntr,S12);
        pole_loc_kp = trapz(kp_cntr,S12.*kp_cntr)/S12_loop;
        
        if abs(S12_loop) < tolerance  % no pole in loop
            pole_cells{pol}(jc).ready = true;
            pole_cells{pol}(jc).empty = true;
            pole_cells{pol}(jc).kp_loc = [];
        elseif max(re_res,im_res) > nf_cell_size/discr_scl % then divide loop
            if re_res > im_res % divide loop in real direction
                lp1 = [pole_cells{pol}(jc).loop(1), real((pole_cells{pol}(jc).loop(1)+pole_cells{pol}(jc).loop(2))/2) + 1i*imag(pole_cells{pol}(jc).loop(2))];
                lp2 = [real((pole_cells{pol}(jc).loop(1)+pole_cells{pol}(jc).loop(2))/2) + 1i*imag(pole_cells{pol}(jc).loop(1)) , pole_cells{pol}(jc).loop(2)] ;
            else
                lp1 = [pole_cells{pol}(jc).loop(1), real(pole_cells{pol}(jc).loop(2)) + 1i*imag((pole_cells{pol}(jc).loop(1)+pole_cells{pol}(jc).loop(2))/2)];
                lp2 = [real(pole_cells{pol}(jc).loop(1)) + 1i*imag((pole_cells{pol}(jc).loop(1)+pole_cells{pol}(jc).loop(2))/2) , pole_cells{pol}(jc).loop(2)] ;
            end
            new_cell1.loop = lp1;
            new_cell1.ready = false;
            new_cell1.empty = [];
            new_cell1.kp_loc = [];
            
            new_cell2.loop = lp2;
            new_cell2.ready = false;
            new_cell2.empty = [];
            new_cell2.kp_loc = [];
            
            pole_cells{pol} = [pole_cells{pol}(1:jc-1) ,new_cell1,new_cell2, pole_cells{pol}(jc+1:end)];
            
        else  % cell already small, no further division
            if inpolygon(real(pole_loc_kp),imag(pole_loc_kp),real(kp_cntr),imag(kp_cntr)) % check if the pole is really inside the loop (otherwise poles might be found several times)
                pole_cells{pol}(jc).ready=true;
                pole_cells{pol}(jc).empty=false;
                pole_cells{pol}(jc).kp_loc = pole_loc_kp;
                % update figure
                lp = pole_cells{pol}(jc).loop;
                xywh = [ real(lp(1)),imag(lp(1)), real(lp(2)-lp(1)), imag(lp(2)-lp(1))];
                delete(handles.findmode.temp_rectangle);
                if pol==1
                    handles.findmode.TE_plot{end+1}=plot(real(pole_loc_kp)/omega,imag(pole_loc_kp)/omega,'o','Color',handles.settings.colplot(2,:),'LineWidth',1.5);
                    set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
                else
                    handles.findmode.TM_plot{end+1}=plot(real(pole_loc_kp)/omega,imag(pole_loc_kp)/omega,'s','Color',handles.settings.colplot(1,:),'LineWidth',1.5);
                    set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
                end
            else
                % pole is not in loop
                pole_cells{pol}(jc).ready=true;
                pole_cells{pol}(jc).empty=true;
                pole_cells{pol}(jc).kp_loc = [];
            end
        end
    end
    delete(handles.findmode.temp_rectangle);
    drawnow
    set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
end

for pol=1:2
    poles{pol}.kpar = [pole_cells{pol}.kp_loc];
end


% more accurately:
discr_scl = 500;

for pol=1:2
    if isempty(poles{pol}.kpar)
        poles_accurate{pol}.kpar = [];
        poles_accurate{pol}.neff = [];
    end
    for jp =1:length(poles{pol}.kpar)
        % eps = min([omega/100 , imag(poles{pol}.kpar(jp))]);
        eps = omega/100;
        cntr = poles{pol}.kpar(jp) + eps * exp(1i*(0:2*pi/discr_scl:2*pi));
        S12=cntr-cntr;
        for jk = 1:length(cntr)
            S = layer_S_matrix(pol,strc,cntr(jk),wl);
            S12(jk)=S(1,2);
        end
        S12_loop = trapz(cntr,S12);
        pole_loc_kp = trapz(cntr,S12.*cntr)/S12_loop;
        poles_accurate{pol}.kpar(jp) = pole_loc_kp;
        poles_accurate{pol}.neff(jp) = pole_loc_kp/omega;
    end
end

for jr=1:length(handles.findmode.rectangles)
    delete(handles.findmode.rectangles{jr})
end

for jpl=1:length(handles.findmode.TE_plot)
    delete(handles.findmode.TE_plot{jpl})
end
for jpl=1:length(handles.findmode.TM_plot)
    delete(handles.findmode.TM_plot{jpl})
end


handles.poles{round(wl), round(d_sweep)} = poles_accurate;

hold off