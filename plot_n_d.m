function handles = plot_n_d(handles)

%creates the range in which the poles will be plotted
min_real_neff = str2double(handles.edit_realneff_start.String);
max_real_neff = str2double(handles.edit_realneff_stop.String);

% + 100, so that in case there are modes found at the upper limit of the
% sweep, these should also be visible inside the graph, and not just at the
% upper edge
min_d = str2double(handles.thickness_sweep_start.String)-100;
max_d = str2double(handles.thickness_sweep_end.String)+100;


%setting the axes and their properties
axes(handles.axes_thickness_neff)
cla(handles.axes_thickness_neff)
set(gca,'YLim',[min_real_neff,max_real_neff],'XLim',[min_d,max_d]);
hold on
xlabel('thickness')
ylabel('real n_{eff}')


jwl=handles.slider_wavelength_select.Value;
dselect = handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2};
darray = str2num(handles.thickness_sweep_start.String):str2num(handles.thickness_sweep_step.String):str2num(handles.thickness_sweep_end.String);handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2}

for pol=1:2
    for jd=round(darray)
        try
            if pol==1
                if jd==round(dselect)
                    handles.modeposplot_n_d{pol,jwl}=plot(jd,real(handles.poles{jwl,jd}{pol}.neff),'o','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',3);
                else
                    handles.modeposplot_n_d{pol,jwl}=plot(jd,real(handles.poles{jwl,jd}{pol}.neff),'o','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1);
                end
            else
                if jd==round(dselect)
                    handles.modeposplot_n_d{pol,jwl}=plot(jd,real(handles.poles{jwl,jd}{pol}.neff),'s','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',3);
                else
                    handles.modeposplot_n_d{pol,jwl}=plot(jd,real(handles.poles{jwl,jd}{pol}.neff),'s','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1);
                end
            end
        end
    end
end

