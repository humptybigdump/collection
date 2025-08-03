function handles = plot_n_lambda(handles)

min_real_neff = str2double(handles.edit_realneff_start.String);
max_real_neff = str2double(handles.edit_realneff_stop.String);

min_lambda = str2double(handles.edit_wavelength_start.String)-100;
max_lambda = str2double(handles.edit_wavelength_end.String)+100;

axes(handles.axes_wavelength_neff)
cla(handles.axes_wavelength_neff)
set(gca,'YLim',[min_real_neff,max_real_neff],'XLim',[min_lambda,max_lambda]);
hold on
xlabel('wavelength')
ylabel('real n_{eff}')

jd =handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2};

wlarray = str2num(handles.edit_wavelength_start.String):str2num(handles.edit_wavelength_step.String):str2num(handles.edit_wavelength_end.String);
wlselect = handles.slider_wavelength_select.Value;

for pol=1:2
    for jwl=round(wlarray)
        try
            if pol==1
                if jwl==round(wlselect)
                    handles.modeposplot_n_d{pol,jwl}=plot(jwl,real(handles.poles{jwl,jd}{pol}.neff),'o','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',3);
                else
                    handles.modeposplot_n_d{pol,jwl}=plot(jwl,real(handles.poles{jwl,jd}{pol}.neff),'o','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1);
                end
            else
                if jwl==round(wlselect)
                    handles.modeposplot_n_d{pol,jwl}=plot(jwl,real(handles.poles{jwl,jd}{pol}.neff),'s','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',3);
                else
                    handles.modeposplot_n_d{pol,jwl}=plot(jwl,real(handles.poles{jwl,jd}{pol}.neff),'s','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1);
                end
            end
        end
    end
end
