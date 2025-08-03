function handles = plotmodepos(handles)

%creates the range in which the poles will be plotted
min_real_neff = str2double(handles.edit_realneff_start.String);
max_real_neff = str2double(handles.edit_realneff_stop.String);

min_imag_neff = str2double(handles.edit_imagneff_start.String);
max_imag_neff = str2double(handles.edit_imagneff_stop.String);

%setting the axes and their properties
axes(handles.axes_neff)
cla(handles.axes_neff)
set(gca,'XLim',[min_real_neff,max_real_neff],'YLim',[min_imag_neff,max_imag_neff]);
hold on
xlabel('real n_{eff}')
ylabel('imag n_{eff}')

textdist = (-min_imag_neff+max_imag_neff)/5;
textoffset = (-min_real_neff+max_real_neff)/20;
jwl=handles.slider_wavelength_select.Value;
jd =handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2};
for pol=1:2
    try
        if pol==1
            handles.modeposplot{pol}=plot(real(handles.poles{jwl,jd}{pol}.neff),imag(handles.poles{jwl,jd}{pol}.neff),'o','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1.5);
        else
            handles.modeposplot{pol}=plot(real(handles.poles{jwl,jd}{pol}.neff),imag(handles.poles{jwl,jd}{pol}.neff),'s','Color',(handles.settings.colplot(3-pol,:)),'LineWidth',1.5);
        end
    end
    try
        counter=0;
        for jpl = length(handles.poles{jwl,jd}{pol}.neff):-1:1
            if pol==1
                modepostextstring = sprintf('TE%i',counter);
            else
                modepostextstring = sprintf('TM%i',counter);
            end
            counter=counter+1;
            handles.modepostext{pol, jpl}=text(real(handles.poles{jwl,jd}{pol}.neff(jpl))-textoffset,imag(handles.poles{jwl,jd}{pol}.neff(jpl))+textdist,modepostextstring,'Color',(handles.settings.colplot(3-pol,:)));
        end
    end
end
