function handles = plotwgmode(handles)

wl=handles.slider_wavelength_select.Value;
d_sweep = handles.thickness_slider.Value;

d = [handles.uitable_layer_system.Data{:,2}]';
d(1)=0;d(end)=0;
n = [handles.uitable_layer_system.Data{:,3}]';
k = [handles.uitable_layer_system.Data{:,4}]';
% strc = array that contains the thickness and the value of the complex
% refractive index

strc = [d,n+1i*k];

number_of_layers = str2num(handles.edit_number_of_layers.String);

linspecs={'-','--',':','-:','.'};
axes(handles.axes_wgmodes)
cla(handles.axes_wgmodes)
set(gca,'Color','none','YLim',[0,1.1],'XLim',handles.axes_layer_system_k.XLim);


hold on
% for pol=1:2
%     try
%         for jpl=1:length(handles.show_profile.plot_nrm{pol})
%             delete(handles.show_profile.plot_nrm{pol}{jpl})
%         end
%     end
%     
%     try
%         for jpl=1:length(handles.show_profile.plot_par{pol})
%             delete(handles.show_profile.plot_par{pol}{jpl})
%         end
%     end
%         
%     try
%         for jpl=1:length(handles.show_profile.plot_perp{pol})
%             delete(handles.show_profile.plot_perp{pol}{jpl})
%         end
%     end
% end

handles.show_profile.plot_nrm = cell(2,1);
handles.show_profile.plot_par = cell(2,1);
handles.show_profile.plot_perp = cell(2,1);

try
    pls = handles.poles{round(wl), round(d_sweep)};
    for pol=1:2
        %        if ~isempty(pls{pol})
        
        for jpl = 1:length(pls{pol}.kpar)
            
            kp = pls{pol}.kpar(jpl);
            
            fpz = -300:1:sum(d)+300;
            
            % number of field points
            fp_num = length(fpz);
            
            % layer number of each field point
            i_L = zeros(1,fp_num);
            for jfp = 1:fp_num
                i_L(jfp) = layer_number(fpz(jfp),[handles.uitable_layer_system.Data{:,2}]);
            end
            
            Epar = fpz-fpz;
            Eperp = fpz-fpz;
            
            
            g_top =   [ 1 ;  0];
            
            for ji = 1:number_of_layers
                
                fp_ind = find(i_L==ji);  % select those field points that are located in layer ji
                
                if ~isempty(fp_ind)
                    
                    giL = layer_Transfer_matrix(pol,strc(ji:number_of_layers,:),kp,wl) * g_top;
                    
                    ki = strc(ji,2)*2*pi/wl;
                    kzi = sqrt(ki^2-kp^2);
                    if imag(kzi)<0
                        kzi=-kzi;
                    end
                    
                    fpzrel = fpz(fp_ind) - sum(d(1:(ji-1)));  % relative position of field point with respect to layer anchor point
                    
                    if pol==1
                        Epar(fp_ind) = giL(1)*exp(1i*kzi*fpzrel)+giL(2)*exp(-1i*kzi*fpzrel);
                        Eperp(fp_ind) = 0;
                    else
                        Epar(fp_ind) = (giL(1)*exp(1i*kzi*fpzrel)-giL(2)*exp(-1i*kzi*fpzrel))*kzi/ki;
                        Eperp(fp_ind) = (-giL(1)*exp(1i*kzi*fpzrel)-giL(2)*exp(-1i*kzi*fpzrel))*kp/ki;
                    end
                end
            end
            
            Epar=Epar/max(abs(Epar));
            if pol==2
                Eperp=Eperp/max(abs(Eperp));
            end
            if pol==1
                plotcol=handles.settings.colplot(2,:);
            else
                plotcol=handles.settings.colplot(1,:);
            end
            if handles.popup_field_component.Value==1
                handles.show_profile.plot_nrm{pol}{jpl}=plot(fpz,(abs(Epar).^2+abs(Eperp).^2)/max(abs(Epar).^2+abs(Eperp).^2),linspecs{jpl},'Color',plotcol,'LineWidth',2);
            elseif handles.popup_field_component.Value==2
                handles.show_profile.plot_nrm{pol}{jpl}=plot(fpz,abs(Epar),linspecs{jpl},'Color',plotcol,'LineWidth',2);
            elseif handles.popup_field_component.Value==3
                handles.show_profile.plot_nrm{pol}{jpl}=plot(fpz,abs(Eperp),linspecs{jpl},'Color',plotcol,'LineWidth',2);
            end
        end
    end
end
