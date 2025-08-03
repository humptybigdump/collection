function varargout = ModeFinder(varargin)

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @ModeFinder_OpeningFcn, ...
                   'gui_OutputFcn',  @ModeFinder_OutputFcn, ...
                   'gui_LayoutFcn',  [], ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
   gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before ModeFinder is made visible.
function ModeFinder_OpeningFcn(hObject, eventdata, handles, varargin)
handles.output = hObject;
handles.nkRawdata = cell(3,1);
handles.settings.designcolors={[119,79,56]/255,[224,142,121]/255,[241,212,175]/255,[236,229,206]/255,[197,224,220]/255}; % let them eat cake
handles.settings.col=handles.settings.designcolors([1,2,3,5]); % col is the color scheme of the visualization of the layers
handles.settings.colnum=length(handles.settings.col);
% handles.settings.colshine = [1,1,0.5];  % color that indicates the active layers
handles.settings.colplot=lines(5);  % colplot is the colorscheme for plots
handles.settings.kcol=[0.7,0.4,0];  % color of the extinction coefficient plot
handles.axes_layer_system_k.Color='none';
handles.axes_layer_system_n.Color='none';
if ~isfield(handles,'poles')
    handles.poles={};
end
handles=plotnk(handles);
update_popupmenu(handles);
handles.edit_realneff_start.TooltipString='minimal real part for n_eff';
handles.edit_realneff_stop.TooltipString='maximal real part for n_eff';
handles.edit_imagneff_start.TooltipString='minimal imaginary part for n_eff';
handles.edit_imagneff_stop.TooltipString='maximal imaginary part for n_eff';
handles.edit_wavelength_start.TooltipString='Choose the beginning of the wavelength sweep';
handles.edit_wavelength_end.TooltipString='Choose the end of the wavelength sweep';
handles.edit_wavelength_step.TooltipString='Choose the step of the wavelength sweep';
handles.thickness_sweep_start.TooltipString='Choose the beginning of the thickness sweep';
handles.thickness_sweep_end.TooltipString='Choose the end of the thickness sweep';
handles.thickness_sweep_step.TooltipString='Choose the step of the thickness sweep';
handles.thickness_popup_menu.TooltipString='Choose the layer to be sweeped';
handles.thickness_checkbox.TooltipString='Tick if thickness sweep is wanted, too';
handles.pushbutton_save.TooltipString='Press to save the results';
handles.pushbutton_run.TooltipString='Press to run the sweep for one wavelength';
handles.pushbutton_run_all.TooltipString='Press to run the sweep for all wavelengths';
handles.pushbutton_export.TooltipString='Press to export the results in ASCI format';
handles.edit_number_of_layers.TooltipString='Type in the desired number of layers';
guidata(hObject, handles);

% --- Outputs from this function are returned to the command line.
function varargout = ModeFinder_OutputFcn(hObject, eventdata, handles)
varargout{1} = handles.output;

% ---------------- uicontrol callbacks -----------------------

function edit_number_of_layers_Callback(hObject, eventdata, handles)
laynums=round(str2num(handles.edit_number_of_layers.String));
if laynums>length(handles.uitable_layer_system.Data(:,1))
    addline={'',0,1,0,false,'',false};
    handles.uitable_layer_system.Data = [handles.uitable_layer_system.Data;repmat(addline,laynums-length(handles.uitable_layer_system.Data(:,1)),1)];
    handles.nkRawdata = [handles.nkRawdata;repmat({},laynums-length(handles.nkRawdata),1)];
elseif laynums<length(handles.uitable_layer_system.Data(:,1))
    handles.uitable_layer_system.Data =    handles.uitable_layer_system.Data(1:laynums,:);
    handles.nkRawdata = handles.nkRawdata(1:laynums,:);
end
%no_of_layers = str2double(handles.edit_number_of_layers.String);
test_consistency(handles)
update_popupmenu(handles);
handles=clear_poles(handles);
guidata(hObject, handles);

function uitable_layer_system_Callback(hObject, eventdata, handles)
% has the checkbox for nk file been modified?
if eventdata.Indices(2)==5 && eventdata.EditData
    file_name=uigetfile('*','Please select refractive index file');
    rawdata = uiimport(file_name);
    rawdata_fieldnames=fieldnames(rawdata);
    if length(rawdata_fieldnames)==1
        rawnkdat = getfield(rawdata,rawdata_fieldnames{1});
    else
        rawnkdat = rawdata.data;
    end
    handles.uitable_layer_system.Data{eventdata.Indices(1),eventdata.Indices(2)+1} = file_name;
    handles.nkRawdata{eventdata.Indices(1)} = rawnkdat;
end
%no_of_layers = str2double(handles.edit_number_of_layers.String);
update_popupmenu(handles);
handles=layer_table_update(handles);
handles=clear_poles(handles);
guidata(hObject, handles);

function slider_wavelength_select_Callback(hObject, eventdata, handles)
handles.text_selected_wavelength.String = ['Selected wavelength: ',num2str(handles.slider_wavelength_select.Value)];
handles=layer_table_update(handles);
handles=refresh_plots(handles);
guidata(hObject, handles);

function pushbutton_export_Callback(hObject, eventdata, handles)
[FileName,PathName] = uiputfile;
fullFileName = [PathName,FileName];
fileID = fopen(fullFileName,'w');
fprintf(fileID,'%% This file was generated by the ModeFinder GUI\n');
wlarray = str2num(handles.edit_wavelength_start.String):str2num(handles.edit_wavelength_step.String):str2num(handles.edit_wavelength_end.String);
if handles.thickness_checkbox.Value == 1 % is the thickness sweep active?
    darray = str2num(handles.thickness_sweep_start.String):str2num(handles.thickness_sweep_step.String):str2num(handles.thickness_sweep_end.String);handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2}
    maximal_pole_order = 0;
    for d=round(darray)
        for wl=round(wlarray)
            for pol=1:2
                try
                    maximal_pole_order = max(maximal_pole_order,length(handles.poles{wl,d}{pol}.neff));
                end
            end
        end
    end
    fprintf(fileID,'%% Columns are: thickness, wavelength');
    for jp=1:maximal_pole_order
        fprintf(fileID,', TE%i mode',jp-1);
    end
    for jp=1:maximal_pole_order
        fprintf(fileID,', TM%i mode',jp-1);
    end
    fprintf(fileID,'\n');
    fclose(fileID);
    poles_array=zeros(length(wlarray)*length(darray),2*maximal_pole_order+2); 
    row_idx=0;
    for d=round(darray)
        for wl=round(wlarray)
            row_idx=row_idx+1;
            poles_array(row_idx,1) = d;
            poles_array(row_idx,2) = wl;
            if ~isempty(handles.poles{wl,d})
                for jwg=1:length(handles.poles{wl,d}{1}.neff)
                    poles_array(row_idx,2+jwg) = handles.poles{wl,d}{1}.neff(end-jwg+1);
                end
                for jwg=1:length(handles.poles{wl,d}{2}.neff)
                    poles_array(row_idx,2+maximal_pole_order+jwg) = handles.poles{wl,d}{2}.neff(end-jwg+1);
                end
            end
        end
    end
    dlmwrite(fullFileName,poles_array,'-append');
else
    d = handles.uitable_layer_system.Data{get(handles.thickness_popup_menu,'Value'),2};
    maximal_pole_order = 0;
    for wl=round(wlarray)
        for pol=1:2
            try
                maximal_pole_order = max(maximal_pole_order,length(handles.poles{wl,d}{pol}.neff));
            end
        end
    end
    fprintf(fileID,'%% Columns are: wavelength');
    for jp=1:maximal_pole_order
        fprintf(fileID,', TE%i mode',jp-1);
    end
    for jp=1:maximal_pole_order
        fprintf(fileID,', TM%i mode',jp-1);
    end
    fprintf(fileID,'\n');
    fclose(fileID);
    poles_array=zeros(length(wlarray),2*maximal_pole_order+1);
    row_idx=0;
    for wl=round(wlarray)
        row_idx=row_idx+1;
        poles_array(row_idx,1) = wl;
        if ~isempty(handles.poles{wl,d})
            for jwg=1:length(handles.poles{wl,d}{1}.neff)
                poles_array(row_idx,1+jwg) = handles.poles{wl,d}{1}.neff(end-jwg+1);
            end
            for jwg=1:length(handles.poles{wl,d}{2}.neff)
                poles_array(row_idx,1+maximal_pole_order+jwg) = handles.poles{wl,d}{2}.neff(end-jwg+1);
            end
        end
    end
    dlmwrite(fullFileName,poles_array,'-append','delimiter','\t');
end

function pushbutton_save_Callback(hObject, eventdata, handles)
uisave('handles','mode_finder_model.mat')

function edit_detection_thresh_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_realneff_stop_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_realneff_start_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_contour_discretization_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_imagneff_stop_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_imagneff_start_Callback(hObject, eventdata, handles)
test_consistency(handles)

function edit_wavelength_start_Callback(hObject, eventdata, handles)
handles.slider_wavelength_select.Min = str2num(handles.edit_wavelength_start.String);

if handles.slider_wavelength_select.Value<handles.slider_wavelength_select.Min
    handles.slider_wavelength_select.Value=handles.slider_wavelength_select.Min;
end
test_consistency(handles);
guidata(hObject, handles);

function edit_wavelength_end_Callback(hObject, eventdata, handles)
handles.slider_wavelength_select.Max = str2num(handles.edit_wavelength_end.String);
if handles.slider_wavelength_select.Value>handles.slider_wavelength_select.Max
    handles.slider_wavelength_select.Value=handles.slider_wavelength_select.Max;
end
test_consistency(handles)
guidata(hObject, handles);

function edit_wavelength_step_Callback(hObject, eventdata, handles)
handles.slider_wavelength_select.SliderStep = ([str2num(handles.edit_wavelength_step.String),str2num(handles.edit_wavelength_step.String)])/(handles.slider_wavelength_select.Max-handles.slider_wavelength_select.Min);
guidata(hObject, handles);
test_consistency(handles)

function edit_min_loop_size_Callback(hObject, eventdata, handles)
test_consistency(handles)

function popup_field_component_Callback(hObject, eventdata, handles)
handles = plotwgmode(handles);
guidata(hObject, handles);

function pushbutton_run_Callback(hObject, eventdata, handles)
handles = find_poles(handles);
handles=refresh_plots(handles);
guidata(hObject, handles);

function pushbutton_run_all_Callback(hObject, eventdata, handles)
wlarray = str2num(handles.edit_wavelength_start.String):str2num(handles.edit_wavelength_step.String):str2num(handles.edit_wavelength_end.String);
thickness_array = str2num(handles.thickness_sweep_start.String):str2num(handles.thickness_sweep_step.String):str2num(handles.thickness_sweep_end.String);
selected_layer=get(handles.thickness_popup_menu,'Value');

if handles.thickness_checkbox.Value == 1

for wl=wlarray
    for d_sweep = thickness_array
    handles.slider_wavelength_select.Value = wl;
    handles.thickness_slider.Value = d_sweep;
    %ModeFinderGUI('CALLBACK','slider_wavelength_select_Callback',hObject, eventdata, handles);
    slider_wavelength_select_Callback(hObject, eventdata, handles);
    thickness_slider_Callback(hObject, eventdata, handles);
    drawnow
    handles = find_poles(handles);
    handles=refresh_plots(handles);
    guidata(hObject, handles);
    end
end

else
    for wl=wlarray
        handles.slider_wavelength_select.Value = wl;
        %ModeFinderGUI('CALLBACK','slider_wavelength_select_Callback',hObject, eventdata, handles);
        slider_wavelength_select_Callback(hObject, eventdata, handles);
        thickness_slider_Callback(hObject, eventdata, handles);
        drawnow
        handles = find_poles(handles);
        handles = plotwgmode(handles);
        handles = plotmodepos(handles);
        handles = plot_n_lambda(handles);
        handles = plot_n_d(handles);
        guidata(hObject, handles);
    end
end

function thickness_slider_Callback(hObject, eventdata, handles)

handles.thickness_slider_name.String = ['Selected thickness: ',num2str(handles.thickness_slider.Value)];
aux_value = handles.thickness_slider.Value;

selected_layer=get(handles.thickness_popup_menu,'Value');
handles.uitable_layer_system.Data(selected_layer,2)={aux_value};
%handles.uitable_layer_system.Data(selected_layer,2).Value = handles.thickness_slider.Value;

handles=layer_table_update(handles);
handles = plotnk(handles);
handles = plotwgmode(handles);
handles = plotmodepos(handles);
handles = plot_n_lambda(handles);
handles = plot_n_d(handles);
guidata(hObject, handles);

function thickness_sweep_start_Callback(hObject, eventdata, handles)
handles.thickness_slider.Min = str2num(handles.thickness_sweep_start.String);
if handles.thickness_slider.Value < handles.thickness_slider.Min
    handles.thickness_slider.Value=handles.thickness_slider.Min;
end
test_consistency(handles)
guidata(hObject, handles);

function thickness_sweep_start_CreateFcn(hObject, eventdata, handles)

function thickness_sweep_end_Callback(hObject, eventdata, handles)
handles.thickness_slider.Max = str2num(handles.thickness_sweep_end.String);
if handles.thickness_slider.Value>handles.thickness_slider.Max
    handles.thickness_slider.Value=handles.thickness_slider.Max;
end
test_consistency(handles)
guidata(hObject, handles);

function thickness_sweep_step_Callback(hObject, eventdata, handles)
handles.thickness_slider.SliderStep = ([str2num(handles.thickness_sweep_step.String),str2num(handles.thickness_sweep_step.String)])/(handles.thickness_slider.Max-handles.thickness_slider.Min);
test_consistency(handles)
guidata(hObject, handles);

function thickness_checkbox_Callback(hObject, eventdata, handles)
update_popupmenu(handles)
if handles.thickness_checkbox.Value == 0
    set(handles.thickness_popup_menu,'Enable','off');
    set(handles.thickness_sweep_start,'Enable','off');
    set(handles.thickness_sweep_end,'Enable','off');
    set(handles.thickness_sweep_step,'Enable','off');
    set(handles.thickness_slider,'Enable','off');
end

if handles.thickness_checkbox.Value == 1
    thickness_popup_menu_Callback(hObject, eventdata, handles)
    set(handles.thickness_popup_menu,'Enable','on');
    set(handles.thickness_sweep_start,'Enable','on');
    set(handles.thickness_sweep_end,'Enable','on');
    set(handles.thickness_sweep_step,'Enable','on');
    set(handles.thickness_slider,'Enable','on');
end

function update_popupmenu(handles)
layer_names=handles.uitable_layer_system.Data(:,1);
set(handles.thickness_popup_menu,'String',layer_names);
set(handles.thickness_popup_menu,'Value',2);

function thickness_popup_menu_Callback(hObject, eventdata, handles)
selected_layer=get(handles.thickness_popup_menu,'Value');
disp(selected_layer);

test_consistency(handles)

if (selected_layer > 1) & (selected_layer < str2num(handles.edit_number_of_layers.String))
    if isfinite(handles.uitable_layer_system.Data{selected_layer,2})==0 
       errordlg('Please enter a finite number for the thickness value!','Error');
    else 
        disp('updating the thickness value from the table into the slider current value')
   aux_value = handles.uitable_layer_system.Data{selected_layer,2};
   handles.thickness_slider.Value = aux_value;
   handles.thickness_slider_name.String = ['Selected thickness: ',num2str(aux_value)];

   disp(aux_value)
      if (aux_value < handles.thickness_slider.Min)
      handles.thickness_slider.Min = 0;
      handles.thickness_slider.Max = aux_value; 
      handles.thickness_sweep_start.String = '0';    
      handles.thickness_sweep_end.String = num2str(aux_value);
      handles.thickness_slider.SliderStep = ([str2num(handles.thickness_sweep_step.String),str2num(handles.thickness_sweep_step.String)])/(handles.thickness_slider.Max-handles.thickness_slider.Min);    
      else
      handles.thickness_slider.String = num2str(aux_value);
      handles.thickness_slider.Max = aux_value;
      handles.thickness_sweep_end.String = num2str(aux_value);
      handles.thickness_slider.SliderStep = ([str2num(handles.thickness_sweep_step.String),str2num(handles.thickness_sweep_step.String)])/(handles.thickness_slider.Max-handles.thickness_slider.Min);
      end
    end
end

function thickness_refresh_popup_Callback(hObject, eventdata, handles)
no_of_layers = str2double(handles.edit_number_of_layers.String);
layer_names=handles.uitable_layer_system.Data(:,1);
set(handles.thickness_popup_menu,'String',layer_names);

function test_consistency(handles)

%check if the the begining of the wavelength sweep range is smaller than the end, if not
%it displays a message

if str2double(handles.edit_wavelength_start.String) > str2double(handles.edit_wavelength_end.String)
  errordlg('The value of the beginning of the sweep cannot be bigger than the end value of the sweep!','Error');  
end

%checks if the the begining of the thickness sweep range is smaller than the end, if not
%it displays a message
if str2double(handles.thickness_sweep_start.String) > str2double(handles.thickness_sweep_end.String)
  errordlg('The value of the beginning of the sweep cannot be bigger than the end value of the sweep!','Error');  
end

%checks if the values introduced in the input boxes are negative, if yes,
%it displays an error message

if (str2double(handles.edit_wavelength_start.String)<0) | (str2double(handles.edit_wavelength_end.String)<0) | (str2double(handles.thickness_sweep_start.String)<0) | (str2double(handles.thickness_sweep_end.String)<0) | (str2double(handles.edit_wavelength_step.String)<0) | (str2double(handles.thickness_sweep_step.String)<0)|(str2double(handles.edit_number_of_layers.String)<0)
   errordlg('This value cannot be negative! Please introduce a positive value!','Error'); 
end

if (str2double(handles.edit_realneff_start.String)<0) | (str2double(handles.edit_realneff_stop.String)<0) | (str2double(handles.edit_imagneff_stop.String)<0) | (str2double(handles.edit_detection_thresh.String)<0) | (str2double(handles.edit_contour_discretization.String)<0)|(str2double(handles.edit_min_loop_size.String)<0)
   errordlg('This value cannot be negative! Please introduce a positive value!','Error'); 
end


%checks if the value at the begining of refractive index is smaller than the end value, if not
%it displays a message

if str2double(handles.edit_realneff_start.String) > str2double(handles.edit_realneff_stop.String)
  errordlg('The value at which the real refractive index starts cannot be bigger than the end value!','Error');  
end

if str2double(handles.edit_imagneff_start.String) > str2double(handles.edit_imagneff_stop.String)
  errordlg('The value at which the imaginary refractive index starts cannot be bigger than the end value!','Error');  
end

% checks if the first or the last layers in the table have been selected
% for the thickness sweep and displays a message to prevent that 
selected_layer=get(handles.thickness_popup_menu,'Value');
if (selected_layer ==1)|(selected_layer == str2num(handles.edit_number_of_layers.String))
  if (isfinite(handles.uitable_layer_system.Data{1,2})==0) | (isfinite(handles.uitable_layer_system.Data{str2num(handles.edit_number_of_layers.String),2})==0)
   errordlg('Please select a different layer for the sweep, other than the first or the last!','Error'); 
  end
end
   
function axes_wgmodes_ButtonDownFcn(hObject, eventdata, handles)
f1=figure;
nhdl=copyobj(handles.axes_layer_system_n,f1);
set(nhdl, 'Position',[0.15,0.15,0.7,0.8])
khdl=copyobj(handles.axes_layer_system_k,f1);
set(khdl, 'Position',[0.15,0.15,0.7,0.8],'XTick',[])
wghdl=copyobj(handles.axes_wgmodes,f1);
% set(wghdl, 'XTickMode', 'auto', 'XTickLabelMode', 'auto')
% set(wghdl, 'YTickMode', 'auto', 'YTickLabelMode', 'auto','YAxisLocation','left')
set(wghdl, 'Position',[0.15,0.15,0.7,0.8])
% xlabel('position')
% ylabel('mode profile')
