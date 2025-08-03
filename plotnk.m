function handles=plotnk(handles)
% plot the refractive index and extinction coefficient as a function of
% position

% delete old graphics objects
cla(handles.axes_layer_system_k)
cla(handles.axes_layer_system_n)

laynum = str2num(handles.edit_number_of_layers.String);

pos_n=[-300,0,0];
for j1=2:1:(laynum-1)
    pos_n = [pos_n,sum([handles.uitable_layer_system.Data{2:j1,2}]),sum([handles.uitable_layer_system.Data{2:j1,2}])];
end
pos_n = [pos_n,pos_n(end)+300];
n_arr = [];
for j1=1:1:(laynum)
    n_arr=[n_arr,handles.uitable_layer_system.Data{j1,3},handles.uitable_layer_system.Data{j1,3}];
end
k_arr = [];
for j1=1:1:(laynum)
    k_arr=[k_arr,handles.uitable_layer_system.Data{j1,4},handles.uitable_layer_system.Data{j1,4}];
end

axes(handles.axes_layer_system_n)
ylabel('refractive index n')
xlabel('z position')
axes(handles.axes_layer_system_k)
ylabel('extinction coefficient k')

y1lim=ceil(2*(max(n_arr)+0.2))/2;
y2lim=ceil(max(k_arr)+1);

set(handles.axes_layer_system_n,'YLim',[0,y1lim])
set(handles.axes_layer_system_n,'YTick',[0:y1lim/5:y1lim])
set(handles.axes_layer_system_n,'XLim',[min(pos_n),max(pos_n)])

set(handles.axes_layer_system_k,'YLim',[0,y2lim])
set(handles.axes_layer_system_k,'YTick',[0:y2lim/5:y2lim])
set(handles.axes_layer_system_k,'XLim',[min(pos_n),max(pos_n)])

handles.nkplot.Rectangles={};
for j1=1:laynum
    layercolor=min(handles.settings.col{mod(j1,handles.settings.colnum)+1},1);
    if j1==1
        handles.nkplot.Rectangles{j1}=rectangle('Position',[-300,0,300,y2lim],'FaceColor',layercolor,'EdgeColor',layercolor);
        uistack(handles.nkplot.Rectangles{j1},'bottom')
        handles.nkplot.PlotText{j1}=text(-300+25,y2lim*0.05,handles.uitable_layer_system.Data{j1,1},'Rotation',90);
        uistack(handles.nkplot.PlotText{j1},'bottom')
    elseif j1==laynum
        handles.nkplot.Rectangles{j1}=rectangle('Position',[sum([handles.uitable_layer_system.Data{2:j1-1,2}]),0,300,y2lim],'FaceColor',layercolor,'EdgeColor',layercolor);
        uistack(handles.nkplot.Rectangles{j1},'bottom')
        handles.nkplot.PlotText{j1}=text(sum([handles.uitable_layer_system.Data{2:j1-1,2}])+25,y2lim*0.05,handles.uitable_layer_system.Data{j1,1},'Rotation',90);
        uistack(handles.nkplot.PlotText{j1},'bottom')
    else
        handles.nkplot.Rectangles{j1}=rectangle('Position',[sum([handles.uitable_layer_system.Data{2:j1-1,2}]),0,handles.uitable_layer_system.Data{j1,2},y2lim],'FaceColor',layercolor,'EdgeColor',layercolor);
        uistack(handles.nkplot.Rectangles{j1},'bottom')
        handles.nkplot.PlotText{j1}=text(sum([handles.uitable_layer_system.Data{2:j1-1,2}])+25,y2lim*0.05,handles.uitable_layer_system.Data{j1,1},'Rotation',90);
        uistack(handles.nkplot.PlotText{j1},'bottom')
    end
end

axes(handles.axes_layer_system_k)
handles.nkplot.kline=line(pos_n,k_arr,'LineWidth',1.5,'Color',handles.settings.kcol,'LineStyle','--');
uistack(handles.nkplot.kline,'top')
axes(handles.axes_layer_system_n)
handles.nkplot.nline=line(pos_n,n_arr,'LineWidth',1.5,'Color',[0,0,0],'LineStyle','--');
uistack(handles.nkplot.nline,'top')
