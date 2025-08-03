clc;clear;close all;

% Define the structure of the physical model and time
dt = 1e-3;
tmax = 1.5;
t0 = 0.1;
t = 0:dt:tmax;

f0 = 10;

h = 10;
nx = fix(2000/h);
ny = fix(2000/h);

Sloc_x = fix(500/h);
Sloc_y = fix(1000/h);

Rloc_y = fix((500:10:1500)/h);
Rloc_x = fix(ones(size(Rloc_y))*1500/h);

% Create velocity model
v = ones(ny,nx)*2000;
v(fix(800/h):fix(1200/h),fix(800/h):fix(1200/h)) = 2500;
v0 = ones(ny,nx)*2000;
[ny,nx] = size(v);

Lpml = 30;

% Create source term (Ricker wavelet)
s = (1-2*pi^2*f0^2.*(t-t0).^2).*exp(-pi^2*f0^2.*(t-t0).^2);
% plot(t,s); xlabel('Time (s)'); ylabel('Amplitude')


[d_obs,~] = Forward(v,h,dt,t,Lpml,s',Sloc_x,Sloc_y,Rloc_x,Rloc_y);
[d_est,wf] = Forward(v0,h,dt,t,Lpml,s',Sloc_x,Sloc_y,Rloc_x,Rloc_y);
[~,wb] = Forward(v0,h,dt,t,Lpml,flipud(d_obs-d_est),Rloc_x,Rloc_y,Rloc_x,Rloc_y);

time_deri = @(x) (x(:,:,1:end-2) - 2*x(:,:,2:end-1) + x(:,:,3:end))/dt^2;

g = (-2./v0.^3).*sum(time_deri(wf).*flip(wb(:,:,2:end-1),3),3);

close all
imagesc(g)
caxis([-4,4]*1e-1)
colormap(customcolormap_preset('orange-white-purple'))










function [data,wavefield] = Forward(v,h,dt,t,Lpml,s,Sloc_x,Sloc_y,Rloc_x,Rloc_y)

[ny,nx] = size(v);
wavefield  = zeros(ny,nx,length(t));
data = zeros(length(t),length(Rloc_x));
source = zeros(size(v));
xaxis = (0:h:nx)*h;
yaxis = (0:h:ny)*h;

u3x = zeros(size(v));
u2x = zeros(size(v));
u1x = zeros(size(v));
u3y = zeros(size(v));
u2y = zeros(size(v));
u1y = zeros(size(v));

for i = 2 : length(t)
    
    for j = 1 : length(Sloc_y)
        source(Sloc_y(j),Sloc_x(j)) = s(i,j);
    end

    [pxx,pyy] = FD(wavefield(:,:,i),h);

    [u3x,u2x,u1x,u3y,u2y,u1y] = PML(v,Lpml,h,dt,wavefield(:,:,i),u3x,u2x,u1x,u3y,u2y,u1y);

    wavefield(:,:,i+1) = 2*wavefield(:,:,i) - wavefield(:,:,i-1) + dt^2*(v.^2.*(pxx + pyy - u1y - u1x + source));

    for j = 1 : length(Rloc_x)
        data(i,j) = wavefield(Rloc_y(j), Rloc_x(j), i+1);
    end


    if mod(i,50) == 0
        f1 = subplot(121); f1.PlotBoxAspectRatio = [2 1 1]; colormap(jet)
        imagesc(xaxis,yaxis,squeeze(wavefield(:,:,i))); clim([-1,1]*max(s(:))); axis('square')
        xlabel('Distance (m)'); ylabel('Depth (m)'); title(['Time:',num2str(i*dt),' (s)'])
        xline(0,'--'); xline(nx*h,'--'); yline(0,'--'); yline(ny*h,'--')
        hold on
        plot(Sloc_x*h,Sloc_y*h,'marker','*','MarkerSize',10,'Color','white')
        plot(Rloc_x*h,Rloc_y*h,'marker','o','MarkerSize',5,'Color','white')
        hold off

        f2 = subplot(122); f2.PlotBoxAspectRatio = [2 1 1]; colorbar
        imagesc(Rloc_y*h,t,data); clim([-1,1]*max(s(:))); xlabel('Distance (m)'); ylabel('Depth (m)');
        yline(t(i),'LineWidth',2)
        colormap('jet')
        drawnow
    end

end

end


function [u3x,u2x,u1x,u3z,u2z,u1z] = PML(v,Lpml,h,dt,p,u3x,u2x,u1x,u3z,u2z,u1z)

[nz,nx] = size(v);
R0 = 1e-5;
x  = zeros(1,nx);
z  = zeros(nz,1);

x(1,1:Lpml)=Lpml:-1:1;
x(1,end-Lpml+1:end) = 1:Lpml;
z(1:Lpml,1)=(Lpml:-1:1)';
z(end-Lpml+1:end,1) = (1:Lpml)';
x = repmat(x,nz,1);
z = repmat(z,1,nx);

sigma_x = ((3.*v.*log(1./R0))./(2.*Lpml.*h)).*(x./Lpml).^2;
sigma_z = ((3.*v.*log(1./R0))./(2.*Lpml.*h)).*(z./Lpml).^2;



[dxsigma,~,~,~] = fdfirst(sigma_x,h);
[~,dzsigma,~,~] = fdfirst(sigma_z,h);

C1_x = (1-sigma_x.*dt./2)./(1+sigma_x.*dt./2);
C1_z = (1-sigma_z.*dt./2)./(1+sigma_z.*dt./2);
C2_x = (dxsigma.*dt)./(1+sigma_x.*dt./2);
C2_z = (dzsigma.*dt)./(1+sigma_z.*dt./2);
C3_x = (sigma_x.*dt)./(1+sigma_x.*dt./2);
C3_z = (sigma_z.*dt)./(1+sigma_z.*dt./2);

[px,pz,pxx,pzz] = fdfirst(p,h);

u3x = C1_x.*u3x + C2_x.*px;
u2x = C1_x.*u2x + 2.*C2_x.*px + C3_x.*(pxx - u3x);
u1x = C1_x.*u1x + C2_x.*px + C3_x.*(2.*pxx - u2x);
u3z = C1_z.*u3z + C2_z.*pz;
u2z = C1_z.*u2z + 2.*C2_z.*pz + C3_z.*(pzz - u3z);
u1z = C1_z.*u1z + C2_z.*pz + C3_z.*(2.*pzz - u2z);

end



function [px,py,pxx,pyy] = fdfirst(p,h)

[ny,nx] = size(p);
px = zeros(ny,nx);
py = zeros(ny,nx);
pxx = zeros(ny,nx);
pyy = zeros(ny,nx);
for i = 1 : ny
    for j = 1 : nx

        if i == 1
            pyy(i,j) = (0 - 2*p(i,j) + p(i+1,j))/h^2;
            py(i,j) = (0 - p(i+1,j))/h/2;
        elseif i == ny
            pyy(i,j) = (p(i-1,j) - 2*p(i,j) + 0)/h^2;
            py(i,j) = (p(i-1,j) - 0)/h/2;
        else
            pyy(i,j) = (p(i-1,j) - 2*p(i,j) + p(i+1,j))/h^2;
            py(i,j) = (p(i-1,j) - p(i+1,j))/h/2;
        end

        if j == 1
            pxx(i,j) = (0 - 2*p(i,j) + p(i,j+1))/h^2;
            px(i,j) = (0 - p(i,j+1))/2/h;
        elseif j == nx
            pxx(i,j) = (p(i,j-1) - 2*p(i,j) + 0)/h^2;
            px(i,j) = (p(i,j-1) - 0)/2/h;
        else
            pxx(i,j) = (p(i,j-1) - 2*p(i,j) + p(i,j+1))/h^2;
            px(i,j) = (p(i,j-1) - p(i,j+1))/2/h;
        end


    end
end
end


function [pxx,pyy] = FD(p,h)
[ny,nx] = size(p);
pxx = size(ny,nx);
pyy = size(ny,nx);

for i = 1 : ny
    for j = 1 : nx

        if i == 1
            pyy(i,j) = (-0 + 0 - 30*p(i,j) + 16*p(i+1,j) - p(i+2,j))/(12*h^2);
        elseif i == 2
            pyy(i,j) = (-0 + 16*p(i-1,j) - 30*p(i,j) + 16*p(i+1,j) - p(i+2,j))/(12*h^2);
        elseif i == ny-1
            pyy(i,j) = (-p(i-2,j) + 16*p(i-1,j) - 30*p(i,j) + 16*p(i+1,j) - 0)/(12*h^2);
        elseif i == ny
            pyy(i,j) = (-p(i-2,j) + 16*p(i-1,j) - 30*p(i,j) + 0 - 0)/(12*h^2);
        else
            pyy(i,j) = (-p(i-2,j) + 16*p(i-1,j) - 30*p(i,j) + 16*p(i+1,j) - p(i+2,j))/(12*h^2);
        end

        if j == 1
            pxx(i,j) = (-0 + 0 - 30*p(i,j) + 16*p(i,j+1) - p(i,j+2))/(12*h^2);
        elseif j == 2
            pxx(i,j) = (-0 + 16*p(i,j-1) - 30*p(i,j) + 16*p(i,j+1) - p(i,j+2))/(12*h^2);
        elseif j == nx-1
            pxx(i,j) = (-p(i,j-2) + 16*p(i,j-1) - 30*p(i,j) + 16*p(i,j+1) - 0)/(12*h^2);
        elseif j == nx
            pxx(i,j) = (-p(i,j-2) + 16*p(i,j-1) - 30*p(i,j) + 0 - 0)/(12*h^2);
        else
            pxx(i,j) = (-p(i,j-2) + 16*p(i,j-1) - 30*p(i,j) + 16*p(i,j+1) - p(i,j+2))/(12*h^2);
        end


    end
end
end
