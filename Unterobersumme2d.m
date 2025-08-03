clf
 [X,Y]=meshgrid(0:0.005:1, 0:0.005:1);
Z=(X-1).^2+Y.^2+1; mesh(X,Y,Z);axis([0 1 0 1 0 3]);
pause(0.5);
for i=2:10,
%[x,y]=meshgrid(linspace(0,1-1/i,i),linspace(0,1-1/i,i));
x=linspace(0,1-1/i,i);
y=linspace(0,1-1/i,i);
mesh(X,Y,Z); axis([0 1 0 1 0 3]); hold on;
 for k=1:i, for j=1:i, 
         z1=(x(j)-1).^2+(y(k)+1/i).^2+1;
         z2=(x(j)+1/i-1).^2+(y(k)).^2+1;
         fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [z1 z1 z1 z1],'r')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [z2 z2 z2 z2],'r')
     fill3([x(j) x(j) x(j) x(j)], [y(k)  y(k)+1/i y(k)+1/i y(k)], [z2 z2 z1 z1],'r')
     fill3([x(j)+1/i  x(j)+1/i  x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [z2 z2 z1 z1],'r')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k) y(k) y(k)], [z2 z1 z1 z2],'r')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)+1/i   y(k)+1/i y(k)+1/i y(k)+1/i ], [z2 z1 z1 z2],'r')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [z2 z2 z2 z2],'b')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [0 0 0 0],'b')
     fill3([x(j) x(j) x(j) x(j)], [y(k)  y(k)+1/i y(k)+1/i y(k)], [0 0 z2 z2],'b')
     fill3([x(j)+1/i  x(j)+1/i  x(j)+1/i x(j)+1/i], [y(k)  y(k)+1/i y(k)+1/i y(k)], [0 0 z2 z2],'b')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)  y(k) y(k) y(k)], [0 z2 z2 0],'b')
     fill3([x(j) x(j) x(j)+1/i x(j)+1/i], [y(k)+1/i   y(k)+1/i y(k)+1/i y(k)+1/i ], [0 z2 z2 0],'b')
     hold on;
     end; end;
mesh(X,Y,Z);
axis([0 1 0 1 0 3]);
pause(0.5);
hold off;
end;