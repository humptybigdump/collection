clf
    [X,Y]=meshgrid(-1.2:.1:1.2);
    subplot(2,2,1);
    quiver(X,Y,Y,X,'color',[0 0 1]);
   axis equal;
   hold on;
   t=0:0.01:2*pi;
   plot(cos(t),sin(t),'k');
   subplot(2,2,2); 
   quiver(0,0,-0.5,0,'color',[1 0 0])
   hold on; axis([-1 1 -1 1]); axis equal;
      quiver(0,0,1,0,'color',[0 0 1])
    subplot(2,2,3); axis([pi/2 7*pi/4, -0.5 0.5])
   pause(5)
   for s=pi/2:0.01:7*pi/4,
       subplot(2,2,1);
       hold off;
    quiver(X,Y,Y,X,'color',[0 0 1]);
           hold on;
   plot(cos(t),sin(t),'k');       
   axis equal;
   %    plot([cos(s) cos(s)-0.5*sin(s)],[sin(s) sin(s)+0.5*cos(s)],'r')
       plot(cos(s),sin(s),'*r')
       quiver(cos(s),sin(s),-0.5*sin(s),0.5*cos(s),'color',[1 0 0])
   subplot(2,2,2);
   hold off; 
    quiver(0,0,-0.5*sin(s),0.5*cos(s),'color',[1 0 0])
   hold on;
      quiver(0,0,sin(s),cos(s),'color',[0 0 1]);
      axis([-1 1 -1 1]);
           axis equal;
      subplot(2,2,3);
      hold on;
      axis([pi/2 7*pi/4, -0.5 0.5])
      plot([s s],[0 0.5*(cos(s)^2-sin(s)^2)],'-g')
       pause(0.01);
       hold off;
   end;