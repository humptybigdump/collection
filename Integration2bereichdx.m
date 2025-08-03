clf
x=-1:0.01:3;
y1=x.^2;
y2=2*x+3;
 for k=-1:0.05:3,
     plot([-1 3],[0 0],'k');
     hold on;
     plot([0 0],[-1 9],'k');
     plot(x,y1,'b');
     plot(x,y2,'b');
     for l=k^2:0.1:2*k+3,
         plot(k,l,'.g'),
              pause(0.01)
     end;
     hold off;
 end;