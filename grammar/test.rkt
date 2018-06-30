#lang autosolve

r:=sqrt(x^2+y^2);
v:=sqrt(vx^2+vy^2);
#with(t){ x'=vx;
          y'=vy;
          vx'=-mu/r^3*x;
          vy'=-mu/r^3*y;
         }