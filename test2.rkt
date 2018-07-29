#lang autosolve


v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);
sinphi:=z/r;gr_phi_r:=-mu/r^2*(1+J*(ae/r)^2*(1-5*sinphi^2));gwe:=-2*mu/r^2*J*(ae/r)^2*sinphi;
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=gr_phi_r*x/r;
          vy'=gr_phi_r*y/r;
          vz'=gr_phi_r*z/r+gwe;
         }
%*#ivp{
%*  [0 0 0 0 0 0;0]->[10]
%*}