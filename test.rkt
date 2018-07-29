#lang racket/base
(require "codegen.rkt")
#|
(display(codegen "v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=-mu/r^3*x;
          vy'=-mu/r^3*y;
          vz'=-mu/r^3*z;
         }"))
(display(codegen "v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);
phi:=asin(z/r);gr_phi_r:=-mu/r^2*(1+J*(ae/r)^2*(1-5*sin(phi)^2));gwe:=-2*mu/r^2*J*(ae/r)^2*sin(phi);
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=gr_phi_r*x/r;
          vy'=gr_phi_r*y/r;
          vz'=gr_phi_r*z/r+gwe;
         }"))

(display(codegen "v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);
sinphi:=z/r;gr_phi_r:=-mu/r^2*(1+J*(ae/r)^2*(1-5*sinphi^2));gwe:=-2*mu/r^2*J*(ae/r)^2*sinphi;
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=gr_phi_r*x/r;
          vy'=gr_phi_r*y/r;
          vz'=gr_phi_r*z/r+gwe;
         }"))
(display(codegen "v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);
sinphi:=z/r;gr_phi_r:=-mu/r^2;
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=gr_phi_r*x/r;
          vy'=gr_phi_r*y/r;
          vz'=gr_phi_r*z/r;
         }"))
|#


(display(codegenrk4 (open-input-string "v:=sqrt(vx^2+vy^2+vz^2);r:=sqrt(x^2+y^2+z^2);%*%s%%
sinphi:=z/r;gr_phi_r:=-mu/r^2*(1+J*(ae/r)^2*(1-5*sinphi^2));gwe:=-2*mu/r^2*J*(ae/r)^2*sinphi;
#with(t){ x'=vx;
          y'=vy;
          z'=vz;
          vx'=gr_phi_r*x/r;
          vy'=gr_phi_r*y/r;
          vz'=gr_phi_r*z/r+gwe;
         }")))