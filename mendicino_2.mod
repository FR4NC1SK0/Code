var C Rd pi pi_star q Ks Kf K Y vp rk Omega omega_b Rtilde_b b d w L ve ne omega_f vb nb KM MCf MCb x1 x2 rho_f rho_b mct I R_b;
parameters beta delta phi_param eta theta_e theta_b mu_f mu_b sigma_f sigma_b xi eps a1 a2 psiK varsigma kappa alpha rho_R Rbar pi_bar alpha_pi alpha_GDP;

beta       = 0.994;
delta      = 0.03;
phi_param  = 1;
eta        = 1.0;
theta_e    = 0.975;
theta_b    = 0.873;
mu_f       = 0.3;
sigma_f    = 0.298;
mu_b       = 0.3;
sigma_b    = 0.029;
xi         = 0.75;
eps        = 0.1;
psiK       = 4.567;
a1         = 0.46;
a2         = -0.0084;
kappa      = 0.54;
alpha      = 0.3;
rho_R      = 0.75;
Rbar       = 1.01;
pi_bar     = 1.02;
alpha_pi   = 1.5;
alpha_GDP  = 0.1;
varsigma   = 0.006;

external_function(name=Gamma,       nargs=3);
external_function(name=G,           nargs=3);
external_function(name=Lognormal_cdf, nargs=3);
external_function(name=Lognormal_pdf, nargs=3);

model;
  // (186)
  1/C = beta*(1/C)*(pi/beta)/pi;

  // (188)
  q + phi_param*Ks = beta * (rk + (1 - delta)*q/pi);

  // (189)
  (pi/beta) = Rd - (1 - kappa)*Omega;

  // (190)
  Omega = (omega_b - Gamma(omega_b,mu_b, sigma_b) + mu_b*G(omega_b,mu_b, sigma_b)) * Rtilde_b * b / d;

  // (191)
  w/C = phi_param * L^eta;

  // (192a)
  ve = beta/pi * (1 - theta_e + theta_e*ve) * rho_f;

  // (192b)
  rho_f = (1 - Gamma(omega_f,mu_f, sigma_f)) * (rk + (1 - delta)*q) * Kf * pi / ne;

  // (194)
  (beta*(1 - theta_e + theta_e*ve))/pi * (1 - Lognormal_cdf(omega_f, mu_f, sigma_f))
    + ve * (beta*(1 - theta_b + theta_b*vb))/pi * (1 - Gamma(omega_b,mu_b, sigma_b))
      * (1 - Lognormal_cdf(omega_f, mu_f, sigma_f) - mu_f*omega_f*Lognormal_pdf(omega_f, mu_f, sigma_f))
      / (vb * phi_param) = 0;

  // (197)
  q * Kf = ne + b;

  // (198)
  beta * (1 - theta_b + theta_b*vb) / pi * (1 - Gamma(omega_b,mu_b, sigma_b)) * Rtilde_b = vb * phi_param;

  // (199)
  omega_f = R_b * b / ((rk + (1 - delta)*q) * Kf) * (1/pi);

  // (200)
  Rtilde_b * b = (Gamma(omega_f,mu_f, sigma_f) - mu_f*G(omega_f,mu_f, sigma_f)) * (rk + (1 - delta)*q) * Kf * pi;

  // (201a)
  vb = beta/pi * (1 - theta_b + theta_b*vb) * rho_b;

  // (201b)
  rho_b = (1 - Gamma(omega_b,mu_b, sigma_b));

  // (202)
  -(1 - Lognormal_cdf(omega_b, 1, sigma_b)) = 0;

  // (203)
  omega_b = (1 - phi_param) * Rd / Rtilde_b;

  // (204)
  b = nb + d;

  // (205)
  K = (a1 * (I/K)^(1 - 1/psiK) + a2) * K + (1 - delta) * K;

  // (206)
  -a1 * (I/K)^(-1/psiK) * (I/K) + (a1 * (I/K)^(1 - 1/psiK) + a2) = 0;

  // (207)
  q * a1 * (I/K)^(-1/psiK) * (I/K) = 1;

  // (208)
  Y = I + C + KM + MCf + MCb;

  // (209)
  Y = K^alpha * L^(1 - alpha) * vp;

  // (210)
  K = Ks + Kf;

  // (211)
  vp = (1 - xi) * (1 + pi_star)^(-eps) * (1 + pi)^eps + (1 + pi)^eps * xi * vp;

  // (KM)
  KM = varsigma * Ks^2 / 2;

  // (212)
  MCf = mu_f * G(omega_f,mu_f, sigma_f) * (rk + (1 - delta)*q/pi) * Kf;

  // (213)
  MCb = mu_b * G(omega_b,mu_b, sigma_b) * Rtilde_b * b * (1/pi);

  // (214)
  mct = 1 / (alpha^alpha * (1 - alpha)^(1 - alpha)) * rk^alpha * w^(1 - alpha);

  // (215)
  K/L = alpha / (1 - alpha) * w / rk;

  // (216)
  (1 + pi)^(1 - eps) = (1 - xi) * (1 + pi_star)^(1 - eps) + xi * (1 + pi)^(1 - eps);

  // (217)
  x1 = mct * Y + xi * x1 * (1 / (1 + pi))^eps;

  // (218)
  x2 = Y + xi * x2 * (1 / (1 + pi))^(eps - 1);

  // (219)
  1 + pi_star = eps / (eps - 1) * (1 + pi) * x1 / x2;
end;

initval;
  rho_f    = 1.006036;   
  omega_f  = 0.2;           
  omega_b  = 0.2; 
  rho_b    = 1 - Gamma(omega_b,mu_b, sigma_b);  
  K        = 1;      
  Ks       = 0;      
  Kf       = 1;      
  I        = delta * K;    
  vp       = 1;      
  w        = 0.7;    
  L        = 0.72;   
  Y        = K^alpha * L^(1 - alpha) * vp;
  C        = Y - I;
  KM       = varsigma * Ks^2 / 2;
  MCf      = 0; 
  MCb      = 0; 
  pi       = 1;    
  pi_star  = 1;    
  q        = 1;    
  rk       = (1/beta - 1 + delta);
  Rd       = pi / beta;
  Rtilde_b = 1.009;  
  R_b      = 1.004;
  Omega    = 1;
  b        = 0;
  d        = 0;      
  ne       = 1;      
  nb       = 0;      
  ve       = 0.00616;    
  vb       = 0;          
  ne       = 1;          
  nb       = 0;          
  mct      = 1/(alpha^alpha * (1 - alpha)^(1 - alpha)) * rk^alpha * w^(1 - alpha);
  x1       = 0; 
  x2       = 0;
end;

steady;
check;
