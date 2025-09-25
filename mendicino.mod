var C R Rtilde_d pi q K_s r_k R_d Omega b_f d w L v_e n_e rho_f omega_f K_f v_b R_b Rtilde_b n_b omega_b K I Y KM MC_f MC_b vp mc x1 x2 rho_b;

parameters beta delta phi_param eta theta_e theta_b mu_f mu_b sigma_f sigma_b xi eps a1 a2 psiK varsigma kappa alpha rho_R Rbar pi_ast chi_e chi_b pi_star;

beta     = 0.994;
delta    = 0.03;
varphi   = 1.0;
eta      = 1.0;
theta_e  = 0.975;
theta_b  = 0.873;
mu_f     = 0.3;
mu_b     = 0.3;
sigma_f  = 0.298;
sigma_b  = 0.029;
xi       = 0.75;
eps      = 0.2;   
psiK     = 4.567;
a1       = 0.46; //a1 y a2 deben tomar valores que garantizan que, en estado estacionario, I/K=delta y S'(I/K)=1 
                 //según mis calculos, eso implica que a1=delta^(1/psiK) y a2 =delta/(1-psiK)
a2       = -0.0084;
kappa    = 0.54;
alpha    = 0.3;
varsigma = 0.006;
chi_e    = 0.001;
chi_b    = 0.859;


mu_b =log(1/(sigma_b^2+1)^(1/2));   //mean of the logarithm of a lognormal variable whose mean is 1 (for banks)
sigma_b = log(sigma_b^2+1)^(1/2);      //standard deviation of the logarithm of a lognormal variable whose mean is 1 (for banks)
mu_f =log(1/(sigma_f^2+1)^(1/2));   //mean of the logarithm of a lognormal variable whose mean is 1 (for firms)
sigma_f = log(sigma_f^2+1)^(1/2);      //standard deviation of the logarithm of a lognormal variable whose mean is 1 (for firms)
phi_param = 1.5;
rho_R     = 0.8;
Rbar      = 1.02;
pi_ast    = 0.02;
pi_star   = 0.02;

external_function(name=Gamma, nargs=2);
external_function(name=G, nargs=2);
external_function(name=Lognormal_cdf, nargs=3);
external_function(name=Lognormal_pdf, nargs=3);

model;
   
  1/C = beta*1/C(+1)*R(+1)/(1+pi(+1));
   
  1/C = beta*1/C(+1)*Rtilde_d(+1)/(1+pi(+1));

  1/C*(q+varsigma * K_s(+1)) = beta/C(+1) * (r_k(+1) + (1-delta)*q(+1));

  Rtilde_d(+1) =R_d-(1 - kappa)*Omega(+1);

  Omega(+1) = (omega_b(+1) - Gamma(omega_b(+1), sigma_b) + mu_b*G(omega_b(+1), sigma_b)) * Rtilde_b(+1) * b_f / d;

  w/C = phi_param * L^eta;

  v_e = beta*C/(C(+1)*(1+pi(+1))) * (1 - theta_e + theta_e*v_e(+1)) * rho_f(+1);

  rho_f(+1) = (1 - Gamma(omega_f(+1), sigma_f)) * (r_k + (1 - delta)*q) * K_f * pi / n_e;

  beta*C/(C(+1)*(1+pi(+1)))*(1 - theta_e + theta_e*v_e) * (1 - Lognormal_cdf(omega_f(+1), mu_f, sigma_f))
    + v_e * beta*C/(C(+1)*(1+pi(+1)))*(1 - theta_b + theta_b*v_b) * (1 - Gamma(omega_b(+1), sigma_b))
      * (1 - Lognormal_cdf(omega_f(+1), mu_f, sigma_f) - mu_f*omega_f*Lognormal_pdf(omega_f(+1), mu_f, sigma_f))
      / (v_b * phi_param)=0;

  q*K_f(+1) = (theta_e*(1-Lognormal_cdf(omega_f(+1), mu_f, sigma_f))+(1-theta_e*(1-Lognormal_cdf(omega_f(+1), mu_f, sigma_f)))*chi_e)*n_e(-1)*rho_f/(1+pi) + n_b + d;

  beta*C/(C(+1)*(1+pi(+1)))*(1 - theta_b + theta_b*v_b)* (1 - Gamma(omega_b(+1), sigma_b)) * Rtilde_b(+1) = v_b * phi_param;

  omega_f(+1) = R_b(+1) * b_f / (r_k(+1) + (1 - delta)*q(+1) * K_f) * 1/(1+pi(+1));

  Rtilde_b(+1) * (n_b + d) = (Gamma(omega_f(+1), sigma_f) - mu_f*G(omega_f(+1), sigma_f)) * (r_k(+1) + (1 - delta)*q(+1)) * K_f(+1) * (1+pi(+1));

  v_b =  beta*C/(C(+1)*(1+pi(+1))) * (1 - theta_b + theta_b*v_b) * rho_b(+1);

  rho_b(+1) = (1 - Gamma(omega_b(+1), sigma_b));

  -(1 - Lognormal_cdf(omega_b(+1), mu_b, sigma_b)) = 0;

  omega_b(+1) = (1 - phi_param) * R_d / Rtilde_b(+1);

  n_b + d = (theta_b*(1-Lognormal_cdf(omega_b(+1), mu_b, sigma_b))+(1-theta_b*(1-Lognormal_cdf(omega_b(+1), mu_b, sigma_b)))*chi_b) * n_b(-1)*rho_b/(1+pi) + d;

  K(+1) = (a1 * (I/K)^(1 - 1/psiK) + a2) * K + (1 - delta) * K;

  -a1 * (I/K)^(-1/psiK) * (I/K) + (a1 * (I/K)^(1 - 1/psiK) + a2) = 0;

  q * a1 * (I/K)^(-1/psiK) * (I/K) = 1;

  Y = I + C + KM + MC_f + MC_b;

  Y = K^alpha * L^(1 - alpha) * vp;

  K = K_s + K_f;

  vp = (1 - xi) * (1 + pi_star)^(-eps) * (1 + pi)^eps + (1 + pi)^eps * xi * vp(-1);

  KM = varsigma * K_s(+1)^2 / 2;

  MC_f = mu_f * G(omega_f, sigma_f) * (r_k + (1 - delta)*q) * K_f;

  MC_b = mu_b * G(omega_b, sigma_b) * Rtilde_b * (n_b(-1)+d(-1)) * 1/(1+pi);

  mc = 1 / (alpha^alpha * (1 - alpha)^(1 - alpha)) * r_k^alpha * w^(1 - alpha);

  K/L = alpha / (1 - alpha) * w / r_k;

  (1 + pi)^(1 - eps) = (1 - xi) * (1 + pi_star)^(1 - eps) + xi * (1 + pi)^(1 - eps);

  x1 = mc * Y + xi * x1(+1) *beta*C/C(+1)*(1 + pi(+1))^eps;

  x2 = Y + xi * x2 *beta*C/C(+1)* (1 + pi(+1))^(eps - 1);

  1 + pi_star = eps / (eps - 1) * (1 + pi) * x1 / x2;
end;

initval;
    r_k      = 0.06; %(1/beta - 1 + delta);
    omega_b  = 4;
    omega_f  = 3;
    K_f      = 5;
    d        = 4;
    w        = 0.7;
    L        = 0.72;
    C        = 0.4;
    R_b      = 1.03;
    Y        = 1;
    vp       = 1.2;
    pi       = 0.003;
    R        = (1+pi)/beta;
    Rtilde_d = R;
    rho_f    = 1.04;
    v_e       = beta*(1-theta_e)*rho_f/(1+pi-beta*theta_e*rho_f);
    rho_b    = 1.07;
    v_b       = beta*(1-theta_b)*rho_b/(1+pi-beta*theta_b*rho_b);
    b_f      = d/(1-(theta_b+(1-theta_b)*chi_b)*phi);
    n_e      = (K_f - b_f)/(theta_e+(1-theta_e)*chi_e);
    K_s      = (beta * (r_k + 1 - delta) - 1) / varsigma;
    I        = delta * (K_f + K_s);
    Rtilde_b = (Gamma(omega_f,sigma_f) - mu_f * G(omega_f,sigma_f)) * (r_k + 1 - delta) * K_f * (1+pi) / b_f;
    Omega    = (omega_b - Gamma(omega_b,sigma_b) + mu_b * G(omega_b,sigma_b)) * Rtilde_b * b_f / d;
    R_d      = Rtilde_d+Omega;
    x1       = 0; 
    x2       = 0;
end;

steady;
check;
