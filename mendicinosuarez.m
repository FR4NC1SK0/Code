beta     = 0.994;
delta    = 0.03;
varphi   = 1.0;
phi      = 0.08;
eta      = 1.0;
theta_e  = 0.975;
theta_b  = 0.873;
mu_f     = 0.3;
mu_b     = 0.3;
sigma_f  = 0.298;
sigma_b  = 0.029;
xi       = 0.75;
eps      = 0.2;   
%psiK     = 4.567;
%a1       = 0.46; a1 y a2 deben tomar valores que garantizan que, en estado estacionario, I/K=delta y S'(I/K)=1 
%                  según mis calculos, eso implica que a1=delta^(1/psiK) y a2 =delta/(1-psiK)
%a2       = -0.0084;
kappa    = 0.54;
alpha    = 0.3;
pi       = 0.004;
varsigma = 0.006;
chi_e    = 0.001;
chi_b    = 0.859;

param = [beta delta varphi phi eta theta_e theta_b mu_f mu_b sigma_f sigma_b xi eps kappa alpha pi varsigma chi_e chi_b];

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
pi_ast   = 0.003;
rho_f    = 1.02;
ve       = beta*(1-theta_e)*rho_f/(1+pi-beta*theta_e*rho_f);
rho_b    = 1.07;
vb       = beta*(1-theta_b)*rho_b/(1+pi-beta*theta_b*rho_b);
b_f      = d/(1-(theta_b+(1-theta_b)*chi_b)*phi);
n_e      = (K_f - b_f)/(theta_e+(1-theta_e)*chi_e);
K_s      = (beta * (r_k + 1 - delta) - 1) / varsigma;
I        = delta * (K_f + K_s);
Rtilde_b = (Gamma(omega_f,sigma_f) - mu_f * G(omega_f,sigma_f)) * (r_k + 1 - delta) * K_f * (1+pi) / b_f;
Omega    = (omega_b - Gamma(omega_b,sigma_b) + mu_b * G(omega_b,sigma_b)) * Rtilde_b * b_f / d;

x0  = [r_k omega_b omega_f K_f d w L C R_b Y vp pi_ast rho_f ve rho_b vb b_f n_e K_s I Rtilde_b Omega];

%bb  = mnss_ss(x0,param)

fun = @(x) mnss_ss(x,param);
options = optimoptions(@fsolve, 'Display', 'iter', 'MaxIterations',1000,'MaxFunctionEvaluations',40000, 'FiniteDifferenceType','central');
[x,fval,exitflag,output]=fsolve(fun,x0,options);
%options = optimset('Display', 'iter', 'MaxFunEvals',10000);
%[x,fval,exitflag,output] = fminsearch(fun,x0,options);

%options = optimoptions('lsqnonlin','Display','iter','FiniteDifferenceType','central','MaxFunctionEvaluations',100000,'Algorithm','levenberg-marquardt');
%lb =[0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0];
%lb =[0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0];
%ub = [];
%[x,fval,exitflag,output] = lsqnonlin(fun,x0,lb,ub,options);