function f=mnss_ss(x0,param)
% solves for the steady state and gets the transition matrix of the
% Mendicino et al.
beta     = param(1);
delta    = param(2);
varphi   = param(3);
phi      = param(4);
eta      = param(5);
theta_e  = param(6);
theta_b  = param(7);
mu_f     = param(8);
mu_b     = param(9);
sigma_f  = param(10);
sigma_b  = param(11);
xi       = param(12);
eps      = param(13);   
%psiK     = param(14);
%a1       = param(15);  % a1 y a2 deben tomar valores que garantizan que, en estado estacionario, I/K=delta y S'(I/K)=1 
                       % según mis calculos, eso implica que a1=delta^(1/psiK) y a2 =delta/(1-psiK)
%a2       = param(16);
kappa    = param(14);
alpha    = param(15);
pi       = param(16);
varsigma = param(17);
chi_e    = param(18);
chi_b    = param(19);

r_k      = x0(1);
omega_b  = x0(2);
omega_f  = x0(3);
K_f      = x0(4);
d        = x0(5);
w        = x0(6);                
L        = x0(7);
C        = x0(8);
R_b      = x0(9);
Y        = x0(10);
vp       = x0(11);
pi_ast   = x0(12);
rho_f    = x0(13);
ve       = x0(14);
rho_b    = x0(15);
vb       = x0(16);
b_f      = x0(17);
n_e      = x0(18);
K_s      = x0(19);
I        = x0(20);
Rtilde_b = x0(21);
Omega    = x0(22);

m_b =log(1/(sigma_b^2+1)^(1/2));   %mean of the logarithm of a lognormal variable whose mean is 1 (for banks)
sigma1_b = log(sigma_b^2+1)^(1/2);      %standard deviation of the logarithm of a lognormal variable whose mean is 1 (for banks)
m_f =log(1/(sigma_f^2+1)^(1/2));   %mean of the logarithm of a lognormal variable whose mean is 1 (for firms)
sigma1_f = log(sigma_f^2+1)^(1/2);      %standard deviation of the logarithm of a lognormal variable whose mean is 1 (for firms)

f(1)  = w - varphi * L^eta * C;
f(2)  = (1-Gamma(omega_f,sigma_f))*(r_k+1-delta) * K_f * (1+pi)/n_e - rho_f;   %rho_f
f(3)  = beta * (1-theta_e) * rho_f/(1+pi-beta * theta_e * rho_f) - ve;   %v_e
f(4)  = (1-Gamma(omega_b,sigma_b)) * Rtilde_b/phi - rho_b;   %rho_b 
f(5)  = beta * (1 -theta_e) * rho_b/(1+pi - beta * theta_b * rho_b) - vb;   %v_b 
f(6)  = (1 - theta_e + theta_e * ve) * (1-logncdf(omega_f,m_f,sigma1_f)) +...
        ve/(vb * phi) * (1-theta_b + theta_b * vb) * (1-Gamma(omega_b,sigma_b)) *...
        (1-logncdf(omega_f,m_f,sigma1_f)) - mu_f * omega_f * lognpdf(omega_f,m_f,sigma1_f); 
f(7)  = beta * (1 - theta_b + theta_b * vb)/(1+pi) * (1-Gamma(omega_b,sigma_b)) * Rtilde_b - vb * phi; 
f(8)  = omega_f - R_b * b_f / ((r_k + 1 - delta *K_f) * (1+pi)); 
f(9)  = (1-logncdf(omega_b,m_b,sigma1_b)); 
f(10) = omega_b - (1 - phi) * ((1+pi) / beta + (1 - kappa) * Omega)/Rtilde_b; 
f(11) = Y-(K_f + K_s)^alpha * L^(1-alpha)/vp;    
f(12) = (1 + (1 + pi)^eps * xi) * vp - (1 - xi) * ((1+pi)/(1+pi_ast))^eps; %vp
f(13) = Y - I - C - varsigma / 2 * K_s^2 - mu_f * G(omega_f,sigma_f) * (r_k + 1 - delta) * K_f-...
        mu_b * G(omega_b,sigma_b) * Rtilde_b * b_f / (1+pi); 
f(14) = (1 - xi * beta * (1 + pi)^eps)/(1 - xi * beta * (1 + pi)^(eps - 1)) * (eps -1) / eps * (1 + pi_ast) / (1 + pi) -...
        1/(alpha^alpha * (1-alpha)^(1-alpha)) * r_k^alpha * w^(1-alpha); 
f(15) = (K_s + K_f) - alpha / (1 - alpha) * w / r_k * L; 
f(16) = (1 + pi)^(1 - eps) -  (1 - xi) * (1 + pi_ast)^(1 - eps) + xi; 
f(17) = K_f - b_f - ((theta_e*(1-logncdf(omega_f, m_f, sigma1_f))+(1-theta_e*(1-logncdf(omega_f, m_f, sigma1_f)))*chi_e))/(1+pi)*n_e;  
f(18) = (beta * (r_k + 1 - delta) - 1) / varsigma - K_s; 
f(19) = delta * (K_f + K_s) - I;
f(20) = (Gamma(omega_f,sigma_f) - mu_f * G(omega_f,sigma_f)) * (r_k + 1 - delta) * K_f * (1+pi) / b_f - Rtilde_b;
f(21) = (omega_b - Gamma(omega_b,sigma_b) + mu_b * G(omega_b,sigma_b)) * Rtilde_b * b_f / d - Omega;
f(22) = d/(1-(theta_b*(1-logncdf(omega_b, m_b, sigma1_b))+(1-theta_b*(1-logncdf(omega_b, m_b, sigma1_b)))*chi_b)/(1+pi)*phi) - b_f;  %n_b=phi b_f más d+((theta_b +(1-theta_b)chi_b)n_b=b_f 
%f = f';
%f=f*f';
