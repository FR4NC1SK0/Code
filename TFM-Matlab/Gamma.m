function out = Gamma(omegabar,sigma)
mu1 =log(1/(sigma^2+1)^(1/2));   %mean of the logarithm of a lognormal variable whose mean is 1
sigma1 = log(sigma^2+1)^(1/2);      %standard deviation of the logarithm of a lognormal variable whose mean is 1        

%Compute Gamma
out = G(omegabar,sigma1)+omegabar*(1-logncdf(omegabar,mu1,sigma1));
end