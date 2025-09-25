function out = G(omegabar,sigma)
mu1 =log(1/(sigma^2+1)^(1/2));   %mean of the logarithm of a lognormal variable whose mean is 1
sigma1 = log(sigma^2+1)^(1/2);      %standard deviation of the logarithm of a lognormal variable whose mean is 1        

% Define the PDF function
pdf_func = @(x) lognpdf(x,mu1,sigma1);
% Compute the truncated expectation
out = integral(@(x) x .* pdf_func(x), 0, omegabar);
end