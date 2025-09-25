function out = Lognormal_cdf(omega, mu, sigma)

    if omega <= 0
        error('Lognormal_cdf: omega debe ser > 0');
    end
    if sigma <= 0
        error('Lognormal_cdf: sigma debe ser > 0');
    end

    out = logncdf(omega, mu, sigma);
end
