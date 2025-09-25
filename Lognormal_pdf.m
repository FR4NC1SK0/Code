function out = Lognormal_pdf(omega, mu, sigma)


    if omega <= 0
        error('Lognormal_pdf: omega debe ser > 0');
    end
    if sigma <= 0
        error('Lognormal_pdf: sigma debe ser > 0');
    end

    out = lognpdf(omega, mu, sigma);
end
