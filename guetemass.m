function [result] = guetemass(measurement, modelValues, f)

Z = calc_Z(modelValues, f);

Amplitude =sqrt(measurement.Z1_re.^2 + measurement.Z1_im.^2);

ErrorReal = (real(Z) - measurement.Z1_re).^2./Amplitude;
ErrorImag = (imag(Z) - measurement.Z1_im).^2./Amplitude;

result=sum(ErrorReal)+sum(ErrorImag);