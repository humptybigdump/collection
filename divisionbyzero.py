import math

def get_fraction(numerator, denominator):
    """
    Calculates the fraction of the two input numbers

    Parameters:
    numerator (float): numerator of the fraction
    denominator (float): denominator of the fraction

    Returns:
    float: fraction of both numbers

    """
    try:
        fraction = numerator/denominator
    except ZeroDivisionError:
        print("Warning: Durch Null teilen ist verboten! Gebe \"not a number\" (nan) zurueck.")
        return float('nan')
    return fraction

numerator = 4.0
denominator = 0.0

fraction = get_fraction(numerator, denominator)
print(f"Der Quotient von {numerator} / {denominator} = {fraction}")

print(fraction > 0)
print(fraction < 0)
print(fraction == 0)
print(math.isclose(fraction, 0.0, abs_tol=1.e9))
print(fraction + 5)
print(fraction * 5)
print(math.isnan(fraction))
