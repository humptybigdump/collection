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
        print("Durch Null teilen ist verboten!")
        return 0
    return fraction

while True:
    try:
        numerator = float(input("Bitte geben Sie hier den Zaehler ein: "))
        denominator = float(input("Bitte geben Sie hier den Nenner ein: "))
        break
    except ValueError:
        print("Das war falsch! Bitte geben Sie eine Fliesskommazahl ein.")

fraction = get_fraction(numerator, denominator)
print(f"Der Quotient von {numerator} / {denominator} = {fraction}")