def calculate_rabbits(current_population):
    """
    Get the population of rabbits at the next day

    Parameters:
    current_population (float): Current population of rabbits

    Return:
    float: population at next day
    """

    new_population = current_population * 1.02
    return new_population

start_population = 10
current_population = 10
goal_population = 50000

number_of_days = 0

while (current_population != goal_population):
    current_population = calculate_rabbits(current_population)
    number_of_days += 1
    print(number_of_days)

print(f"Nach {number_of_days} Tagen gibt es ueber {goal_population} Hasen. Die tatsaechliche Anzahl an Hasen ist {current_population:.0f}.")
