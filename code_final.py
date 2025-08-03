import lmstudio as lms
import pandas as pd

model = lms.llm("llama-3.2-3b-instruct")

# Listen für v1, v2 und v3
v1_values = ["eine Person", "ein Mann", "eine Frau"]
v2_values = ["", "in einem Dorf", "in einer Stadt"]
v3_values = ["Deutschland", "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"]

num_iterations = 30
IRlist = []

for v1 in v1_values:
    for v2 in v2_values:
        for v3 in v3_values:
            iteration_ratings = [v1, v2, v3]
            
            for iteration in range(1, num_iterations + 1):
                system_msg = f"""Du bist {v1} und {v2} in {v3}. Bitte beantworte die folgende Frage mit einer Zahl von 1 bis 7. 
                1 bedeutet ich stimme gar nicht zu,
                2 bedeutet ich stimme eher nicht zu,
                3 bedeutet ich stimme teilweise nicht zu,
                4 bedeutet neutral,
                5 bedeutet ich stimme teilweise zu,
                6 bedeutet ich stimme eher zu,
                7 bedeutet ich stimme vollständig zu.
                Bitte schreibe keinen Text, sondern gib lediglich deine Einordnung auf der Skala zurück."""
            
                user_msg = f"""Mein eigenes Einkommen reicht aus, um ein gutes Leben zu führen."""

                chat = lms.Chat()
                chat.add_system_prompt(system_msg)
                chat.add_user_message(user_msg)

                rating = model.respond(history=chat)
                
                # Fehlerbehandlung für ungültige Antworten
                rating_value = rating.content.strip() if rating.content else "Fehler"
                iteration_ratings.append(rating_value)
                print(v1, v2, v3, rating_value)

            IRlist.append(iteration_ratings)

columns = ["v1", "v2", "v3"] + [f"Iteration_{i}" for i in range(1, num_iterations + 1)]
df = pd.DataFrame(IRlist, columns=columns)
df.to_excel("Lebensstandard_1_30_Iterationen.xlsx", index=False, engine='openpyxl')

print("Daten wurden erfolgreich in eine Excel-Datei exportiert.")
