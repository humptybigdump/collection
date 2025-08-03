import tkinter as tk
import time
import random
import csv
import os

# Modul für akustischen Ton abhängig vom Betriebssystem
try:
    import winsound  # Nur für Windows
    def play_beep(frequency=1000, duration=500):
        winsound.Beep(frequency, duration)
except ImportError:
    import sounddevice as sd
    import numpy as np

    def play_beep(frequency=1000, duration=500):
        fs = 44100  # Sampling rate
        t = np.linspace(0, duration / 1000, int(fs * duration / 1000), endpoint=False)
        wave = 0.5 * np.sin(2 * np.pi * frequency * t)
        sd.play(wave, samplerate=fs)

# Konfigurationseinstellungen
STIMULUS_COLOR = "red"  # Farbe des visuellen Stimulus
SOUND_FREQUENCY = 2000  # Frequenz des Tons in Hz
SOUND_DURATION = 700  # Dauer des Tons in ms

class ReactionTimeTest:
    def __init__(self, root):
        self.root = root
        self.root.title("Reaktionszeitexperiment")
        self.root.geometry("400x300")
        self.root.configure(bg="white")

        self.instruction_label = tk.Label(root, text="Warte auf den Stimulus und drücke die Leertaste!", font=("Arial", 14))
        self.instruction_label.pack(pady=20)

        self.result_label = tk.Label(root, text="", font=("Arial", 12))
        self.result_label.pack(pady=10)

        # CSV-Datei erstellen oder öffnen
        self.filename = "reaction_times.csv"
        self.create_csv_if_not_exists()

        # Event-Handler für die Leertaste
        self.root.bind("<space>", self.check_reaction)

        # Initialisieren von Variablen
        self.start_time = None
        self.waiting_for_reaction = False

        # Startet den Test
        self.start_test()

    def create_csv_if_not_exists(self):
        # CSV-Datei erstellen, wenn sie nicht existiert
        if not os.path.exists(self.filename):
            with open(self.filename, mode='w', newline='') as file:
                writer = csv.writer(file)
                writer.writerow(["Stimulus", "Reaktionszeit (Sekunden)", "Zeitpunkt"])

    def start_test(self):
        # Zufällige Verzögerung
        delay = random.uniform(2, 5)  # zwischen 2 und 5 Sekunden
        self.root.after(int(delay * 1000), self.trigger_stimulus)

    def trigger_stimulus(self):
        # Sofortige Änderung der Farbe und Abspielen des Tons
        self.start_time = time.time()  # Start der Zeitmessung vor beiden Stimuli
        self.root.configure(bg=STIMULUS_COLOR)  # Farbe des Stimulus
        play_beep(frequency=SOUND_FREQUENCY, duration=SOUND_DURATION)  # Konfigurierbarer Ton

        # Warten auf die Reaktion des Benutzers
        self.waiting_for_reaction = True

    def check_reaction(self, event):
        if self.waiting_for_reaction:
            # Reaktionszeit sofort berechnen
            reaction_time = time.time() - self.start_time
            self.result_label.config(text=f"Reaktionszeit: {reaction_time:.3f} Sekunden")

            # Ergebnis in CSV speichern
            self.save_result("visuell+akustisch", reaction_time)

            # Zurücksetzen für den nächsten Versuch
            self.waiting_for_reaction = False
            self.root.configure(bg="white")  # Setzt den Hintergrund zurück

            # Startet einen neuen Test nach kurzer Verzögerung
            self.root.after(2000, self.start_test)  # Warte 2 Sekunden vor dem nächsten Test

    def save_result(self, stimulus_type, reaction_time):
        # Ergebnis in CSV-Datei speichern
        with open(self.filename, mode='a', newline='') as file:
            writer = csv.writer(file)
            writer.writerow([stimulus_type, f"{reaction_time:.3f}", time.strftime("%Y-%m-%d %H:%M:%S")])

# Hauptanwendung starten
root = tk.Tk()
app = ReactionTimeTest(root)
root.mainloop()