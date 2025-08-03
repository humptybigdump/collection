import re
import numpy as np
import matplotlib.pyplot as plt

sentences = ["They call it a Royale with cheese.",
           "A Royale with cheese. What do they call a Bic Mac?",
           "Well, a Big Mac is a Big Mac, but they call it le Big-Mac.",
           "Le Big-Mac. Ha ha ha ha. What do they call a Whopper?"]

def split_sentence(sentence):
    return [word for word in re.split(r'[ .,!?]+', sentence) if word]

def occ(w, x):
    words = split_sentence(x)
    return words.count(w)

def k(x, z, d):
    list_of_all_occuring_substrings = list(set(split_sentence(x) + split_sentence(z)))
    sum = 0
    for w in list_of_all_occuring_substrings:
        sum = sum + occ(w, x) * occ(w, z)
    return sum**d

def compute_k_matrix(sentences, d):
    n = len(sentences)
    k_matrix = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            k_matrix[i][j] = k(sentences[i], sentences[j], d)
    return k_matrix

def print_k_matrix(k_matrix):
    n = k_matrix.shape[0]
    # Print header
    header = " " * 11 + " | " + " | ".join([f'Sentence {i + 1}' for i in range(n)])
    print(header)
    print("-" * len(header))

    # Print each row with its index
    for i in range(n):
        row = [f'Sentence {i + 1}'] + [f"{k_matrix[i][j]:.2f}" for j in range(n)]
        print(" | ".join(row))

def print_k_grid(k_matrix):
    plt.imshow(k_matrix, cmap='gray')
    plt.show()

if __name__ == '__main__':
    matrix = compute_k_matrix(sentences, 1)
    print_k_grid(matrix)