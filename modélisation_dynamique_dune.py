# -*- coding: utf-8 -*-
"""
Created on Wed May 21 09:57:28 2025

@author: auxen
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.animation import FuncAnimation

#############################################################################

# Fonction pour obtenir le fichier OCaml
def charger_matrices_depuis_fichier(nom_fichier):
    with open(nom_fichier, 'r') as f:
        lignes = f.readlines()

    matrices = []
    matrice_courante = []

    for ligne in lignes:
        ligne = ligne.strip()
        if ligne.startswith("Étape"):
            if matrice_courante:
                matrices.append(np.array(matrice_courante, dtype=int))
                matrice_courante = []
        elif ligne and all(c.isdigit() or c.isspace() for c in ligne):
            valeurs = list(map(int, ligne.split()))
            matrice_courante.append(valeurs)

    if matrice_courante:
        matrices.append(np.array(matrice_courante, dtype=int))

    return matrices


# Charger les données souhaitées
chemin = r"C:\Users\auxen\OneDrive\Documents\MP_etoile\TIPE\jpp\historique_simulation.txt"
matrices = charger_matrices_depuis_fichier(chemin)

#  Initialisation de la figure
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

nrows, ncols = matrices[0].shape
_x = np.arange(ncols)
_y = np.arange(nrows)
_xx, _yy = np.meshgrid(_x, _y)
x, y = _xx.ravel(), _yy.ravel()
bottom = np.zeros_like(x)
width = depth = 0.8
bars = None

# Fonction d’animation
def update(frame):
    global bars
    ax.clear()

    Z = matrices[frame]
    top = Z.ravel()

    colors = cm.gist_earth((top - top.min()) / (top.max() - top.min()))
    bars = ax.bar3d(x, y, bottom, width, depth, top, color=colors, shade=True)

    ax.set_title(f"Étape {frame}")
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Hauteur")
    ax.view_init(elev=45, azim=135)

# Lancer l’animation
ani = FuncAnimation(fig, update, frames=len(matrices), interval=200)

plt.show()
