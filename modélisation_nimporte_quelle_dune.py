# -*- coding: utf-8 -*-
"""
Created on Fri May  9 16:47:09 2025

@author: auxen
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import os
##############################################################################

chemin = r"C:\Users\auxen\OneDrive\Documents\MP_etoile\TIPE\jpp\pilat_ocaml.txt"
print("Fichier existe :", os.path.isfile(chemin))

try:
    sortie_ocaml = np.loadtxt(chemin, delimiter=",", dtype=int)
    print("Matrice chargée !")
#    print(sortie_ocaml)
except Exception as e:
    print("Erreur :", e)



##############################################################################

def visualiser_2d(matrice, titre="Dune de sable - Vue 2D"):
    """
    Affiche une visualisation 2D de la matrice avec une échelle de couleur.
    """
    plt.figure(figsize=(10, 8))
    im = plt.imshow(matrice, cmap=cm.terrain)
    plt.colorbar(im, label="Hauteur")
    plt.title(titre)
    plt.tight_layout()
    plt.savefig("dune_2d.png")
    plt.show()

##############################################################################

def visualiser_3d(matrice, titre="Dune de sable - Vue 3D"):
    """
    Crée une représentation 3D de la dune de sable.
    """
    fig = plt.figure(figsize=(12, 10))
    ax = fig.add_subplot(111, projection='3d')
    
    # Créer des coordonnées x, y pour chaque point de la matrice
    y, x = np.meshgrid(range(matrice.shape[1]), range(matrice.shape[0]))
    
    # Tracer la surface
    surf = ax.plot_surface(x, y, matrice, cmap=cm.terrain, 
                         linewidth=0, antialiased=True)
    
    # Ajouter une barre de couleur
    fig.colorbar(surf, ax=ax, shrink=0.5, aspect=5, label="Hauteur")
    
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Hauteur')
    ax.set_title(titre)
    
    plt.savefig("dune_3d.png")
    plt.show()

##############################################################################

# Ta matrice déjà chargée
Z = sortie_ocaml
nrows, ncols = Z.shape

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

_x = np.arange(ncols)
_y = np.arange(nrows)
_xx, _yy = np.meshgrid(_x, _y)
x, y = _xx.ravel(), _yy.ravel()
top = Z.ravel()
bottom = np.zeros_like(top)
width = depth = 0.8

# Couleur sable : RGB (0.76, 0.70, 0.50)
colors = [(0.76, 0.70, 0.50)] * len(top)
colors = plt.cm.gist_earth((top - top.min()) / (top.max() - top.min()))

ax.bar3d(x, y, bottom, width, depth, top, shade=True, color=colors)

ax.set_title("Histogramme 3D - Dune couleur sable")
ax.set_xlabel("X")
ax.set_ylabel("Y")
ax.set_zlabel("Hauteur")

plt.show()




##############################################################################

# Afficher la matrice dans la console
#print("Matrice importée:")
#for ligne in sortie_ocaml:
#    print(" ".join(f"{val:2d}" for val in ligne))

# Visualisations
#if __name__ == "__main__":
#    visualiser_2d(sortie_ocaml)
#    visualiser_3d(sortie_ocaml)
