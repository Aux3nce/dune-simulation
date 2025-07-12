matrice = charger_matrice_depuis_fichier("matrice_ocaml.txt")
visualiser_2d(matrice)
visualiser_3d(matrice)

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm

def charger_matrice_depuis_fichier(nom_fichier):
    """
    Charge une matrice depuis un fichier texte où chaque ligne
    représente une ligne de la matrice, avec des valeurs séparées par des espaces.
    """
    with open(nom_fichier, 'r') as f:
        lignes = f.readlines()
    
    # Convertir chaque ligne en liste d'entiers
    matrice = []
    for ligne in lignes:
        if ligne.strip():  # Ignorer les lignes vides
            valeurs = [int(x) for x in ligne.strip().split()]
            matrice.append(valeurs)
    
    return np.array(matrice)

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

def generer_animation(matrices, nom_fichier="animation_dune.gif"):
    """
    Génère une animation à partir d'une liste de matrices représentant
    différentes étapes de la simulation.
    Nécessite imageio : pip install imageio
    """
    import imageio
    import os
    
    # Créer un dossier temporaire pour les images
    os.makedirs("temp_frames", exist_ok=True)
    
    # Générer une image pour chaque matrice
    filenames = []
    for i, mat in enumerate(matrices):
        # Sauvegarder l'image
        filename = f"temp_frames/frame_{i:03d}.png"
        filenames.append(filename)
        
        plt.figure(figsize=(8, 6))
        plt.imshow(mat, cmap=cm.terrain)
        plt.colorbar(label="Hauteur")
        plt.title(f"Étape {i}")
        plt.tight_layout()
        plt.savefig(filename)
        plt.close()
    
    # Créer le GIF
    with imageio.get_writer(nom_fichier, mode='I', duration=0.3) as writer:
        for filename in filenames:
            image = imageio.imread(filename)
            writer.append_data(image)
    
    print(f"Animation sauvegardée sous {nom_fichier}")
    
    # Nettoyer les fichiers temporaires (optionnel)
    for filename in filenames:
        os.remove(filename)
    os.rmdir("temp_frames")

def exemple_utilisation():
    """
    Exemple d'utilisation avec une matrice générée directement en Python.
    Dans un cas réel, vous chargeriez la matrice depuis un fichier généré par OCaml.
    """
    # Exemple de matrice (à remplacer par les données de OCaml)
    matrice = np.zeros((10, 10), dtype=int)
    matrice[5, 5] = 10
    matrice[4, 5] = 8
    matrice[5, 4] = 8
    matrice[6, 5] = 8
    matrice[5, 6] = 8
    matrice[4, 4] = 6
    matrice[6, 6] = 6
    matrice[4, 6] = 6
    matrice[6, 4] = 6
    
    # Afficher la matrice
    print("Matrice:")
    for ligne in matrice:
        print(" ".join(f"{val:2d}" for val in ligne))
    
    # Visualiser
    visualiser_2d(matrice)
    visualiser_3d(matrice)

if __name__ == "__main__":
    # Vous pouvez soit charger une matrice depuis un fichier généré par OCaml:
    # matrice = charger_matrice_depuis_fichier("matrice_ocaml.txt")
    
    # Soit utiliser l'exemple pour tester:
    exemple_utilisation()
