(*Seuil d'écoulement du sable*)
let seuil = 5;;

(*Initialisation du plateau recevant les grains de sables*)
let init_grille longueur largeur = Array.make_matrix largeur longueur 0;;


(*Affichage de la grille sous formes de cellules d'entiers*)
let affichage_grille grille = 
  Array.iter (fun ligne ->
      Array.iter (fun colonne -> Printf.printf "%2d " colonne) ligne;
      print_newline ()
    ) grille;
  print_newline ()
;;


(*Règle d'écoulement en absence de contrainte sur notre automate*)
let appliquer_regle grille = 
  let largeur, longueur = (Array.length grille), (Array.length grille.(0)) in
  let nouvelle_grille = Array.map Array.copy grille in
  let modifie = ref false in
  for i = 0 to (largeur-1) do
    for j = 0 to (longueur-1) do
      let voisins = [(i-1, j); (i+1, j); (i, j-1); (i, j+1)] in
      List.iter (fun (vi, vj) ->
          if vi >= 0 && vi < largeur && vj >= 0 && vj < longueur then
            let ecart = grille.(i).(j) - grille.(vi).(vj) in
            if ecart > seuil then (
              nouvelle_grille.(i).(j) <- nouvelle_grille.(i).(j) - 1;
              nouvelle_grille.(vi).(vj) <- nouvelle_grille.(vi).(vj) + 1;
              modifie := true
            )
        ) voisins
    done;
  done;
  if !modifie then Some nouvelle_grille else None
;;


(* Simulation jusqu'à stabilisation *)
let rec simuler grille = match appliquer_regle grille with
  | Some nouvelle_grille -> simuler nouvelle_grille
  | None -> grille
;;

(* Programme principal *)
let () =
  let grille = init_grille 10 10 in
  grille.(5).(5)  <- 100;
  Printf.printf "Grille initiale :\n";
  affichage_grille grille;

  let resultat = simuler grille in
  Printf.printf "Grille après simulation :\n";
  affichage_grille resultat
;;
