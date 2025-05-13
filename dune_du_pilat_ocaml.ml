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


(*Implémentation du vent sous un module qui priorise les voisins*)
module Vent = struct
  type direction = Nord | Sud | Est | Ouest

  let voisins_selon_vent dir (i, j) =
    match dir with
    | Nord  -> [(i-1, j); (i, j+1); (i, j-1); (i+1, j)]
    | Sud   -> [(i+1, j); (i, j+1); (i, j-1); (i-1, j)]
    | Est   -> [(i, j+1); (i+1, j); (i-1, j); (i, j-1)]
    | Ouest -> [(i, j-1); (i+1, j); (i-1, j); (i, j+1)]
end
;;

let appliquer_regle_vent grille vent =
  let largeur = Array.length grille in
  let longueur = Array.length grille.(0) in
  let nouvelle_grille = Array.map Array.copy grille in
  let modifie = ref false in
  for i = 0 to largeur - 1 do
    for j = 0 to longueur - 1 do
      let voisins = Vent.voisins_selon_vent vent (i, j) in
      List.iter (fun (vi, vj) ->
          if vi >= 0 && vi < largeur && vj >= 0 && vj < longueur then
            let ecart = grille.(i).(j) - grille.(vi).(vj) in
            if ecart > seuil then (
              nouvelle_grille.(i).(j) <- nouvelle_grille.(i).(j) - 1;
              nouvelle_grille.(vi).(vj) <- nouvelle_grille.(vi).(vj) + 1;
              modifie := true
            )
        ) voisins
    done
  done;
  if !modifie then Some nouvelle_grille 
  else None
;;



(* Retourne true si un des sommets initiaux n'est plus un maximum local *)
let sommet_n_est_plus_maximum grille positions =
  let hauteur i j = grille.(i).(j) in
  let largeur = Array.length grille
  and longueur = Array.length grille.(0) in
  List.exists (fun (i,j) ->
      let h = hauteur i j in
      let voisins = [(i-1,j); (i+1,j); (i,j-1); (i,j+1)] in
      List.exists (fun (vi,vj) ->
          vi >= 0 && vi < largeur && vj >= 0 && vj < longueur &&
          grille.(vi).(vj) > h
        ) voisins
    ) positions
;;

(* Simulation avec vent jusqu'à stabilisation *)
let rec simuler_avec_vent grille vent =
  match appliquer_regle_vent grille vent with
  | Some nouvelle_grille -> simuler_avec_vent nouvelle_grille vent
  | None -> grille
;;

(* Transformation en fichier txt*)
let sauvegarder_grille grille nom_fichier =
  let out = open_out nom_fichier in
  Array.iter (fun ligne ->
      Array.iter (fun valeur -> Printf.fprintf out "%d " valeur) ligne;
      Printf.fprintf out "\n"
    ) grille;
  close_out out
;;

let save_to_file filename matrix =
  let oc = open_out filename in
  Array.iter (fun row ->
      Array.iteri (fun i v ->
          output_string oc (string_of_int v);
          if i < Array.length row - 1 then output_char oc ','
        ) row;
      output_char oc '\n'
    ) matrix;
  close_out oc
;;

(* Programme principal *)
let () =
  let grille = init_grille 50 50 in

  (* Dépôt de sable au centre *)
  for i = 2 to 44 do
    grille.(i).(22) <- 9000;
  done;

  Printf.printf "Grille initiale :\n";
  affichage_grille grille;

  let vent = Vent.Est in
  let resultat = simuler_avec_vent grille vent in

  Printf.printf "Grille après simulation :\n";
  affichage_grille resultat;
  sauvegarder_grille resultat "pilat_ocaml.txt";
  save_to_file "pilat_ocaml.txt" resultat
;;