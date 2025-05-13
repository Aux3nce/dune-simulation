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

(* Programme principal*)
let () =
  let grille = init_grille 10 10 in
  grille.(5).(5)  <- 1000;
  Printf.printf "Grille initiale :\n";
  affichage_grille grille;

  let resultat = simuler grille in
  Printf.printf "Grille après simulation :\n";
  affichage_grille resultat;
  sauvegarder_grille resultat "matrice_ocaml.txt";
  save_to_file "matrice_ocaml.txt" resultat
;;