(*Seuil d'�coulement du sable*)
let seuil = 6;;

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


(*R�gle d'�coulement en absence de contrainte sur notre automate*)
let appliquer_regle grille =
  let largeur = Array.length grille in
  let longueur = Array.length grille.(0) in
  let delta = Array.make_matrix largeur longueur 0 in
  let modifie = ref false in

  for i = 0 to largeur - 1 do
    for j = 0 to longueur - 1 do
      let h = grille.(i).(j) in
      let voisins = [(i-1,j); (i+1,j); (i,j-1); (i,j+1)] in
      let voisins_valides =
        List.filter (fun (vi,vj) ->
            vi >= 0 && vi < largeur && vj >= 0 && vj < longueur &&
            (h - grille.(vi).(vj)) > seuil
          ) voisins
      in
      let n = List.length voisins_valides in
      if n > 0 then (
        delta.(i).(j) <- delta.(i).(j) - n;
        List.iter (fun (vi, vj) ->
            delta.(vi).(vj) <- delta.(vi).(vj) + 1
          ) voisins_valides;
        modifie := true
      )
    done
  done;

  if !modifie then (
    let nouvelle_grille = Array.map Array.copy grille in
    for i = 0 to largeur - 1 do
      for j = 0 to longueur - 1 do
        nouvelle_grille.(i).(j) <- grille.(i).(j) + delta.(i).(j)
      done
    done;
    Some nouvelle_grille
  ) else None
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


(* Simulation jusqu'� stabilisation *)
let rec simuler grille = match appliquer_regle grille with
  | Some nouvelle_grille -> simuler nouvelle_grille
  | None -> grille
;;

		(* ou *)

(* Simulation dynamique jusqu'� stabilisation *)
let simuler_dynamique grille sommets =
  let grille_courante = ref grille in
  let continuer = ref true in
  while !continuer do
    match appliquer_regle !grille_courante with
    | Some nouvelle ->
        if sommet_n_est_plus_maximum nouvelle sommets then (
          print_endline "Un des sommets initiaux est d�pass�. Arr�t.";
          continuer := false
        ) else (
          print_string "\027[2J";  (* Efface �cran *)
          print_string "\027[H";   (* Curseur en haut *)
          Printf.printf "�tape suivante :\n";
          affichage_grille nouvelle;
          grille_courante := nouvelle
        )
    | None -> continuer := false
  done;
  !grille_courante
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
  let grille = init_grille 30 30 in
  let sommets = ref [] in
  for i = 0 to 29 do
  	grille.(10).(i) <- 10000;
  	sommets := (i,7)::(!sommets);
  done;
  grille.(25).(25) <- 50000;
  Printf.printf "Grille initiale :\n";
  affichage_grille grille;

  let resultat = simuler(*_dynamique*) grille (* !voisins *) in
  Printf.printf "Grille apr�s simulation :\n";
  affichage_grille resultat;
  sauvegarder_grille resultat "mat_ocaml.txt";
  save_to_file "mat_ocaml.txt" resultat
;;
