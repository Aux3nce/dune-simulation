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

  let deplacement_vent dir =
    match dir with
    | Nord  -> (-1, 0)
    | Sud   -> (1, 0)
    | Est   -> (0, 2)
    | Ouest -> (0, -1)

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

(* Implémentation de la saltation sur chacune des cellules de la dune *)

let appliquer_saltation grille x y =
  let (_,vent_dir) = Vent.deplacement_vent Vent.Est in
  let proba_saut = 0.75 in
  match grille.(x).(y) with
  | qte when qte > 0 && Random.float 1.0 < proba_saut ->
      let dist = 2 + Random.int 10 in  (* Saut entre 1 et 10 cases *)
      let x' = x + (vent_dir * dist) in
      if x' < Array.length grille then
       ( match grille.(x').(y) with
        | 0 ->
            grille.(x').(y) <- 3;  (* dépose dans la cellule cible entre 1 et 5 grains *)
            let nouvelle_qte = qte - 3 in
            grille.(x).(y) <- nouvelle_qte
        | qte' ->
            (* ajoute un grain à la cellule cible *)
            grille.(x').(y) <- qte' + 2;
            let nouvelle_qte = qte - 2 in
            grille.(x).(y) <- nouvelle_qte
        )
  | _ -> ()
;;

(* Implémentation du charriage sur chacune des cellules recevant la saltation *)

let appliquer_charriage grille x y force_charriage =
  let proba_charriage = 0.25 in
  match grille.(x).(y) with
  | qte when qte >= force_charriage && Random.float 1.0 < proba_charriage ->
      let hauteur = Array.length grille.(0) in
      if y + 1 < hauteur then
       ( match grille.(x).(y+1) with
        | 0 ->
            grille.(x).(y+1) <- force_charriage;
            grille.(x).(y) <- qte - force_charriage
        | qte_bas ->
            grille.(x).(y+1) <- qte_bas + force_charriage;
            grille.(x).(y) <- qte - force_charriage
        )
  | _ -> ()
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

let rec simuler_dynamique_avec_vent grille vent =
  match appliquer_regle_vent grille vent with
  | Some nouvelle_grille ->
      (* Effacer l'écran *)
      print_string "\027[2J";  (* code ANSI pour effacer l'écran *)
      print_string "\027[H";   (* se replacer en haut à gauche *)
      affichage_grille nouvelle_grille;
      simuler_dynamique_avec_vent nouvelle_grille vent
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

let sauvegarder_etape_cumulative oc etape grille =
  Printf.fprintf oc "Étape %d:\n" etape;
  Array.iter (fun ligne ->
      Array.iter (fun valeur -> Printf.fprintf oc "%d " valeur) ligne;
      Printf.fprintf oc "\n"
    ) grille;
  Printf.fprintf oc "\n%!";
;;

(* Vérification du programme *)

let compteur grille = 
	let cpt = ref 0 in
	let n,m = (Array.length grille, Array.length grille.(0)) in 
	for i = 0 to (n-1) do
		for j = 0 to (m-1) do
			cpt := !cpt + grille.(i).(j)
		done;
	done;
	!cpt;
;;

(* Programme principal *)
let () =
  let grille = init_grille 30 30 in

  (* Dépôt de sable *)
  for i = 5 to 25 do
    grille.(i).(22) <- 1000;
  done;

  Printf.printf "Grille initiale :\n";
  affichage_grille grille;

  let vent = Vent.Est in
  let resultat = simuler_dynamique_avec_vent grille vent in

  for x = 0 to (Array.length resultat - 1) do
    for y = 0 to (Array.length resultat.(0) - 1) do
		appliquer_saltation resultat x y
	 done;
  done;

  for x = 0 to (Array.length resultat - 1) do
    for y = 0 to (Array.length resultat.(0) - 2) do  (* on s'arrête avant que l'on ne puisse plus transférer plus bas *)
      appliquer_charriage resultat x y 2
    done;
  done;

  (* Ouverture du fichier de sortie *)
  let out = open_out "historique_simulation.txt" in

  (* Initialisation de la simulation *)
  let etape = ref 0 in
  let g = ref (Array.map Array.copy grille) in

  (* Simulation dynamique avec export à chaque étape *)
  while true do
    sauvegarder_etape_cumulative out !etape !g;

    match appliquer_regle_vent !g vent with
    | Some nouvelle ->
        incr etape;
        g := nouvelle;
    | None ->
        Printf.fprintf out "Simulation terminée à l'étape %d\n" !etape;
        close_out out;
        exit 0
  done;

  Printf.printf "Grille après simulation :\n";
  affichage_grille resultat;
  let total = compteur resultat in
  Printf.printf "Total de sable dans la grille : %d\n" total;
  sauvegarder_grille resultat "pilat_ocaml.txt";
  save_to_file "pilat_ocaml.txt" resultat
;;