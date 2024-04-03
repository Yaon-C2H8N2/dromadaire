open Printf

(* Fonction pour lire l'entête PPM (P6) et retourner les dimensions *)
let lire_entete ic =
  ignore (input_line ic);  (* Ignore la ligne de type "P6" *)
  let dimensions = input_line ic in
  Scanf.sscanf dimensions "%d %d" (fun w h -> (w, h))

(* Fonction pour afficher les valeurs RGB d'un pixel *)
let afficher_pixel ic =
  let r = input_byte ic in
  let g = input_byte ic in
  let b = input_byte ic in
  printf "R: %d, G: %d, B: %d\n" r g b

(* Fonction principale *)
let () =
  let chemin_fichier = "input.ppm" in
  let ic = open_in_bin chemin_fichier in  (* Notez open_in_bin ici *)
  let (largeur, hauteur) = lire_entete ic in
  ignore (input_line ic);  (* Ignore la ligne avec la valeur maximale de couleur *)
  let total_pixels = ref 0 in
  try
    for _ = 1 to hauteur do
      for _ = 1 to largeur do
        afficher_pixel ic;
        incr total_pixels;
      done;
    done;
    close_in ic;
    printf "Nombre total de pixels lus: %d\n" !total_pixels;
  with End_of_file ->
    close_in_noerr ic;
    failwith "Fin de fichier atteinte prématurément."
