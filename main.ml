open Printf

(* Fonction pour convertir un caractère en chaîne binaire de 8 bits *)
let char_to_binary_string c =
  let n = Char.code c in
  let rec aux n acc count =
    if count = 8 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0

(* Fonction pour convertir une chaîne de caractères en chaîne binaire *)
let string_to_binary_string s =
  let rec aux chars acc =
    match chars with
    | [] -> acc
    | c::cs -> aux cs (acc ^ (char_to_binary_string c))
  in
  aux (String.to_seq s |> List.of_seq) ""

(* Fonction pour convertir un entier en chaîne binaire de 8 bits *)
let int_to_binary_string n =
  let rec aux n acc count =
    if count = 8 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0

(* Fonction pour lire l'entête PPM (P6) et retourner les dimensions et la valeur max de couleur *)
let lire_entete ic =
  ignore (input_line ic);  (* Ignore la ligne de type "P6" *)
  let dimensions = input_line ic in
  let (largeur, hauteur) = Scanf.sscanf dimensions "%d %d" (fun w h -> (w, h)) in
  let max_val = input_line ic in  (* Lit la valeur maximale des couleurs *)
  (largeur, hauteur, max_val)



(* Fonction pour afficher les valeurs RGB d'un pixel en binaire *)
let afficher_pixel ic =
  let r = input_byte ic in
  let g = input_byte ic in
  let b = input_byte ic in
  printf "R: %s, G: %s, B: %s\n" (int_to_binary_string r) (int_to_binary_string g) (int_to_binary_string b)

(* Fonction pour écraser le dernier bit d'un octet avec un bit de message *)
let ecraser_dernier_bit octet bit_message =
  match bit_message with
  | '1' -> octet lor 1   (* Met le dernier bit à 1 *)
  | '0' -> octet land 0xFE (* Met le dernier bit à 0 *)
  | _ -> failwith "Le bit de message doit être '0' ou '1'"


(* Fonction pour convertir un entier en chaîne binaire de longueur fixe *)
let int_to_fixed_length_binary_string n length =
  let binary = int_to_binary_string n in
  String.make (length - String.length binary) '0' ^ binary

(* Fonction pour insérer le message dans l'image *)
let inserer_message chemin_fichier message =
  let ic = open_in_bin chemin_fichier in
  let oc = open_out_bin "output.ppm" in
  let (largeur, hauteur, max_val) = lire_entete ic in
  Printf.fprintf oc "P6\n%d %d\n%s\n" largeur hauteur max_val;  (* Réécrit l'entête complète *)


  let binary_message = string_to_binary_string message in
  let message_length = String.length binary_message in
  let binary_length = int_to_fixed_length_binary_string message_length 30 in
  let full_message = binary_length ^ binary_message in
  let message_index = ref 0 in

  try
    for _ = 1 to hauteur do
      for _ = 1 to largeur do
        for _ = 1 to 3 do
          let octet = input_byte ic in
          let new_octet =
            if !message_index < String.length full_message then
              let bit_message = full_message.[!message_index] in
              incr message_index;
              ecraser_dernier_bit octet bit_message
            else
              octet
          in
          output_byte oc new_octet;
        done;
      done;
    done;
    close_in ic;
    close_out oc;
  with End_of_file ->
    close_in_noerr ic;
    close_out_noerr oc;
    failwith "Fin de fichier atteinte prématurément."

(* Exemple d'utilisation *)
let () =
  let message = "maxime" in
  inserer_message "input.ppm" message
