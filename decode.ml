(* ... Autres fonctions ... *)
let lire_entete ic =
  ignore (input_line ic);  (* Ignore la ligne de type "P6" *)
  let dimensions = input_line ic in
  let (largeur, hauteur) = Scanf.sscanf dimensions "%d %d" (fun w h -> (w, h)) in
  let max_val = input_line ic in  (* Lit la valeur maximale des couleurs *)
  (largeur, hauteur, max_val)

(* Fonction pour lire le dernier bit de l'octet *)
let lire_dernier_bit octet =
  string_of_int (octet land 1)

(* Fonction pour convertir une chaîne binaire en caractère *)
let binary_to_char bin =
  Char.chr (int_of_string ("0b" ^ bin))


(* Fonction pour décoder le message caché dans l'image *)
let decoder_message chemin_fichier =
  let ic = open_in_bin chemin_fichier in
  let (_, _, _) = lire_entete ic in  (* Ignore l'entête *)

  let rec lire_bits n acc =
    if n = 0 then acc
    else
      let octet = input_byte ic in
      let bit = lire_dernier_bit octet in
      lire_bits (n - 1) (acc ^ bit)
  in

  (* Lire les 30 bits pour la longueur du message *)
  let longueur_bits = lire_bits (10 * 3) "" in
  let longueur_message = int_of_string ("0b" ^ longueur_bits) in
  Printf.printf "Longueur du message: %d bits\n" longueur_message;

  (* Lire les bits du message *)
  let message_bits = lire_bits longueur_message "" in

  close_in ic;

  (* Convertir les bits en message *)
  let rec convertir_message bits =
    if String.length bits < 8 then ""
    else
      let char_bin = String.sub bits 0 8 in
      let reste = String.sub bits 8 (String.length bits - 8) in
      let char = binary_to_char char_bin in
      String.make 1 char ^ convertir_message reste
  in

  convertir_message message_bits

(* Exemple d'utilisation *)
let () =
  let message = decoder_message "output.ppm" in
  Printf.printf "Message décodé : %s\n" message


  
