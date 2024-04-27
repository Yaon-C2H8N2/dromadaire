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

(* decrypt.ml *)
open Z

type private_key = {
  n : Z.t;
  p : Z.t;
  q : Z.t;
  e : Z.t;
  d : Z.t
}

(* Helper Functions *)
let read_from_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let deserialize_private_key content =
  let lines = String.split_on_char '\n' content in
  let find_value s =
    match String.split_on_char '=' s with
    | [_; value] -> Z.of_string value
    | _ -> failwith "Invalid private key format"
  in
  {
    n = find_value (List.nth lines 0);
    p = find_value (List.nth lines 1);
    q = find_value (List.nth lines 2);
    e = find_value (List.nth lines 3);
    d = find_value (List.nth lines 4);
  }

let deserialize_ciphertext s =
  List.map Z.of_string (String.split_on_char ' ' s)

let mod_exp a b n = Z.powm a b n

(* Decryption Function *)
let decrypt_message ciphertext sk =
  let decrypted_list = List.map (fun c -> mod_exp c sk.d sk.n) ciphertext in
  let char_options = List.map (fun m ->
    if Z.leq m (Z.of_int 255) then
      Some (Char.chr (Z.to_int m))
    else
      None
  ) decrypted_list in
  (* Debug: Print decrypted characters to see their order
  List.iter (fun c -> match c with
    | Some ch -> print_char ch
    | None -> ()) char_options;*)
  print_newline ();
  char_options

(* Main Function *)
let main () =
  let message = decoder_message "output.ppm" in

  let ciphertext = deserialize_ciphertext message in
  let private_key_content = read_from_file "private_key.txt" in
  let sk = deserialize_private_key private_key_content in
  let decrypted_chars = decrypt_message ciphertext sk in
  let message = decrypted_chars |> List.filter_map Fun.id |> List.rev |> List.to_seq |> String.of_seq in
  print_endline "Decrypted Message:";
  print_endline message









  
let () = main ()
