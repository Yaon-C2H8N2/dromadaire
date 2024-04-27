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

(* Fonction pour convertir un entier en chaîne binaire de longueur fixe (30 bits ici) *)
let int_to_fixed_length_binary_string_30 n =
  let rec aux n acc count =
    if count = 30 then acc
    else aux (n lsr 1) ((string_of_int (n land 1)) ^ acc) (count + 1)
  in
  aux n "" 0

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

let inserer_message chemin_fichier message =
  let ic = open_in_bin chemin_fichier in
  let oc = open_out_bin "output.ppm" in
  let (largeur, hauteur, max_val) = lire_entete ic in
  Printf.fprintf oc "P6\n%d %d\n%s\n" largeur hauteur max_val;  (* Réécrit l'entête complète *)

  let taille_image = largeur * hauteur * 3 in  (* Calcule la taille de l'image *)
  let buffer = Bytes.create taille_image in  (* Crée un buffer de bytes mutable *)
  really_input ic buffer 0 taille_image;  (* Lit l'image dans le buffer *)

  let binary_message = string_to_binary_string message in
  let message_length = String.length binary_message in
  let binary_length = int_to_fixed_length_binary_string_30 message_length in
  let full_message = binary_length ^ binary_message in
  let message_index = ref 0 in

  for i = 0 to taille_image - 1 do
    if !message_index < String.length full_message then
      let octet = Char.code (Bytes.get buffer i) in
      let bit_message = full_message.[!message_index] in
      incr message_index;
      Bytes.set buffer i (Char.chr (ecraser_dernier_bit octet bit_message));
  done;

  output_bytes oc buffer;  (* Écrit le buffer modifié dans le fichier de sortie *)

  close_in ic;
  close_out oc



type rsa_config = { p_len : int; q_len : int; e_len : int }
type public_key = { n : Z.t; e : Z.t }
type private_key = { n : Z.t; p : Z.t; q : Z.t; e : Z.t; d : Z.t }
type plaintext = { message : Z.t list; types : string }
type cipertext = { c : Z.t list; types : string }


let () = Random.self_init ()
(* Helper Fuction *)
(* check gcd = 1 *)
let coprime a b = Z.gcd a b = Z.one

let rec prime_phi plist =
  match plist with
  | [] -> Z.one
  | p :: plist' -> Z.mul (Z.sub p Z.one) (prime_phi plist')

let mod_exp a b n = Z.powm a b n
let mod_minv a n = Z.invert a n

(* concat String to int *)
let z_gen (len : int) =
  let rec z_gen' len acc =
    if len = 0 then acc
    else
      z_gen' (len - 1)
        (Z.add (Z.mul acc (Z.of_int 10)) (Z.of_int (Random.int 10)))
  in
  z_gen' len Z.zero


let miller_rabin_test ?(trails = 50) n =
  let rec get_factor_q num =
    if Z.( mod ) num (Z.of_int 2) = Z.zero then
      get_factor_q (Z.div num (Z.of_int 2))
    else num
  in
  if Z.of_int 2 = n then true
  else
    let q = get_factor_q (Z.sub n Z.one) in
    let rec miller_rabin_test' trails =
      if trails = 0 then true
      else
        (* TODO, MORE RANDOM *)
        let a =
          Z.of_int64
            (Random.int64 (Z.to_int64 (Z.min n (Z.of_int64 Int64.max_int))))
        in
        let rec miller_rabin_test'' expp =
          if Z.abs (Z.powm a expp n) = Z.one then miller_rabin_test' (trails - 1)
          else if expp = Z.sub n Z.one then false
          else if Z.( mod ) (Z.powm a expp n) n = Z.sub n Z.one then
            miller_rabin_test'' (Z.mul expp (Z.of_int 2))
          else false
        in
        miller_rabin_test'' q
    in
    miller_rabin_test' trails

let prime_gen ?(trails = 50) len =
  let rec prime_gen' len =
    let p = z_gen len in
    if miller_rabin_test ~trails p then p else prime_gen' len
  in
  prime_gen' len

let e_gen rc phi =
  let rec e_gen' phi =
    let e = z_gen rc.e_len in
    if coprime e phi then e else e_gen' phi
  in
  e_gen' phi

let d_gen e phi = mod_minv e phi

let private_key_gen rc =
  let p = prime_gen rc.p_len in
  let q = prime_gen rc.q_len in
  let phi = Z.mul (Z.sub p Z.one) (Z.sub q Z.one) in
  let e = e_gen rc phi in
  let d = d_gen e phi in
  { n = Z.mul p q; p; q; e; d }

let public_key_gen pk = { n = pk.n; e = pk.e }

let config_input () =
  let _ = print_string "Please input p_len: " in
  let p_len = read_int () in
  let _ = print_string "Please input q_len: " in
  let q_len = read_int () in
  let _ = print_string "Please input e_len: " in
  let e_len = read_int () in
  { p_len; q_len; e_len }

let plaintext_input () =
  let _ = print_string "Please input plaintext: " in
  let m = read_line () in
  let types = "String" in
  {
    message =
      List.map (fun x -> Z.of_int (Char.code x)) (String.to_seq m |> List.of_seq);
    types;
  }

let plaintext_input_string s =
  {
    message =
      List.map (fun x -> Z.of_int (Char.code x)) (String.to_seq s |> List.of_seq);
    types = "String";
  }

let plaintext_encrypt pt (pk : public_key) =
  let rec plaintext_encrypt' m acc =
    match m with
    | [] -> acc
    | h :: t -> plaintext_encrypt' t (Z.powm h pk.e pk.n :: acc)
  in
  { c = plaintext_encrypt' pt.message []; types = pt.types }

let cipertext_decrypt ct sk =
  let rec cipertext_decrypt' c acc =
    match c with
    | [] -> acc
    | c :: c' -> cipertext_decrypt' c' (Z.powm c sk.d sk.n :: acc)
  in
  { message = cipertext_decrypt' ct.c []; types = ct.types }

let cipertext_output ct =
  let _ = print_endline "Cipertext: " in
  let rec cipertext_output' c acc =
    match c with
    | [] -> acc
    | c :: c' -> cipertext_output' c' (acc ^ Z.to_string c)
  in
  let _ = print_string (cipertext_output' ct.c "") in
  print_endline ""

let plaintext_output pt =
  let _ = print_endline "Plaintext: " in
  let rec plaintext_output' m acc =
    match m with
    | [] -> acc
    | m :: m' ->
        plaintext_output' m' (acc ^ Char.escaped (Char.chr (Z.to_int m)))
  in
  let _ = print_string (plaintext_output' pt.message "") in
  print_endline ""


(* Helper Functions *)
let write_to_file ~filename ~content =
  let channel = open_out filename in
  output_string channel content;
  close_out channel

let serialize_private_key pk =
  Printf.sprintf "n=%s\np=%s\nq=%s\ne=%s\nd=%s"
    (Z.to_string pk.n) (Z.to_string pk.p) (Z.to_string pk.q)
    (Z.to_string pk.e) (Z.to_string pk.d)

let serialize_ciphertext ct =
  String.concat " " (List.map Z.to_string ct)

(* Main RSA Functions Updated *)
let private_key_gen_and_save rc filename =
  let p = prime_gen rc.p_len in
  let q = prime_gen rc.q_len in
  let phi = Z.mul (Z.sub p Z.one) (Z.sub q Z.one) in
  let e = e_gen rc phi in
  let d = d_gen e phi in
  let pk = { n = Z.mul p q; p; q; e; d } in
  let serialized_pk = serialize_private_key pk in
  write_to_file ~filename ~content:serialized_pk;
  pk

(* Function to generate and print encrypted message, and return the serialized ciphertext *)
let encrypt_and_return_message message rc =
  let pk = private_key_gen_and_save rc "private_key.txt" in
  let public_key = public_key_gen pk in
  let plaintext = plaintext_input_string message in
  let ciphertext = plaintext_encrypt plaintext public_key in
  let serialized_ct = serialize_ciphertext ciphertext.c in
  print_endline ("Encrypted Text: " ^ serialized_ct);
  serialized_ct  (* Return the serialized ciphertext *)

(* Example Usage *)
let rsa_config = { p_len = 256; q_len = 256; e_len = 32 }

(* Example of usage *)
let () =
  let message = "salut les gars" in
  let rsa_message = encrypt_and_return_message message rsa_config in
  inserer_message "input.ppm" rsa_message


