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

(* Fonction pour écraser le dernier bit d'un octet avec un bit spécifié *)
let ecraser_dernier_bit octet bit_message =
  match bit_message with
  | '1' -> octet lor 1   (* Met le dernier bit à 1 *)
  | '0' -> octet land 0xFE (* Met le dernier bit à 0 (0xFE = 11111110 en binaire) *)
  | _ -> failwith "Le bit de message doit être '0' ou '1'"  (* Gère le cas d'erreur *)


(* Fonction pour insérer un message dans un fichier image PPM *)
let inserer_message chemin_fichier message =
  (* Ouverture des fichiers d'entrée et de sortie *)
  let ic = open_in_bin chemin_fichier in
  let oc = open_out_bin "output.ppm" in

  (* Lecture et réécriture de l'en-tête du fichier PPM *)
  let (largeur, hauteur, max_val) = lire_entete ic in
  Printf.fprintf oc "P6\n%d %d\n%s\n" largeur hauteur max_val;

  (* Calcul de la taille totale des données de l'image *)
  let taille_image = largeur * hauteur * 3 in
  let buffer = Bytes.create taille_image in
  really_input ic buffer 0 taille_image;  (* Chargement de l'image dans un buffer *)

  (* Conversion du message en chaîne binaire *)
  let binary_message = string_to_binary_string message in
  let message_length = String.length binary_message in
  let binary_length = int_to_fixed_length_binary_string_30 message_length in
  (* Concaténation de la longueur du message et du message lui-même *)
  let full_message = binary_length ^ binary_message in
  let message_index = ref 0 in

  (* Parcours de chaque octet de l'image et insertion du message bit par bit *)
  for i = 0 to taille_image - 1 do
    if !message_index < String.length full_message then
      let octet = Char.code (Bytes.get buffer i) in
      let bit_message = full_message.[!message_index] in
      incr message_index;  (* Incrémente l'index du message *)
      Bytes.set buffer i (Char.chr (ecraser_dernier_bit octet bit_message));
  done;

  (* Écriture du buffer modifié dans le fichier de sortie *)
  output_bytes oc buffer;

  (* Fermeture des fichiers *)
  close_in ic;
  close_out oc

(* Définition des types pour la configuration RSA, les clés publiques et privées, le texte en clair et le texte chiffré *)
type rsa_config = { p_len : int; q_len : int; e_len : int }
type public_key = { n : Z.t; e : Z.t }
type private_key = { n : Z.t; p : Z.t; q : Z.t; e : Z.t; d : Z.t }
type plaintext = { message : Z.t; types : string }
type ciphertext = { c : Z.t; types : string }

(* Initialisation du générateur de nombres aléatoires *)
let () = Random.self_init ()

(* Fonction pour vérifier si deux nombres sont copremiers *)
let coprime a b = Z.gcd a b = Z.one

(* Calculer φ(n) pour une liste de nombres premiers (n = produit des nombres premiers dans la liste) *)
let rec prime_phi plist =
  match plist with
  | [] -> Z.one
  | p :: plist' -> Z.mul (Z.sub p Z.one) (prime_phi plist')

(* Exponentiation modulaire *)
let mod_exp a b n = Z.powm a b n

(* Inverse modulaire multiplicatif *)
let mod_minv a n = Z.invert a n

(* Générer un grand entier aléatoire de longueur spécifiée *)
let z_gen (len : int) =
  let rec z_gen' len acc =
    if len = 0 then acc
    else
      z_gen' (len - 1)
        (Z.add (Z.mul acc (Z.of_int 10)) (Z.of_int (Random.int 10)))
  in
  z_gen' len Z.zero

(* Test de primalité de Miller-Rabin avec un nombre configurable d'essais *)
let miller_rabin_test ?(trails = 50) n =
  let rec get_factor_q num =
    if Z.(mod) num (Z.of_int 2) = Z.zero then
      get_factor_q (Z.div num (Z.of_int 2))
    else num
  in
  if Z.of_int 2 = n then true
  else
    let q = get_factor_q (Z.sub n Z.one) in
    let rec miller_rabin_test' trails =
      if trails = 0 then true
      else
        let a =
          Z.of_int64
            (Random.int64 (Z.to_int64 (Z.min n (Z.of_int64 Int64.max_int))))
        in
        let rec miller_rabin_test'' expp =
          if Z.abs (Z.powm a expp n) = Z.one then miller_rabin_test' (trails - 1)
          else if expp = Z.sub n Z.one then false
          else if Z.(mod) (Z.powm a expp n) n = Z.sub n Z.one then
            miller_rabin_test'' (Z.mul expp (Z.of_int 2))
          else false
        in
        miller_rabin_test'' q
    in
    miller_rabin_test' trails

(* Générer un nombre premier d'une longueur donnée *)
let prime_gen ?(trails = 50) len =
  let rec prime_gen' len =
    let p = z_gen len in
    if miller_rabin_test ~trails p then p else prime_gen' len
  in
  prime_gen' len

(* Générer un nombre coprime à phi (pour l'exposant public e) *)
let e_gen rc phi =
  let rec e_gen' phi =
    let e = z_gen rc.e_len in
    if coprime e phi then e else e_gen' phi
  in
  e_gen' phi

(* Générer l'exposant privé d en utilisant l'inverse modulaire *)
let d_gen e phi = mod_minv e phi

(* Générer une clé publique à partir d'une clé privée *)
let public_key_gen pk = { n = pk.n; e = pk.e }

(* Encoder un texte clair en chaîne de caractères en un grand entier *)
let plaintext_input_string s =
  let rec encode acc chars = match chars with
    | [] -> acc
    | h :: t -> encode (Z.add (Z.mul acc (Z.of_int 256)) (Z.of_int (Char.code h))) t
  in
  let encoded = encode Z.zero (List.of_seq (String.to_seq s)) in
  { message = encoded; types = "String" }

(* Chiffrer un texte clair en utilisant la clé publique *)
let plaintext_encrypt pt (pk : public_key) =
  let c = Z.powm pt.message pk.e pk.n in
  { c = c; types = pt.types }

(* Fonction d'aide pour écrire une chaîne dans un fichier *)
let write_to_file ~filename ~content =
  let channel = open_out filename in
  output_string channel content;
  close_out channel

(* Sérialiser une clé privée en format chaîne *)
let serialize_private_key pk =
  Printf.sprintf "n=%s\np=%s\nq=%s\ne=%s\nd=%s"
    (Z.to_string pk.n) (Z.to_string pk.p) (Z.to_string pk.q)
    (Z.to_string pk.e) (Z.to_string pk.d)

(* Sérialiser un texte chiffré en format chaîne *)
let serialize_ciphertext c =
  Z.to_string c

(* Fonctions mises à jour pour générer une clé privée, la sauvegarder dans un fichier, puis chiffrer un message *)
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

let encrypt_and_return_message message rc =
  let pk = private_key_gen_and_save rc "private_key.txt" in
  let public_key = public_key_gen pk in
  let block_size = (Z.numbits pk.n) / 8 - 1 in

  (* La fonction récursive d'encodage des blocs *)
  let rec encode_blocks msg acc =
    if msg = "" then List.rev acc
    else
      let block = String.sub msg 0 (min block_size (String.length msg)) in
      let remaining = String.sub msg (min block_size (String.length msg)) (String.length msg - min block_size (String.length msg)) in
      let encoded = plaintext_input_string block in
      let encrypted = plaintext_encrypt encoded public_key in
      encode_blocks remaining (serialize_ciphertext encrypted.c :: acc)
  in

  String.concat " " (encode_blocks message [])

(* Configuration RSA par défaut *)
let rsa_config = { p_len = 256; q_len = 256; e_len = 65537 }


let () =
  let channel = open_in "msg.txt" in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  let rsa_message = encrypt_and_return_message content rsa_config in
  inserer_message "input.ppm" rsa_message;
  Printf.printf "Message chiffré et inséré dans l'image\n";;