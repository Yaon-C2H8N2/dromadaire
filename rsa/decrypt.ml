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
  (* Debug: Print decrypted characters to see their order *)
  List.iter (fun c -> match c with
    | Some ch -> print_char ch
    | None -> ()) char_options;
  print_newline ();
  char_options

(* Main Function *)
let main () =
  let encrypted_input = "2317637460346404933593423330597241144302990784407205300402957683433776274364661661947229842369990441072745302139221218813953230498219424226646680136941550262741190307045493444594891827449920079263702860425560345828325573441621469737961001532594468466504825288637756305808554374430751857079265290098339765803572477177818367045705239304298297379081872477089669583617329474338977458691735715925756707337050162032512774537763549587808127477857213994910946003976865134088537094290692829588993636366467768650947655509 4439148513599043508656394648393197978375884814509087115688205086257229828378880604995769269266809242160768372633423046578136755948454361965331681233264805062683310835261677843903281142933862883387255971819222620035030285917053843820782789885420491103531877614766924245264597732445974659521939537256370586836546049090035007408784886842146529835372989553751309029721263894005437513939385713967083464110854996734399144806543021531208454190191675409771660768375876609732524393653823160616130419837859150183907120977 4488837847969615899691489638309366228561068500090539223292972419773723117945931192690555543255596564552043504936928121510532413261974922997284473426765188040934187182788809577541428452394587811338332135197359024266297329334017950093868974748082330980477513242837168196967009885017013966524169960727188017924209334062058748052742942440796034727239426922035099421455466079357306320425935575498620278710830820201641978590610530670297013874215379373468780458112045899399012328964328325360658360908077219740359586256 1620366696992764873617156217044042072335661527971856126525908941859873039752076585853942520177442672040862611968487710382014151512960153682027194499089487247323002244176996272160610086875447568266772324232587324122936040811326590925862033550514191344452604259822535886063384556728263227716053234389599512085100901590546969815267920675752601346373704957978594428350566666315380407002697862470040453112960456905099809908285398026157964583713492806751696010679543615217912677960788248334928172400887118775017560663 491024146417485300727171912887308532911213303098470940338800745285072762089410770938612887110248991320907132740453598482034528709109034805053931708883705465323694798963979781114294398245760914960758043171678912166643797316080709924604289469701021387320937831643744787431520372511950728541473592537684784377594634384204343567810707057034726763569889646065220924837275000422001301795582897026188619750842433699796783841973241750349143315696760086104445484308275215936223115414474135818084323814141695743125296924 2133320753519759946046718508843136032688922115599440688674385461382516900366996601116697302982263031747929411460126945157517416678880397136050895828067188080080470446366896645006629098061392075542635371537964360353533152263903665380419800016714133753083559475791309306263850041498934895230246648633095425273249039525777820005542296247495318953627950247952700940964704358505152794320571896862201872432589706971228191823761221692064156417106884003908204987583616401624481135755281278766150630131135923699491222409 2308064468230126765625620137250612861457374759554700520784406414086282680584190517305859728799504928227148105720723761347177114576930938487159923855933729584956437553295962198138755941752408923715907625880454810043208525322491631651570049987370116361766079953934792828094201865764440701588845342426949415343267441406930613837366108458178311108429136449610973340547528345604752940486269663900400470419594624136077562318429698596958275839431693770859535499473120291639495129944454254961660701448195637033537160603 1118355213530925537282808343650423710192108332080282072587533338952880418772156621138563612242821033570839254716025415743947084508462092889449664086657328698064092887723996096765364779152103907456598964068452761816886404905128834439001835538044628545896704380953029437465974466487623990938637985597754843445238031888569781283322323213178430598910549707792144587235078670585924378941725635094569906428750922340263027797165421241652557085310567227220676350701534297072187364412217319888000355780854906641491662568 491024146417485300727171912887308532911213303098470940338800745285072762089410770938612887110248991320907132740453598482034528709109034805053931708883705465323694798963979781114294398245760914960758043171678912166643797316080709924604289469701021387320937831643744787431520372511950728541473592537684784377594634384204343567810707057034726763569889646065220924837275000422001301795582897026188619750842433699796783841973241750349143315696760086104445484308275215936223115414474135818084323814141695743125296924 4488837847969615899691489638309366228561068500090539223292972419773723117945931192690555543255596564552043504936928121510532413261974922997284473426765188040934187182788809577541428452394587811338332135197359024266297329334017950093868974748082330980477513242837168196967009885017013966524169960727188017924209334062058748052742942440796034727239426922035099421455466079357306320425935575498620278710830820201641978590610530670297013874215379373468780458112045899399012328964328325360658360908077219740359586256 4488837847969615899691489638309366228561068500090539223292972419773723117945931192690555543255596564552043504936928121510532413261974922997284473426765188040934187182788809577541428452394587811338332135197359024266297329334017950093868974748082330980477513242837168196967009885017013966524169960727188017924209334062058748052742942440796034727239426922035099421455466079357306320425935575498620278710830820201641978590610530670297013874215379373468780458112045899399012328964328325360658360908077219740359586256 5120951266341118920526798326285484992337893273707594982943522513121561765297941164981952365194693615654587498077483759882390272952423598089487157359922219943469437105357872175684846112471055511835263404484327280777500202967912545848457911327100265821074929009839951278081365717478008675666401723775580813596089632371633017087503964011537277738584740276689090490945949401796331263277685150787232415472890077273572258244949657899788062149663341845770446215482718289928714245429965555134672121609918273774669422018 1063290962213963451249969405661267700627269361470565365064410382686289545732697174766443882990155163274121819809939361208282872224959241712077422580050736027723597546750473984310656520922803259857773128540544571639427209204677429505902861136322125142278269997502713647696344790592915302510957766152777175087247462893065682738073595417298117198215948192102536104373308897422367657077222764625796731180800109221018579691477520872723546060173421113167019313265838633334968110577810266604219417611045355785002656556" in
  let ciphertext = deserialize_ciphertext encrypted_input in
  let private_key_content = read_from_file "private_key.txt" in
  let sk = deserialize_private_key private_key_content in
  let decrypted_chars = decrypt_message ciphertext sk in
  let message = decrypted_chars |> List.filter_map Fun.id |> List.rev |> List.to_seq |> String.of_seq in
  print_endline "Decrypted Message:";
  print_endline message

let () = main ()



