open Value

let filter pred : xd =
  { xd_init = (fun next ->
      fun acc item ->
        if pred item then next acc item
        else (acc, Xd_continue)) }

let map f : xd =
  { xd_init = (fun next ->
      fun acc item ->
        next acc (f item)) }

let take n : xd =
  { xd_init = (fun next ->
      let remaining = ref n in
      fun acc item ->
        if !remaining <= 0 then (acc, Xd_done)
        else (
          decr remaining;
          let acc', _ = next acc item in
          if !remaining <= 0 then (acc', Xd_done)
          else (acc', Xd_continue))) }

let skip n : xd =
  { xd_init = (fun next ->
      let skipped = ref 0 in
      fun acc item ->
        if !skipped < n then (incr skipped; (acc, Xd_continue))
        else next acc item) }

let unique to_key : xd =
  { xd_init = (fun next ->
      let seen = Hashtbl.create 16 in
      fun acc item ->
        let k = to_key item in
        if Hashtbl.mem seen k then (acc, Xd_continue)
        else (Hashtbl.replace seen k (); next acc item)) }

let compose a b : xd =
  { xd_init = (fun next -> a.xd_init (b.xd_init next)) }

let run (xd : xd) (source : t Seq.t) : t list =
  let emit acc item = (item :: acc, Xd_continue) in
  let step = xd.xd_init emit in
  let acc = ref [] in
  let src = ref source in
  let stopped = ref false in
  while not !stopped do
    match !src () with
    | Seq.Nil -> stopped := true
    | Seq.Cons (v, rest) ->
      src := rest;
      let acc', signal = step !acc v in
      acc := acc';
      (match signal with Xd_done -> stopped := true | Xd_continue -> ())
  done;
  List.rev !acc

let fold : 'a. xd -> ('a -> t -> 'a) -> 'a -> t Seq.t -> 'a =
  fun xd f init source ->
    let result = ref init in
    let emit acc item =
      result := f !result item;
      (acc, Xd_continue)
    in
    let step = xd.xd_init emit in
    let src = ref source in
    let stopped = ref false in
    while not !stopped do
      match !src () with
      | Seq.Nil -> stopped := true
      | Seq.Cons (v, rest) ->
        src := rest;
        let _, signal = step [] v in
        (match signal with Xd_done -> stopped := true | Xd_continue -> ())
    done;
    !result

let flatten : xd =
  { xd_init = (fun next ->
      fun acc item ->
        let items = match item with
          | Array inner -> inner
          | Seq s -> List.of_seq s
          | Xd (source, xd) -> run xd (to_seq_of source)
          | other -> [other]
        in
        let rec go acc = function
          | [] -> (acc, Xd_continue)
          | sub :: rest ->
            let acc', signal = next acc sub in
            (match signal with
             | Xd_done -> (acc', Xd_done)
             | Xd_continue -> go acc' rest)
        in
        go acc items) }

let flatmap f : xd = compose (map f) flatten
