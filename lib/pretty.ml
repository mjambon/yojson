
let pp_list sep ppx out l =
  let pp_sep out () = Format.fprintf out "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx out l

let rec format std (out:Format.formatter) (x:t) : unit =
  match x with
    | `Null -> Format.pp_print_string out "null"
    | `Bool x -> Format.pp_print_bool out x
#ifdef INT
    | `Int x -> Format.pp_print_string out (json_string_of_int x)
#endif
#ifdef FLOAT
    | `Float x ->
        let s =
          if std then std_json_string_of_float x
          else json_string_of_float x
        in
        Format.pp_print_string out s
#endif
#ifdef STRING
    | `String s -> Format.pp_print_string out (json_string_of_string s)
#endif
#ifdef INTLIT
    | `Intlit s -> Format.pp_print_string out s
#endif
#ifdef FLOATLIT
    | `Floatlit s -> Format.pp_print_string out s
#endif
#ifdef STRINGLIT
    | `Stringlit s -> Format.pp_print_string out s
#endif
    | `List [] -> Format.pp_print_string out "[]"
    | `List l -> Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]" (pp_list "," (format std)) l
    | `Assoc [] -> Format.pp_print_string out "{}"
    | `Assoc l ->
      Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field std)) l
#ifdef TUPLE
    | `Tuple l ->
        if std then
          format std out (`List l)
        else
          if l = [] then
            Format.pp_print_string out "()"
          else
            Format.fprintf out "(@,%a@;<0 -2>)" (pp_list "," (format std)) l
#endif
#ifdef VARIANT
    | `Variant (s, None) ->
        if std then
#ifdef STRING
          let representation = `String s in
#else
          let representation = `Stringlit s in
#endif
          format std out representation
        else
          Format.fprintf out "<%s>" (json_string_of_string s)

    | `Variant (s, Some x) ->
        if std then
#ifdef STRING
          let representation = `String s in
#else
          let representation = `Stringlit s in
#endif
          format std out (`List [ representation; x ])
        else
          let op = json_string_of_string s in
          Format.fprintf out "<@[<hv2>%s: %a@]>" op (format std) x
#endif

and format_field std out (name, x) =
  Format.fprintf out "@[<hv2>%s: %a@]" (json_string_of_string name) (format std) x

let pp ?(std = false) out x =
  if std && not (is_object_or_array x) then
    json_error
      "Root is not an object or array as requested by the JSON standard"
  else
    Format.fprintf out "@[<hv2>%a@]" (format std) (x :> t)

let to_string ?std x =
  Format.asprintf "%a" (pp ?std) x

let to_channel ?std oc x =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@?" (pp ?std) x
