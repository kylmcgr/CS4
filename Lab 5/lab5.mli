(* A *)

val fibonacci : int -> int
val fibonacci2 : int -> int
val bubble_sort : 'a array -> unit

(* B *)

val meters_per_foot : float

val get_meters :
  [< `Foot of float | `Inch of float | `Meter of float ] -> float

val length_add :
  [< `Foot of float | `Inch of float | `Meter of float ] ->
  [< `Foot of float | `Inch of float | `Meter of float ] ->
  [> `Meter of float ]

val grams_per_slug : float

val get_grams :
  [< `Gram of float | `Kilo of float | `Slug of float ] -> float

val mass_add :
  [< `Gram of float | `Kilo of float | `Slug of float ] ->
  [< `Gram of float | `Kilo of float | `Slug of float ] ->
  [> `Gram of float ]

val get_seconds :
  [< `Day of float | `Hour of float | `Minute of float | `Second of float ] ->
  float

val time_add :
  [< `Day of float | `Hour of float | `Minute of float | `Second of float ] ->
  [< `Day of float | `Hour of float | `Minute of float | `Second of float ] ->
  [> `Second of float ]

val unit_add :
  [> `Length of [< `Foot of float | `Inch of float | `Meter of float ]
   | `Mass of [< `Gram of float | `Kilo of float | `Slug of float ]
   | `Time of
       [< `Day of float
        | `Hour of float
        | `Minute of float
        | `Second of float ] ] ->
  [> `Length of [< `Foot of float | `Inch of float | `Meter of float ]
   | `Mass of [< `Gram of float | `Kilo of float | `Slug of float ]
   | `Time of
       [< `Day of float
        | `Hour of float
        | `Minute of float
        | `Second of float ] ] ->
  [> `Length of [> `Meter of float ]
   | `Mass of [> `Gram of float ]
   | `Time of [> `Second of float ] ]

(* C *)

val make_gram :
  float ->
  (< 
     add : < get_grams : float; unit_type : [> `Gram | `Slug ]; .. > -> 'a;
     compatible : < unit_type : [> `Gram | `Slug ]; .. > -> bool;
     get_grams : float; 
     get_slugs : float; 
     unit_type : [> `Gram ] 
   >
   as 'a)

val make_number :
  int ->
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a)

val make_variable :
  string ->
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a)

val make_sum :
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a) ->
  'a -> 'a

val make_product :
  (< 
     derive    : string -> 'a; 
     evaluate  : string -> int -> 'a; 
     is_number : bool;
     is_zero   : bool; 
     show      : string; 
     value     : int 
   >
   as 'a) ->
  'a -> 'a

val evaluate : < evaluate : 'a -> 'b -> 'c; .. > -> 'a -> 'b -> 'c

val show : < show : 'a; .. > -> 'a

val differentiate : < derive : 'a -> 'b; .. > -> 'a -> 'b

