val count : ('a -> bool) -> 'a list -> int
val find_all_index : ('a -> bool) -> 'a list -> int list
val rfind_index : ('a -> bool) -> 'a list -> int option
val remove_duplicates : ('a -> 'a -> bool) -> 'a list -> 'a list
val contains_duplicates : ('a -> 'a -> bool) -> 'a list -> bool
val without_item_at : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val with_item_at : int -> 'a -> 'a list -> 'a list
val swap : int -> int -> 'a list -> 'a list
val par_map : ('a -> 'b) -> 'a list -> 'b list
val par_filter : ('a -> bool) -> 'a list -> 'a list
