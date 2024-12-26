val count : ('a -> bool) -> 'a list -> int
val count_from_index : ('a -> bool) -> int -> 'a list -> int
val find_all_index : ('a -> bool) -> 'a list -> int list
val rev_index : 'a list -> int -> int
val find_nth_index : ('a -> bool) -> int -> 'a list -> int option
val find_index_rev : ('a -> bool) -> 'a list -> int option
val find_nth_index_rev : ('a -> bool) -> int -> 'a list -> int option
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
