(* GADT implementation of MPSC queue   *)
exception Closed
exception Empty

type 'a t = { mutable head : 'a head_pack; tail : 'a tail_pack Atomic.t }

and ('a, _) head =
  | Cons : 'a * 'a head_pack -> ('a, [> `Cons ]) head
  | Hopen : ('a, [> `Hopen ]) head
  | Hclosed : ('a, [> `Hclosed ]) head

and 'a head_pack =
  | H : ('a, [< `Cons | `Hopen | `Hclosed ]) head -> 'a head_pack
[@@unboxed]

and ('a, _) tail =
  | Snoc : 'a tail_pack * 'a -> ('a, [> `Snoc ]) tail
  | Topen : ('a, [> `Topen ]) tail
  | Tclosed : ('a, [> `Tclosed ]) tail

and 'a tail_pack =
  | T : ('a, [< `Snoc | `Topen | `Tclosed ]) tail -> 'a tail_pack
[@@unboxed]

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make (T Topen) in
  let head = Multicore_magic.copy_as_padded @@ H Hopen in
  Multicore_magic.copy_as_padded { tail; head }

(* let push t x = t.tail <- T (Snoc (t.tail, x)) *)

let rec push backoff t x =
  match Atomic.get t.tail with
  | T Tclosed -> raise Closed
  | before ->
      let after = T (Snoc (before, x)) in
      if not (Atomic.compare_and_set t.tail before after) then
        Backoff.once backoff;
        push (Backoff.default) t x

let push t x = push Backoff.default t x

(*  *)
let rec rev_to (head : (_, [< `Cons ]) head)
    (tail : (_, [< `Snoc | `Topen ]) tail) =
  match tail with
  | Topen -> head
  | Snoc (T xs, x) -> rev_to (Cons (x, H head)) xs

let rec rev head = function
  | T Topen -> head
  | T Tclosed -> assert false
  | T (Snoc (xs, x)) -> rev (H (Cons (x, head))) xs

let rev_pop = function
  | (Snoc (tail, x) : (_, [< `Snoc ]) tail) -> rev_to (Cons (x, H Hopen)) tail

let rec append a b =
  match b with
  | H Hclosed ->
      failwith
        "This cannot happen, maybe you are running [close] concurrently with \
         another [close]"
  | H Hopen -> a
  | H (Cons (x, xs)) -> H (Cons (x, append a xs))

let pop_opt t =
  match t.head with
  | H Hclosed -> raise Closed (*Closed and empty*)
  | H (Cons (x, xs)) ->
      t.head <- xs;
      Some x
  | H Hopen -> (
      match Atomic.exchange t.tail (T Topen) with
      | T Topen -> None
      | T Tclosed ->
          failwith
            "This cannot happen, maybe you are running [close] concurrently \
             with [pop]"
      | T (Snoc _ as tail) -> (
          match rev_pop tail with
          | Cons (x, H Hopen) -> Some x
          | Cons (x, xs) ->
              t.head <- xs;
              Some x))

let pop t =
  match t.head with
  | H Hclosed -> raise Closed (*Closed and empty*)
  | H (Cons (x, xs)) ->
      t.head <- xs;
      x
  | H Hopen -> (
      match Atomic.exchange t.tail (T Topen) with
      | T Topen -> raise Empty
      | T Tclosed ->
          failwith
            "This cannot happen, maybe you are running [close] concurrently \
             with [pop]"
      | T (Snoc _ as tail) -> (
          match rev_pop tail with
          | Cons (x, H Hopen) -> x
          | Cons (x, xs) ->
              t.head <- xs;
              x))

let close t =
  match Atomic.exchange t.tail (T Tclosed) with
  | T Tclosed -> raise Closed
  | tail ->
      let y = rev (H Hclosed) tail in
      t.head <- append y t.head

let peek t =
  match t.head with
  | H (Cons (x, _)) -> x
  | H Hclosed -> raise Closed
  | H Hopen -> (
      match Atomic.get t.tail with
      | T Topen -> raise Empty
      | T Tclosed ->
          failwith
            "This cannot happen, maybe you are running [peek] concurrently \
             with [close]"
      | T (Snoc _ as tail) -> (
          match rev_pop tail with
          | Cons (x, H Hopen) -> x
          | Cons (x, _) as head ->
              t.head <- head;
              x))

let peek_opt t =
  match t.head with
  | H (Cons (x, _)) -> Some x
  | H Hclosed -> raise Closed
  | H Hopen -> (
      match Atomic.get t.tail with
      | T Topen -> None
      | T Tclosed ->
          failwith
            "This cannot happen, maybe you are running [peek] concurrently \
             with [close]"
      | T (Snoc _ as tail) -> (
          match rev_pop tail with
          | Cons (x, H Hopen) -> Some x
          | Cons (x, _) as head ->
              t.head <- head;
              Some x))

let is_empty t =
  match t.head with
  | H Hclosed -> raise Closed
  | H (Cons (_, _)) -> false
  | H Hopen -> (
      match Atomic.get t.tail with
      | T (Snoc (_, _)) -> false
      | T Topen -> true
      | T Tclosed ->
          failwith
            "This cannot happen, maybe you are running [close] concurrently \
             with [is_empty]")

let push_head t x =
  match t.head with
  | H Hclosed -> raise Closed
  | head -> t.head <- H (Cons (x, head))
