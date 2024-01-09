open Saturn_lockfree

(* module Mpsc_queue_1 = Saturn_lockfree.Mpsc_queue_1 *)
let n_messages = 500_000

let average action =
  let n_times = 10 in
  let n_warmup = 3 in
  let total = ref 0.0 in
  for i = 1 to n_times + n_warmup do
    let start = Unix.gettimeofday () in
    action ();
    let elapsed = Unix.gettimeofday () -. start in
    if n_warmup < i then total := !total +. elapsed
  done;
  (* Printf.printf "%f" (!total /. Float.of_int n_times); *)
  !total /. Float.of_int n_times

let bench_single label create push pop =
  let elapsed =
    average @@ fun () ->
    let queue = create () in
    for i = 1 to n_messages do
      push queue i;
      pop queue
    done
  in

  Printf.printf "%s  (one domain): %f per second\n%!" label
    (Float.of_int n_messages /. elapsed)

let bench_single_length length label create push pop =
  let elapsed =
    average @@ fun () ->
    let queue = create () in
    for i = 1 to n_messages do
      if i > n_messages - length then pop queue
      else if i > length && i mod 2 = 1 then pop queue
      else push queue i
    done
  in

  Printf.printf "%s  (one domain with queue length %d): %f per second\n%!" label
    length
    (Float.of_int n_messages /. elapsed)

let bench_single_seq list label create push pop =
  let elapsed =
    average @@ fun () ->
    let queue = create () in
    let rec operation list len push pop n =
      match list with
      | [] -> ()
      | x :: xs ->
          if x = true then (
            push queue n;
            if n > 0 then operation xs (len + 1) push pop (n - 1))
          else (
            pop queue;
            if n > 0 then operation xs (len - 1) push pop (n - 1))
    in
    operation list 0 push pop n_messages
  in
  Printf.printf "%s  (one domain with operation list): %f per second\n%!" label
    (Float.of_int n_messages /. elapsed)

let rec generate_list length queue_len (list : bool list) =
  if length = 0 then list
  else if (* length here is the length of the list required *)
          length = queue_len
  then generate_list (length - 1) (queue_len + 1) (false :: list)
  else
    let rand = Random.bool () in
    if rand = true then generate_list (length - 1) (queue_len + 1) (rand :: list)
    else if queue_len = 0 then
      generate_list (length - 1) (queue_len + 1) (true :: list)
    else generate_list (length - 1) (queue_len - 1) (rand :: list)

let rec rev from_list to_list =
  match from_list with [] -> to_list | x :: xs -> rev xs (x :: to_list)

let bench_single_rand label create push pop =
  let elapsed =
    average @@ fun () ->
    let len = ref 0 in
    let queue = create () in
    for i = 1 to n_messages do
      if !len = n_messages - i + 1 then (
        len := !len - 1;
        pop queue)
      else if !len > 1000 then (
        len := !len - 1;
        pop queue)
      else if !len = 0 then (
        len := !len + 1;
        push queue i)
      else
        let rand = Random.bool () in
        if rand = true then (
          len := !len + 1;
          push queue i)
        else (
          len := !len - 1;
          push queue i)
    done
  in

  Printf.printf "%s  (one domain with rand operation): %f per second\n%!" label
    (Float.of_int n_messages /. elapsed)

let bench_single_all label create push pop is_empty push_head peek =
  let elapsed =
    average @@ fun () ->
    let len = ref 0 in
    let queue = create () in
    for i = 1 to n_messages do
      if !len = n_messages - i + 1 then pop queue
      else
        let rand = Random.int 7 in
        if rand < 2 then (
          ignore @@ push queue i;
          len := !len + 1)
        else if rand < 4 then (
          ignore @@ pop queue;
          len := !len - 1)
        else if rand < 5 then (
          ignore @@ push_head queue i;
          len := !len + 1)
        else if rand < 6 then ignore @@ is_empty queue
        else if rand < 7 then ignore @@ peek queue
    done
  in
  Printf.printf "%s  (one domain with all operations): %f per second\n%!" label
    (Float.of_int n_messages /. elapsed)

let bench_multi label create push try_pop n_producers =
  let n_messages = n_messages / n_producers in

  let elapsed =
    average @@ fun () ->
    let queue = create () in

    let others =
      List.init n_producers @@ fun _ ->
      Domain.spawn @@ fun () ->
      for i = 1 to n_messages do
        push queue i
      done
    in

    let rec consume n = if 0 < n then consume (Bool.to_int (try_pop queue)) in
    consume (n_messages * n_producers);

    List.iter Domain.join others
  in

  Printf.printf "%s (%d producers): %f per second\n%!" label n_producers
    (Float.of_int (n_messages * n_producers) /. elapsed)

let bench_multi_push_pop label create push try_pop n_producers =
  let n_messages = n_messages / n_producers in

  let elapsed =
    average @@ fun () ->
    let queue = create () in
    (* let avg = average @@ fun () -> push queue 0 in *)
    let others =
      (* Printf.printf "%f" avg; *)
      List.init n_producers @@ fun _ ->
      Domain.spawn @@ fun () ->
      for i = 1 to n_messages do
        push queue i
      done
    in

    for i = 1 to n_messages do
      if i > 100 && i mod 2 = 1 then try_pop queue else push queue i
    done;

    List.iter Domain.join others
  in
  Printf.printf "%s push_pop (%d producers): %f per second\n%!" label
    n_producers
    (Float.of_int (n_messages * n_producers) /. elapsed)

let bench_multi_rand label create push try_pop n_producers =
  let n_messages = n_messages / n_producers in

  let elapsed =
    average @@ fun () ->
    let queue = create () in
    let others =
      List.init n_producers @@ fun _ ->
      Domain.spawn @@ fun () ->
      for i = 1 to n_messages do
        push queue i
      done
    in

    let len = ref 0 in
    let queue = create () in
    for i = 1 to n_messages do
      if !len > 1000 then (
        len := !len - 1;
        try_pop queue)
      else if !len = 0 then (
        len := !len + 1;
        push queue i)
      else
        let rand = Random.bool () in
        if rand = true then (
          len := !len + 1;
          push queue i)
        else (
          len := !len - 1;
          push queue i)
    done;
    List.iter Domain.join others
  in
  Printf.printf "%s random (%d producers): %f per second\n%!" label n_producers
    (Float.of_int (n_messages * n_producers) /. elapsed)

let bench_multi_ratio label create push try_pop n_producers =
  let n_messages = n_messages / n_producers in

  let elapsed =
    average @@ fun () ->
    let queue = create () in
    (* let avg = 1000*(average @@ fun () -> push queue 0) in *)
    let others =
      (* Printf.printf "%f" avg; *)
      List.init n_producers @@ fun _ ->
      Domain.spawn @@ fun () ->
      for i = 1 to n_messages do
        if i mod 1000 = 0 then push queue i
          (* else Unix.sleepf @@ avg  --> this is causing problems*)
      done
    in

    for i = 1 to n_messages do
      (* limiting the length of the queue to 100 *)
      if i > 100 && i mod 2 = 1 then try_pop queue else push queue i
    done;

    List.iter Domain.join others
  in
  Printf.printf "%s ratio (%d producers): %f per second\n%!" label n_producers
    (Float.of_int (n_messages * n_producers) /. elapsed)

let list = rev (generate_list (2 * n_messages) 0 []) []

let () =
  print_endline "Single domain";
  bench_single "Picos MPSC single     " Mpsc_queue_1.create Mpsc_queue_1.push
    (fun q -> Mpsc_queue_1.pop q |> ignore);
  bench_single "GADT implementation  " Saturn.Single_consumer_queue.create
    Saturn.Single_consumer_queue.push (fun q ->
      Saturn.Single_consumer_queue.pop q |> ignore);
  bench_single "Saturn old MPSC queue " Mpsc_queue_old.create
    Mpsc_queue_old.push (fun q -> Mpsc_queue_old.pop q |> ignore);
  bench_single "Saturn queue          " Saturn.Queue.create Saturn.Queue.push
    (fun q -> Saturn.Queue.pop q |> ignore);

  print_endline "";
  [ 1; 10; 100; 1000; 5000 ]
  |> List.iter (fun n ->
         print_endline "";
         bench_single_length n "Picos MPSC single    " Mpsc_queue_1.create
           Mpsc_queue_1.push (fun q -> Mpsc_queue_1.pop q |> ignore);
         bench_single_length n "GADT implementation  "
           Saturn.Single_consumer_queue.create Saturn.Single_consumer_queue.push
           (fun q -> Saturn.Single_consumer_queue.pop q |> ignore);
         bench_single_length n "Saturn old MPSC queue " Mpsc_queue_old.create
           Mpsc_queue_old.push (fun q -> Mpsc_queue_old.pop q |> ignore);
         bench_single_length n "Saturn queue          " Saturn.Queue.create
           Saturn.Queue.push (fun q -> Saturn.Queue.pop q |> ignore));

  print_endline "";

  bench_single_rand "Picos MPSC single    " Mpsc_queue_1.create
    Mpsc_queue_1.push (fun q -> Mpsc_queue_1.pop q |> ignore);
  bench_single_rand "GADT implementation  " Saturn.Single_consumer_queue.create
    Saturn.Single_consumer_queue.push (fun q ->
      Saturn.Single_consumer_queue.pop q |> ignore);
  bench_single_rand "Saturn old MPSC queue " Mpsc_queue_old.create
    Mpsc_queue_old.push (fun q -> Mpsc_queue_old.pop q |> ignore);
  bench_single_rand "Saturn queue          " Saturn.Queue.create
    Saturn.Queue.push (fun q -> Saturn.Queue.pop q |> ignore);

  print_endline "";
  bench_single_seq list "Picos MPSC single    " Mpsc_queue_1.create
    Mpsc_queue_1.push (fun q -> Mpsc_queue_1.pop q |> ignore);
  bench_single_seq list "GADT implementation  "
    Saturn.Single_consumer_queue.create Saturn.Single_consumer_queue.push
    (fun q -> Saturn.Single_consumer_queue.pop q |> ignore);
  bench_single_seq list "Saturn old MPSC queue " Mpsc_queue_old.create
    Mpsc_queue_old.push (fun q -> Mpsc_queue_old.pop q |> ignore);
  bench_single_seq list "Saturn queue          " Saturn.Queue.create
    Saturn.Queue.push (fun q -> Saturn.Queue.pop q |> ignore);

  print_endline "";

  bench_single_all "Picos MPSC single    " Mpsc_queue_1.create Mpsc_queue_1.push
    (fun q -> Mpsc_queue_1.pop_opt q |> ignore)
    (fun q -> Mpsc_queue_1.is_empty q |> ignore)
    Mpsc_queue_1.push_head
    (fun q -> Mpsc_queue_1.peek_opt q |> ignore);
  bench_single_all "GADT implementation  " Saturn.Single_consumer_queue.create
    Saturn.Single_consumer_queue.push
    (fun q -> Saturn.Single_consumer_queue.pop_opt q |> ignore)
    Saturn.Single_consumer_queue.is_empty Saturn.Single_consumer_queue.push_head
    Saturn.Single_consumer_queue.peek_opt;
  bench_single_all "Saturn old MPSC queue " Mpsc_queue_old.create
    Mpsc_queue_old.push
    (fun q -> Mpsc_queue_old.pop_opt q |> ignore)
    Mpsc_queue_old.is_empty Mpsc_queue_old.push_head Mpsc_queue_old.peek_opt;

  (* bench_single_all "Saturn queue          " Saturn.Queue.create
     Saturn.Queue.push
     (fun q -> Saturn.Queue.pop q |> ignore)
     Saturn.Queue.is_empty Saturn.Queue.push_head Saturn.Queue.peek
     Saturn.Queue.close; *)
  print_endline "";
  [ 1; 2; 4 ]
  |> List.iter @@ fun n ->
     print_endline "";
     bench_multi "Picos MPSC " Mpsc_queue_1.create Mpsc_queue_1.push
       (fun q ->
         match Mpsc_queue_1.pop q with
         | _ -> true
         | exception Mpsc_queue_1.Empty -> false)
       n;
     bench_multi "GADT implementation " Saturn.Single_consumer_queue.create
       Saturn.Single_consumer_queue.push
       (fun q -> Saturn.Single_consumer_queue.pop_opt q |> Option.is_some)
       n;
     bench_multi "Saturn old MPSC queue " Mpsc_queue_old.create
       Mpsc_queue_old.push
       (fun q -> Mpsc_queue_old.pop_opt q |> Option.is_some)
       n;
     bench_multi "Saturn queue " Saturn.Queue.create Saturn.Queue.push
       (fun q -> Saturn.Queue.pop_opt q |> Option.is_some)
       n;
     print_endline "";
     bench_multi_ratio "Picos MPSC " Mpsc_queue_1.create Mpsc_queue_1.push
       (fun q -> Mpsc_queue_1.pop q |> ignore)
       n;
     bench_multi_ratio "GADT implementation "
       Saturn.Single_consumer_queue.create Saturn.Single_consumer_queue.push
       (fun q -> Saturn.Single_consumer_queue.pop q |> ignore)
       n;
     bench_multi_ratio "Saturn old MPSC queue " Mpsc_queue_old.create
       Mpsc_queue_old.push
       (fun q -> Mpsc_queue_old.pop q |> ignore)
       n;
     bench_multi_ratio "Saturn queue " Saturn.Queue.create Saturn.Queue.push
       (fun q -> Saturn.Queue.pop q |> ignore)
       n;
     print_endline "";
     bench_multi_rand "Picos MPSC " Mpsc_queue_1.create Mpsc_queue_1.push
       (fun q -> Mpsc_queue_1.pop q |> ignore)
       n;
     bench_multi_rand "GADT implementation " Saturn.Single_consumer_queue.create
       Saturn.Single_consumer_queue.push
       (fun q -> Saturn.Single_consumer_queue.pop q |> ignore)
       n;
     bench_multi_rand "Saturn old MPSC queue " Mpsc_queue_old.create
       Mpsc_queue_old.push
       (fun q -> Mpsc_queue_old.pop q |> ignore)
       n;
     bench_multi_rand "Saturn queue " Saturn.Queue.create Saturn.Queue.push
       (fun q -> Saturn.Queue.pop q |> ignore)
       n;
     print_endline "";
     bench_multi_push_pop "Picos MPSC " Mpsc_queue_1.create Mpsc_queue_1.push
       (fun q -> Mpsc_queue_1.pop q |> ignore)
       n;
     bench_multi_push_pop "GADT implementation "
       Saturn.Single_consumer_queue.create Saturn.Single_consumer_queue.push
       (fun q -> Saturn.Single_consumer_queue.pop q |> ignore)
       n;
     bench_multi_push_pop "Saturn old MPSC queue " Mpsc_queue_old.create
       Mpsc_queue_old.push
       (fun q -> Mpsc_queue_old.pop q |> ignore)
       n;
     bench_multi_push_pop "Saturn queue " Saturn.Queue.create Saturn.Queue.push
       (fun q -> Saturn.Queue.pop q |> ignore)
       n
