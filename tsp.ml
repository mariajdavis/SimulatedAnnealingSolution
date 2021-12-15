open Anneal

(* The following code is needed to build the starting state *)
type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t 
let rec from n = Cons (n, lazy (from (n + 1)))
let rec take n (Cons (h, t)) =
  if n = 0 then []
  else h::take (n - 1) (Lazy.force t)

(* Randomly get any int between 1 and 1 less than size of state array, since
we cannot swap the start or end node (which is the same) *)
let randomly_select_two_diff_nodes state = 
    let array_length = (Array.length state) - 2 in
    let random1 = (Random.int array_length) + 1 in
    let random2 = (Random.int array_length) + 1 in
    let rec random_helper rand1 rand2 =
        if Int.compare rand1 rand2 == 0 then
        random_helper rand1 ((Random.int array_length) + 1)
        else 
        rand1, rand2
    in random_helper random1 random2

(* Fold function for determining the energy of a state.
If else to handle the last node having no next node *)
let energy_fold_helper (energy, index, state_array, adj_matrix, array_length) node =
    if index < (array_length - 1) then
    let next_node = state_array.(index + 1) in
    let updated_energy = energy +. adj_matrix.(node).(next_node) in
    updated_energy, index + 1, state_array, adj_matrix, array_length
    else
    energy, index, state_array, adj_matrix, array_length

(* Return the energy of a state/path *)
let energy_function state_array adj_matrix =
    let energy, _, _, _, _ = Array.fold_left energy_fold_helper (0.0, 0, state_array, adj_matrix, Array.length state_array) state_array in
    energy

(* Return a new (neighbour) state with two nodes swapped from original state *)
let new_state_with_swapped_nodes old_state node1_index node2_index =
    let new_state = Array.copy old_state in
    let node1 = Array.get old_state node1_index in
    let node2 = Array.get old_state node2_index in
    Array.set new_state node1_index node2;
    Array.set new_state node2_index node1;
    new_state

(* Return result of simulated annealing p function to help determine whether
the next state is chosen or not *)
let p current_state_energy next_state_energy temp = 
    if Float.compare current_state_energy next_state_energy > 0 then
        1.0
    else
        Float.exp ((current_state_energy -. next_state_energy) /. temp)

(* Return next state/path (may be previous state or randomly chosen next state) *)
let next current_state current_state_energy energy temp = 
    let node1_index, node2_index = randomly_select_two_diff_nodes current_state in
    let potential_next_state = new_state_with_swapped_nodes current_state node1_index node2_index in
    let potential_next_state_energy = energy potential_next_state in
    let p_value = p current_state_energy potential_next_state_energy temp in
    if (Float.compare current_state_energy potential_next_state_energy = 0) || (Float.compare p_value (Random.float 1.0) >= 0) then
        potential_next_state
    else
        current_state

(* Return a starting state for a given matrix, using indices *)
let starting_state length_of_matrix = 
    let starting_list = take length_of_matrix (from 0) in
    Array.append (Array.of_list starting_list) [|0|]

(* Return float array from a string (line from file) *)
let get_float_array s = 
    Array.map float_of_string (Array.of_list (Base.List.filter (Base.String.split_on_chars ~on:[' ';'\n'] (String.trim s)) ~f:(fun x -> String.compare x "" <> 0)))

(* read_distances fold helper function which appends a float array from each file line to a list *)
let adj_matrix_list_builder list file_line =
    list@[get_float_array file_line]

(* Build float adjacency matrix (2D array) from txt file *)
let read_distances file_name =
    let ic = Stdio.In_channel.create file_name in 
    Array.of_list (Stdio.In_channel.fold_lines ic ~init:[] ~f:(adj_matrix_list_builder))

(* print_path fold helper function *)
let print_path_helper (string, index) element =
    if index = 0 then 
        string_of_int element, 1
    else
        string ^ " -> " ^ string_of_int element, index + 1

(* Print results of final path/state *)
let print_path state =
    let state_string, _ = Array.fold_left print_path_helper ("", 0) state in
    Printf.printf "%s\n" state_string

(* Runs simulated annealing for TSP by turning distances from file into an adjacency matrix *)
let tsp file_name starting_temp temp_factor temp_interval number_of_iterations =
    let adj_matrix = read_distances file_name in
    let energy_helper state = energy_function state adj_matrix in
    let start_s = starting_state (Array.length adj_matrix) in
    let final_path, final_energy = Anneal.run start_s (energy_helper) (next) starting_temp temp_factor temp_interval number_of_iterations in
    Printf.printf "%s\n" "The final energy:";
    Printf.printf "%f\n" final_energy;
    Printf.printf "%s\n" "The final state:";
    print_path final_path

let () =
	if (Array.length Sys.argv) != 3 then Printf.eprintf "Incorrect number of arguments!\n"
	else 
        let file_name = Sys.argv.(1) in
        let number_of_iterations = int_of_string Sys.argv.(2) in
        let starting_temp = 100.0 in
        let temp_factor = 0.99 in
        let temp_interval = (float_of_int number_of_iterations /. 1000.0) +. 1.0 in
        tsp file_name starting_temp temp_factor temp_interval number_of_iterations
