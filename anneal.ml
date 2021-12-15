(* Update temperature using factor *)
let temp_function factor temp = 
    factor *. temp

(* run fold helper function *)  
let run_helper (start_state, energy, next, t, factor, interval, iter, current_energy) _ =
    let temp = if (iter > 0 && (iter mod (int_of_float interval) = 0)) then (temp_function factor t) else t in
    let the_next_state = next start_state current_energy energy temp in
    the_next_state, energy, next, temp, factor, interval, (iter + 1), (energy the_next_state)

(* Return final path and final path's energy afte running run_helper maxsteps number of times *)
let run start_state energy next t factor interval maxsteps =
    let steps_array = Array.make maxsteps 0 in
    let (final_path, _, _, _, _, _, _, final_energy) = Array.fold_left run_helper (start_state, energy, next, t, factor, interval, 0, 0.0) steps_array in
    final_path, final_energy