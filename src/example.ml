open Trading

let generate_sample_data n =
  let rec helper acc i =
    if i >= n then List.rev acc
    else
      let price = 100.0 +. (sin (float_of_int i /. 10.0) *. 10.0) in
      helper ({timestamp = i; price = price} :: acc) (i + 1)
  in
  helper [] 0

let () =
  let market_data = generate_sample_data 100 in
  let signals = generate_signals market_data 5 20 in
  let trades = simulate_trades market_data signals in
  let returns = calculate_returns trades in
  let sharpe = calculate_sharpe_ratio returns 0.02 in

  Printf.printf "Number of trades: %d\n" (List.length trades);
  Printf.printf "Average return: %.2f%%\n" ((List.fold_left (+.) 0.0 returns /. float_of_int (List.length returns)) *. 100.0);
  Printf.printf "Sharpe Ratio: %.2f\n" sharpe;

  List.iter (fun trade ->
    Printf.printf "Trade: %s at %.2f, exit at %.2f, return: %.2f%%\n"
      (match trade.position with Long -> "Long" | Short -> "Short" | Flat -> "Flat")
      trade.entry_price
      trade.exit_price
      ((match trade.position with
        | Long -> (trade.exit_price -. trade.entry_price) /. trade.entry_price
        | Short -> (trade.entry_price -. trade.exit_price) /. trade.entry_price
        | Flat -> 0.0) *. 100.0)
  ) trades 