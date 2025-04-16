(* Trading Simulator Module
   This module implements a simple moving average crossover trading strategy.
   It includes functions for calculating moving averages, generating trading signals,
   simulating trades, and calculating performance metrics. *)

(* Position type represents the current trading position *)
type position = Long | Short | Flat

(* Trade record type to store trade details *)
type trade = {
  entry_price: float;    (* Price at which the trade was entered *)
  exit_price: float;     (* Price at which the trade was exited *)
  position: position;     (* Type of position (Long/Short/Flat) *)
  entry_time: int;       (* Timestamp when trade was entered *)
  exit_time: int;        (* Timestamp when trade was exited *)
}

(* Market data record type to store price and timestamp *)
type market_data = {
  timestamp: int;        (* Time of the price point *)
  price: float;          (* Price at this timestamp *)
}

(* Calculate Simple Moving Average (SMA)
   Takes a list of prices and a period, returns the average of the last 'period' prices *)
let calculate_sma prices period =
  let rec take n = function
    | [] -> []
    | x :: xs -> if n = 0 then [] else x :: take (n-1) xs
  in
  let window = take period (List.rev prices) in
  let sum = List.fold_left (+.) 0.0 window in
  sum /. float_of_int (List.length window)

(* Generate trading signals based on moving average crossover
   When short-term MA crosses above long-term MA, generate Long signal
   When short-term MA crosses below long-term MA, generate Short signal *)
let generate_signals market_data short_period long_period =
  let rec helper signals = function
    | [] -> List.rev signals
    | hd :: tl ->
      let short_ma = calculate_sma (hd.price :: List.map (fun x -> x.price) tl) short_period in
      let long_ma = calculate_sma (hd.price :: List.map (fun x -> x.price) tl) long_period in
      let signal = if short_ma > long_ma then Long else Short in
      helper ((hd.timestamp, signal) :: signals) tl
  in
  helper [] market_data

(* Simulate trades based on generated signals
   Opens new positions when signals change and closes existing positions
   Returns a list of completed trades with entry and exit prices *)
let simulate_trades market_data signals =
  let rec helper current_position trades = function
    | [] -> List.rev trades
    | (timestamp, signal) :: tl ->
      let current_price = List.find (fun x -> x.timestamp = timestamp) market_data |> fun x -> x.price in
      match current_position, signal with
      | Flat, Long -> helper Long ({entry_price = current_price; exit_price = 0.0; position = Long; entry_time = timestamp; exit_time = 0} :: trades) tl
      | Flat, Short -> helper Short ({entry_price = current_price; exit_price = 0.0; position = Short; entry_time = timestamp; exit_time = 0} :: trades) tl
      | Long, Short -> 
        let trade = List.hd trades in
        let updated_trade = {trade with exit_price = current_price; exit_time = timestamp} in
        helper Short (updated_trade :: List.tl trades) tl
      | Short, Long ->
        let trade = List.hd trades in
        let updated_trade = {trade with exit_price = current_price; exit_time = timestamp} in
        helper Long (updated_trade :: List.tl trades) tl
      | _ -> helper current_position trades tl
  in
  helper Flat [] signals

(* Calculate returns for a list of trades
   For Long positions: (exit_price - entry_price) / entry_price
   For Short positions: (entry_price - exit_price) / entry_price *)
let calculate_returns trades =
  List.map (fun trade ->
    match trade.position with
    | Long -> (trade.exit_price -. trade.entry_price) /. trade.entry_price
    | Short -> (trade.entry_price -. trade.exit_price) /. trade.entry_price
    | Flat -> 0.0
  ) trades

(* Calculate Sharpe Ratio for a list of returns
   Sharpe Ratio = (mean_return - risk_free_rate) / standard_deviation
   Higher Sharpe ratio indicates better risk-adjusted returns *)
let calculate_sharpe_ratio returns risk_free_rate =
  let mean = List.fold_left (+.) 0.0 returns /. float_of_int (List.length returns) in
  let variance = List.fold_left (fun acc x -> acc +. (x -. mean) ** 2.0) 0.0 returns 
                 /. float_of_int (List.length returns) in
  let std_dev = sqrt variance in
  (mean -. risk_free_rate) /. std_dev 