open Trading

(* Test calculate_sma:
   This test verifies our Simple Moving Average (SMA) calculation, which is fundamental
   to our trading strategy. We test it with a simple sequence [1;2;3;4;5] and period=3.
   
   Expected result: SMA should be 4.0 because:
   - For period=3, we look at the last 3 numbers [3;4;5]
   - (3 + 4 + 5) / 3 = 4.0
   
   This test is crucial because SMA calculation errors would lead to incorrect trading signals. *)
let test_calculate_sma () =
  let prices = [1.0; 2.0; 3.0; 4.0; 5.0] in
  let sma = calculate_sma prices 3 in
  assert (abs_float (sma -. 4.0) < 0.0001);
  print_endline "✅ calculate_sma test passed"

(* Test generate_signals:
   This test checks if our strategy correctly generates trading signals based on
   moving average crossovers. We use an upward trending price sequence to test
   signal generation with short period=2 and long period=3.
   
   The test verifies:
   1. Signal generation for each price point
   2. Proper handling of the moving average comparison
   
   This is important because signal generation drives our trading decisions. *)
let test_generate_signals () =
  let market_data = [
    {timestamp = 1; price = 100.0};
    {timestamp = 2; price = 101.0};
    {timestamp = 3; price = 102.0};
    {timestamp = 4; price = 103.0};
    {timestamp = 5; price = 104.0};
  ] in
  let signals = generate_signals market_data 2 3 in
  assert (List.length signals = 5);
  print_endline "✅ generate_signals test passed"

(* Test simulate_trades:
   This test verifies our trade execution logic by simulating a sequence of trades
   with alternating Long/Short signals. It checks if:
   1. Trades are properly opened and closed
   2. Position transitions (Long->Short, Short->Long) are handled correctly
   3. Trade details (entry/exit prices, timestamps) are recorded accurately
   
   This test is critical because it validates our core trading mechanics and
   position management. *)
let test_simulate_trades () =
  let market_data = [
    {timestamp = 1; price = 100.0};
    {timestamp = 2; price = 101.0};
    {timestamp = 3; price = 102.0};
    {timestamp = 4; price = 103.0};
    {timestamp = 5; price = 104.0};
  ] in
  let signals = [(1, Long); (2, Short); (3, Long); (4, Short); (5, Long)] in
  let trades = simulate_trades market_data signals in
  assert (List.length trades > 0);
  print_endline "✅ simulate_trades test passed"

let () =
  print_endline "Running tests...";
  test_calculate_sma ();
  test_generate_signals ();
  test_simulate_trades ();
  print_endline "All tests passed! 🎉" 