# OCaml Trading Simulator

A simple algorithmic trading simulator written in OCaml that implements a moving average crossover strategy. This project demonstrates basic quantitative trading concepts and OCaml programming skills.

## Features

- Moving Average Crossover Strategy
- Trade Simulation
- Performance Metrics Calculation (Returns, Sharpe Ratio)
- Type-safe Implementation
- Unit Tests

## Project Structure

- `src/trading.ml`: Core trading logic and types
- `src/example.ml`: Example usage with sample data
- `tests/test_trading.ml`: Unit tests

## Requirements

- OCaml 4.14.0 or later
- Dune build system
- OPAM (OCaml Package Manager)

## Installation

1. Install OPAM:
```bash
brew install opam  # For macOS
```

2. Initialize OPAM:
```bash
opam init
```

3. Update your shell environment:
```bash
eval $(opam env --switch=default)
```

4. Install Dune:
```bash
opam install dune
```

## Building and Running

1. Build the project:
```bash
dune build
```

2. Run the example:
```bash
dune exec ./_build/default/src/example.exe
```

3. Run the tests:
```bash
dune exec ./_build/default/tests/test_trading.exe
```

## How It Works

The simulator implements a simple moving average crossover strategy:
- Calculates short-term and long-term moving averages
- Generates buy/sell signals when the averages cross
- Simulates trades based on these signals
- Calculates performance metrics including returns and Sharpe ratio

## Example Output

The example generates sample market data and runs the strategy, outputting:
- Number of trades executed
- Average return per trade
- Sharpe ratio
- Detailed trade information

## Why OCaml?

OCaml is particularly well-suited for quantitative trading because of its:
- Strong type system
- Functional programming features
- Performance characteristics
- Pattern matching capabilities
- Immutable data structures

This project demonstrates these features while implementing a practical trading strategy. 