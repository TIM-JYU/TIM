# TIM Rust library and bindings

This directory contains a Rust library with various
helper functions used throughout TIM.
Currently, only basic and frequently used functions
are defined, but over time the goal is to convert the entire
document processing pipeline to use Rust.

The library includes Python bindings, which are used
by TIM backend.

## Building

To build the library, use `./tim rust` command.
The command automatically builds and installs the library
in the `tim_rust/python` path.

`./tim update all` and other commands will automatically
invoke the Rust build if necessary.

## Quick guide: Defining new functions for Python

tim_rust uses [PyO3](https://pyo3.rs/v0.19.2/) to handle interop between Python and Rust.

1. Add the function signature to `python/tim_rust.pyi`.
    Define any documentation and type annotations for Python.

    Example:
    ```python
    def sum(a: int, b: int) -> int:
        """Returns the sum of two integers."""
        ...
    ```
   
    Instead of the method body, use `...` to indicate that
    the function is defined in Rust.

2. Add the function to `python/src/lib.rs`.
    Use `#[pyfunction]` attribute to mark the function
    as a Python function.

    Example:
    ```rust
    #[pyfunction]
    fn sum(a: i64, b: i64) -> PyResult<i64> {
        Ok(a + b)
    }
    ```
   
    Note that the return type is always PyResult<T>,
    where T is the actual return type of the function.
    Refer to [PyO3 type conversion chart](https://pyo3.rs/v0.19.2/conversions/tables)
    to see how to convert between Python and Rust types.

3. Add the function to `tim_rust` module with `add_function` function in `python/src/lib.rs`:

    ```rust
    #[pymodule]
    fn tim_rust(_py: Python, m: &PyModule) -> PyResult<()> {
        // Other functions...
        m.add_function(wrap_pyfunction!(sum, m)?)?;
        // Other functions...
        Ok(())
    }
   ```
