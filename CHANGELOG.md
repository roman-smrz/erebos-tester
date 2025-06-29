# Revision history for erebos-tester

## 0.3.3 -- 2025-06-25

* Added optional `timeout` setting to config file
* Added `multiply_timeout` command
* Added `True` and `False` literals, and comparison operators for boolean values
* Added `--exclude` command-line option to exclude tests
* Execute shell commands in appropriate network namespace
* Show name of failed test in output

## 0.3.2 -- 2025-05-16

* Asset files and directories for use during tests
* Select tests from project configuration using only test name on command line without script path
* Added `args` parameter to `spawn` command to pass extra command-line arguments to the spawned tool
* Experimental shell interpreter

## 0.3.1 -- 2025-03-03

* Fix executing test tool given with relative path

## 0.3.0 -- 2025-02-28

* User-defined functions
* Modules, exports and imports
* Added `ifname` member to the `node` type
* Added `>`, `>=`, `<=` and `<` operators for numbers
* Change "flush" command to take regex argument via "matching" keyword
* Change working directory of spawned process to node directory
* Use custom C main instead of wrapper binary for unshare(2) call.
* Fix regex check in flush command
* Time information in output
* Support for GHC up to 9.12
* Fail when test requested on command-line is not found

## 0.2.4 -- 2024-08-13

* Fix build with mtl-2.3
* Fix type error reporting for some command parameters

## 0.2.3 -- 2024-08-10

* Added `network` member to the `node` object
* Use colors by default only on terminal, add `--color`/`--no-color` options to select manually.
* Accept module name declaration
* Report multiple parsing errors in single pass

## 0.2.2 -- 2024-05-17

* Fix unshare failing with newer compilers
* Documentation and helptext updates
* Compatibility with GHC up to 9.10

## 0.2.1 -- 2024-05-14

* Selection of test from test file path on command line using '`:`' charater
* Added `--repeat` option to run the tests multiple times
* Added `--wait` option to wait at the end of each test
* Added `flush` command
* Show record selectors in failure reports
* Compatibility with GHC up to 9.8

## 0.2.0 -- 2023-04-26

* Lists and "for" statement.
* Subnets of networks.
* Commands to disconnect nodes or networks.

## 0.1.1 -- 2023-02-04

* GDB improvements and fixes.

## 0.1.0 -- 2023-01-08

* First version.
