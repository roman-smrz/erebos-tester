# Revision history for erebos-tester

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
