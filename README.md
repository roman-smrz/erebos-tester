Erebos Tester
=============

Developed for testing the [Erebos protocol implementation](https://erebosprotocol.net/erebos)
and [C++ library](https://erebosprotocol.net/cpp), generally intended mainly
for testing networking code – it provides an easy way to execute the tested
program simultaneously on multiple nodes within a virtual network. These nodes
can be configured in a single or multiple subnets, and their properties, like
whether they are connected, can be changed during test run according to a given
script.

The test framework uses two components from the tested project:

1. Test tool – executable that accepts commands on standard input, executes the
   tested functionality based on those, and provides output on standard output.
   The output can be asynchronous, i.e. some events may trigger a message to be
   generated while waiting for output of a command; interpretation is up to the
   provided test script.

   The test tool can be set either:
   * by the `--tool` command-line parameter of `erebos-tester`, or
   * in the `erebos-tester.yaml` configuration file, or
   * by the `EREBOS_TEST_TOOL` environment variable.

2. Test script – defines how to run the instances of test tool and in what kind
   of network topology. Contains commands to send to the test tool instances
   and rules to interpret the responses. The script is written in a custom
   language described below.

Usage
-----

The `erebos-tester` tool, when executed without any arguments,
looks for a `erebos-tester.yaml` file in the current or any parent directory (see below for details).
Run `erebos-tester --help` for details about command-line parameters.

The tester can be installed from sources or directly via cabal:
```
cabal install erebos-tester
```

When available in the `PATH`, it can be run to test the [Haskell Erebos implementation](https://erebosprotocol.net/erebos):
```
git clone git://erebosprotocol.net/erebos
cd erebos
cabal build
erebos-tester --tool="$(cabal list-bin erebos) test" --verbose
```

or the [C++ one](https://erebosprotocol.net/cpp):
```
git clone git://erebosprotocol.net/cpp
cd cpp
cmake -B build
cmake --build build
erebos-tester --verbose
```

To run all tests from project configuration (see below), run the tester without any argument:
```
erebos-tester
```

To run only some named tests, list the names on command line:
```
erebos-tester FirstTest SecondTest
```

To run tests from a given test file, pass it as command-line argument (the path
must contain a slash, so use e.g. `./script.et` for script in the current
directory):
```
erebos-tester path/to/script.et
```

To select single test from a file, use `:` separator:
```
erebos-tester path/to/script.et:TestName
```

Configuration
-------------

To allow running `erebos-tester` without the need to supply project-specific configuration on command line,
per-project configuration can be done using `erebos-tester.yaml` file placed in the root of the project
(or other directory from which the tests will be executed).
This is a YAML file with following fields:

* `tool`: path to the test tool, which may be overridden by the `--tool` command-line option.
* `tests`: glob pattern that expands to all the test script files that should be used.
* `timeout`: initial timeout for test steps like `expect`, given as `int` or `float`; defaults to `1` if not specified.

Script language
---------------

The test script language uses indentation to define the command blocks, e.g. to
define a test body or denote the scope of variables. Each command is on its own
line, terminated by newline. Commands accept arguments preceded by name/keyword
in arbitrary order, to make the behavior clear without the need to know
the expected order of the parameters.

For examples, see tests within the
[Erebos implementation repository](https://code.erebosprotocol.net/erebos/tree/test).

Each test script file consists of one or more test cases, started with `test`
keyword, with its body within indented block:

```
test [<name>]:
    <test block>
```

Test name is optional, but if present can be used to run the single test from
a file that contains multiple tests.

### Types

The script language is strictly typed without any implicit conversions,
although types can not be (as of now) declared explicitly and are always inferred.
Each expression has specific concrete type, polymorphic types are not supported (yet).

#### integer

Integer numbers. Entered as decimal literals and used in arithmetic expressions:
```
let x = 2
let y = 3
let z = x * 2 + y
```

#### number

Arbitrary-precision numbers. Entered as literals with decimal point or percentage and used in arithmetic expressions:
```
let x = 2.1
let y = 34%
let z = x * 2.0 + y
```

#### string

String literals are enclosed in double quotes (`"`),
using backslash to escape special characters (`"`, `\` and `$`)
and to represent some others (`\n` for newline).
```
let s = "some text"
```

Dollar sign (`$`) can be used to expand variables (numbers are expanded to decimal representation).
```
let a = "abc"
let b = 4
let c = "$a $b"  # = "abc 4"
```

Arbitrary expression can be used within additional curly braces:
```
let a = 2
let b = 3
let s = "abc ${2*a + b}"  # = "abc 7"
```

#### regex

Regular expression literals are enclosed in slash characters (`/`):
```
let re = /a.*/ # match any string starting with 'a'
```

Dollar-expansion can be used here as well.
Strings expand to regular expressions matching the exact string,
regular expressions expand are used directly.
```
let str = "."
let re1 = /./
let re2 = /$str$re1/ # match '.' followed by any character
```

#### boolean

Result of comparison operators `==` and `/=`.

#### network

Represents network/subnet, created by `subnet` command and used by `subnet`, `node`, `spawn` and network configuration commands.

#### node

Represents network node, created by `node` command or implicitly by `spawn`,
and used by `spawn` or network configuration commands.

Members:

`ifname`
: Name of the primary network interface of the node.

`ip`
: String representation of the node primary IP address.

`network`
: The network which the node belogs to.

#### process

Represents running process. Created by `spawn`, used by `send` and `expect` commands.

Members:

`node`
: Node on which the process is running.

#### asset

Represents an asset (file or directory), which can be used during test execution.

Members:

`path`
: Path to the asset valid during the test execution.

#### list

Lists are written using bracket notation:
```
let numbers = [1, 2, 4]
```

List elements can be of any type, but all elements of a particular list must have the same type.

Used in the `for` command.

### Built-in commands

```
subnet <name> [of <network>]
```

Create a subnet within a `<network>` (or context network if omitted) and assign the new network to the variable `<name>`.

```
node <name> [on <network>]
```

Create a node on network `<network>` (or context network if omitted) and assign the new node to the variable `<name>`.

```
spawn as <name> [on (<node> | <network>)] [args <arguments>]
```

Spawn a new test process on `<node>` or `<network>` (or one from context) and assign the new process to variable `<name>`.
When spawning on network, create a new node for this process.
Extra `<arguments>` to the tool can be given as a list of strings using the `args` keyword.

The process is terminated when the variable `<name>` goes out of scope (at the end of the block in which it was created) by closing its stdin.
When the process fails to terminate successfully within a timeout, the test fails.

```
send <string> to <process>
```
Send line with `<string>` to the standard input of `<process>`.

```
expect <regex> from <process> [capture <var1> [, <var2> ... ]]
```
Check whether `<process>` produces line matching `<regex>` on standard output, and if this does not happen within current timeout, the test fails.
Output lines produced before starting this command and not matched by some previous `expect` are accepted as well.
Output lines not matching `<regex>` are ignored by this `expect` call, i.e. do not cause the `expect` call to fail.

Regular expressions are anchored on both sides, so must match the entire line.
If e.g. only the beginning should be matched, the passed regular expression needs to end with `.*`.

The regular expression can contain capture groups – parts enclosed in parentheses (`(`, `)`).
In that case the expect command has to have the `capture` clause with matching number of variable names.
Results of the captures are then assigned to the newly created variables as strings.

```
flush [from <proc>] [matching <regex>]
```

Flush memory of `<proc>` output, so no following `expect` command will match anything produced up to this point.
If the `matching` clause is used, discard only output lines matching `<regex>`.

```
guard <expr>
```

Check whether boolean expression `<expr>` is true; if not, the test fails.

```
disconnect_node [<node>]
```

Disconnect `<node>` from network – state of the veth network link from the node is set to down.
The effect lasts until the end of the block.

```
disconnect_nodes [<network>]
```

Disconnect all nodes of `<network>`. The network bridge interface state is set to down.
The effect lasts until the end of the block.

```
disconnect_upstream [<network>]
```

Disconnect network upstream – state of the veth network link connecting network bridge to the upstream network is set to down.
The effect lasts until the end of the block.

```
packet_loss <rate> [on <node>]
```

Set the packet loss rate on the node's veth link to `<rate>` as decimal number or percentage, e.g. `0.2` or `20%` for 20% packet loss rate.
The effect lasts until the end of the block.

```
for <var> in <expr>:
    <test block>
```

Execute `<test block>` for each element of list `<expr>`, with current element assigned to `<var>`.

```
local:
    <test block>
```

Execute `<test block>` in a new local scope. Used to restrict scope of variables or duration of effects.

```
with <expr>:
    <test block>
```

Execute `<test block>` with `<expr>` as context.

```
multiply_timeout by <multiplier>
```

Modify the timeout used for commands like `expect` by multiplying it with `<multiplier>`.
The effect lasts until the end of the block.

```
wait
```

Wait for user input before continuing. Useful mostly for debugging or test development.


### Functions

When calling a function, parameters are usually passed using argument keywords
(in the case of built-in commands, those keywords are typically prepositions
like `on`, `from`, etc.), and apart from those, there can be at most one
parameter passed without a keyword. This is done in order to avoid the need to
remember parameter order and to make the behavior of each call as clear as
possible, even without looking up the documentation.

To make the syntax unambiguous, the keywordless parameter can be passed as
a literal (number, string, etc.), or using parentheses. So this is ok:

```
expect /something/ from p
```

but if the regular expression is stored in a variable, the parameter needs to
be enclosed in parentheses:
```
expect (re) from p
```
or in a literal:
```
expect /$re/ from p
```

### Defining functions

Custom functions can be defined on the top level using `def` keyword, and with
the parameters either followed by `=` sign to return a value:
```
def quadruple of x = 4 * x
```

or followed by `:` to define test block:
```
def say_hello to p:
    send "hello" to p
    expect /hi/ from p
```

Those then can be invoked elsewhere:
```
test:
    spawn as p
    say_hello to p
```

When defining a function, the unnamed parameter, if any, must be enclosed in
parentheses:
```
def twice (x) = 2 * x
```

### Modules, exports and imports

Each test script file constitutes a module. As such, it can export definitions
for other modules to use, and import definitions from other modules. The name
of each module must match the filename with the file extension removed, and is
given using the `module` declaration. This declaration, if present, must be
given at the beginning of the file, before other declarations.

For example a file `test/foo.et` can start with:
```
module foo
```
This name is also implicitly assigned when the `module` declaration is omitted.

In case of a more complex hierarchy, individual parts are separated with `.`
and must match names of parent directories. E.g. a file `test/bar/baz.et`
can start with:

```
module bar.baz
```

Such declared hierarchy is then used to determine the root of the project in
order to find imported modules.

To export a definition from module, use `export` keyword before `def`:
```
export def say_hello to p:
    send "hello" to p
    expect /hi/ from p
```
or list the exported name in a standalone export declaration:
```
export say_hello

...

def say_hello to p:
    send "hello" to p
    expect /hi/ from p
```

To import module, use `import <name>` statement, which makes all the exported
definitions from the module `<name>` available in the local scope.
```
module bar.baz

import foo
```

### Assets

To provide the used test tool with access to auxiliary files needed for the
test execution, asset objects can be defined. The definition is done on the
toplevel using the `asset` keyword, giving the asset object name and a path to
the asset on the filesystem, relative to the directory containing the test
script:

```
asset my_asset:
    path: ../path/to/file
```

Such defined asset object can then be used in expressions within tests or function definitions:

```
test:
    spawn p
    send to p "use-asset ${my_asset.path}"
```

The `my_asset.path` expression expands to a string containing path to the asset
that can be used by the spawned process `p`. The process should not try to
modify the file.

Assets can be exported for use in other modules using the `export` keyword,
just like other definitions:

```
export asset my_asset:
    path: ../path/to/file
```


Optional dependencies
---------------------

The test framework can use some other tools to help with debugging or development.

### GDB

If GDB is installed, it's possible to use `--gdb` command-line switch of the `erebos-tester` tool to use the debugger.
The GDB session is started in background and tester uses the GDB machine interface to communicate with it.
Whenever a new process is spawned, it is attached to the debugger as a new inferior.
In case any process is terminated by a signal, e.g. crashes with segfault, interactive GDB session is opened.

### tcpdump

If `tcpdump` binary is found in the `PATH`,
it is used to generate network log in the pcap capture format within the test directory,
separately for each virtual network (specifically its bridge interface).
