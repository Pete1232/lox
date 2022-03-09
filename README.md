# lox

Implementations of the "lox" interpreter, from the [Crafting Interpreters] book.

## Scala 3 build

```sh
# REPL
mill _.run

# Run script from file
mill _.run <path>

# Format
mill _.reformat

# Test
mill _.test

# Build a JAR
mill _.assembly

# and then run from the JAR
java -jar out/slox/assembly.dest/out.jar
java -jar out/slox/assembly.dest/out.jar <path>
```

[crafting interpreters]: https://github.com/munificent/craftinginterpreters
