# cipher-cli
Encrypt and decrypt messages using various algorithms

## How to use it
```sh
cipher-cli v0.0.0.1, (C) Vincibean

algo [COMMAND] ... [OPTIONS]
  Encrypt and decrypt messages using various algorithms

Common flags:
  -e --encrypt          Encrypt a message
  -d --decrypt          Decrypt a message
  -m --msg=ITEM         The message to encrypt / decrypt
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
  -v --verbose          Loud verbosity
  -q --quiet            Quiet verbosity

algo caesar [OPTIONS]
  Use Caesar's encryption / decryption algorithm

  -n --numkey=INT       The integer to use as key. It can be either a
                        positive or a negative number

algo vigenere [OPTIONS]
  Use Vigenere's encryption / decryption algorithm

  -s --stringkey=ITEM   The string to use as key. It must not be an empty
                        string
```