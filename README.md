RMarshal
========

[![Build Status](https://travis-ci.org/kyrylo/rmarshal.svg?branch=master)](https://travis-ci.org/kyrylo/rmarshal)
[![semver]](http://semver.org)

Introduction
------------

This Erlang library deserialises Ruby objects dumped by `Marshal.dump` into
Erlang terms. It's like Ruby's [`Marshal.load`][marshal-load], but for
Erlang. Currently, it doesn't support loading advanced objects like classes, but
the bare minimum is supported. The list of supported objects includes:

* Fixnum
* Hash
* Array
* Bignum
* Symbol
* Float
* String

Installation
------------

### Rebar3

```erlang
%% rebar.config
{deps, [
  {rmarshal, {git, "git://github.com/kyrylo/rmarshal.git", {tag, "v0.0.4"}}}
]}.
```

Examples
---

### Basic example

Dump some Ruby objects into a binary file:

```ruby
# data.rb
data = Marshal.dump(hello: 'world')
File.open('data.dat', 'w') do |f|
  f.write(data)
end
```

Read the dumped data with Erlang and load it with help of the RMarshal library:

```erlang
%% loader.erl
load_ruby_data() ->
    case file:read_file("data.dat") of
        {ok, BinaryData} -> rmarshal:load(BinaryData);
        Any -> Any
    end.

load_ruby_data()
%% Returns:
%% {ok, #{hello => "world"}}
```

API
---

### rmarshal:load/1

Accepts binary string formatted according to the official
[Marshal Format][marshal-format]. Returns a tuple with in the format `{ok,
rterm()}`.

```erlang
{ok, Decoded} = rmarshal:load(BinaryData).
```

Testing
---

Run the following command:

```shell
make test
```

Supported Erlang versions
-------------------------

* ≥17.0

Contact
-------

In case you have a problem, question or a bug report, feel free to:

* [file an issue][issues]
* [send me an email](mailto:silin@kyrylo.org)
* [tweet at me][twitter]

Licence
---

The project uses the zlib License. See the LICENCE.txt file for more
information.

[semver]: https://img.shields.io/:semver-0.0.4-brightgreen.svg?style=flat
[marshal-load]: http://ruby-doc.org/core-2.3.0/Marshal.html#method-c-load
[marshal-format]: https://github.com/ruby/ruby/blob/b01c28eeb3942bce1ddf9b9243ecf727d5421c6d/doc/marshal.rdoc
[issues]: https://github.com/kyrylo/rmarshal/issues
[twitter]: https://twitter.com/kyrylosilin
