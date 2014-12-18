RMarshal
========

The library deserialises Ruby objects dumped by `Marshal.dump` into Erlang
terms. It's like `Marshal.load`, but for Erlang. Currently, it doesn't support
loading advanced data structures like classes, but the vital things are
supported.

* Fixnum
* Hash
* Array
* Bignum
* Symbol
* Float
* String

Example
---

```ruby
# data.rb
data = Marshal.dump({:hello => 'world'})
File.open('data.dat', 'w') do |f|
  f.write(data)
end
```

```erlang
case file:read_file("data.dat") of
    {ok, Data} -> rmarshal:load(Data);
    Any -> Any
end.

%% Returns:
%% #{hello => "world"}
```

Testing
---

Install rebar (not bundled with this library) and then run this.

```shell
make test
```

Licence
---

The project uses the zlib License. See the LICENCE.txt file for more
information.
