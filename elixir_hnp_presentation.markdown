# Title: Elixir and OTP

There are 2 parts to this talk:

* Elixir
* OTP

By the end of the presentation, we would see what Elixir and OTP both are.

## Introducing Elixir

Elixir is a programming language that is build on top of the Erlang VM.

Erlang is a functional language that focuses on concurrency and fault tolerance. It was part of Ericssons' ongoing effort to build fault tolerance telcommunications systems. Erlang is 28 years old.

## Why Elixir

Not just a pretty syntax for Erlang. Meta-programming, Macros, Tooling. 

No conversion cost for calling Erlang from Elixir.

## The Standard Library – Enum API

```elixir
[1, 2, 3] |> Enum.map fn x -> x * x end
```

## Macros

## Metaprogramming

```elixir
defmodule MimeTypes do
  HTTPotion.start
  HTTPotion.Response[body: body] = HTTPotion.get "http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types"

  Enum.each String.split(body, %r/\n/), fn (line) ->
    unless line == "" or line =~ %r/^#/ do
      [ mimetype | _exts ] = String.split(line)

      def is_valid?(unquote(mimetype)), do: true
    end
  end

  def is_valid?(_mimetype), do: false
end
```

MimeTypes.is_valid?("application/vnd.exn") #=> false
MimeTypes.is_valid?("application/json")    #=> true

## Mix Build Tool

## Processes

```elixir
defmodule Maps do

  def map([], _), do: []
  def map([h|t], func), do: [ func.(h) | map(t, func) ]

  def child(element, func, parent) do
    send parent, func.(element)
  end
  defp spawn_children(collection, func) do
    map collection, fn element -> spawn(__MODULE__, :child, [element, func, self]) end
  end

  defp collect_results(pids) do
    map pids, fn _ -> receive do: ( value -> value) end
  end

  def pmap(collection, func) do
    collection |> spawn_children(func) |> collect_results
  end
end

Maps.map [1, 2, 3], &(&1 * &1)
# [1, 4, 9]

Maps.pmap [1, 2, 3], &(&1 * &1)
# [1, 4, 9]
```


## Erlang Interoperability

... and still plays nice with Erlang

## OTP

OTP - Open Telecom Protocol, has almost nothing to do with Telecom. Think of it as an Erlang framework.

Elixir calls them behaviours. In Erlang, pretty verbose.

### GenServers - Generic Servers

Build Client Server:

```elixir
defmodule Stacker.Server do
  use GenServer.Behaviour

  def init(stack) do
    { :ok, stack }
  end

  def handle_call(:pop, _from, [h|stack]) do
    { :reply, h, stack }
  end

  def handle_cast({ :push, new }, stack) do
    { :noreply, [new|stack] }
  end
end
```
In action:

```elixir
# Let's start the server using Erlang's :gen_server module.
# It expects 3 arguments: the server module, the initial
# stack and some options (if desired):
iex> { :ok, pid } = :gen_server.start_link(Stacker.Server, [], [])
{:ok,<...>}

# Now let's push something onto the stack
iex> :gen_server.cast(pid, { :push, 13 })
:ok

# Now let's get it out from the stack
# Notice we are using *call* instead of *cast*
iex> :gen_server.call(pid, :pop)
13
```

### Supervisors - Supervisors

```elixir
defmodule Stacker.Supervisor do
  use Supervisor.Behaviour

  # A convenience to start the supervisor
  def start_link(stack) do
    :supervisor.start_link(__MODULE__, stack)
  end

  # The callback invoked when the supervisor starts
  def init(stack) do
    children = [ worker(Stacker.Server, [stack]) ]
    supervise children, strategy: :one_for_one
  end
end
```
