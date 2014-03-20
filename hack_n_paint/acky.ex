
defmodule Ackermann do
  def ackermann(0, n), do: n + 1
  def ackermann(m, 0), do: ackermann(m-1, 1)
  def ackermann(m, n), do: ackermann(m-1, ackermann(m,n-1))

  def loop do
    receive do
      { from, {m, n} } ->
        from |> send(ackermann(m, n))
        loop
    end
  end
end



