defmodule MyServer do
  use GenServer.Behaviour

  # Callbacks

  def handle_call(:pop, _from, [h|t]) do
    { :reply, h, t }
  end

  def handle_call(request, from, config) do
    # Call the default implementation from GenServer.Behaviour
    super(request, from, config)
  end

  def handle_cast({ :push, item }, config) do
    { :noreply, [item|config] }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
