# File: Heiko.ex
# This file was generated from heiko.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Heiko do
  def unquote(:"start")() do
    :erlang.apply(:"heiko", :"start", [])
  end
  def unquote(:"create_queue")(arg1, arg2) do
    :erlang.apply(:"heiko", :"create_queue", [arg1, arg2])
  end
  def unquote(:"update_queue")(arg1, arg2) do
    :erlang.apply(:"heiko", :"update_queue", [arg1, arg2])
  end
  def unquote(:"delete_queue")(arg1) do
    :erlang.apply(:"heiko", :"delete_queue", [arg1])
  end
  def unquote(:"queue_size")(arg1) do
    :erlang.apply(:"heiko", :"queue_size", [arg1])
  end
  def unquote(:"active_workers")(arg1) do
    :erlang.apply(:"heiko", :"active_workers", [arg1])
  end
  def unquote(:"queue")(arg1, arg2, arg3) do
    :erlang.apply(:"heiko", :"queue", [arg1, arg2, arg3])
  end
  def unquote(:"queue")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"heiko", :"queue", [arg1, arg2, arg3, arg4])
  end
end
