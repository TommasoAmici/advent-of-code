ExUnit.start()

defmodule Tests do
  @inputs Path.expand("./inputs", __DIR__)

  def open_file(filename) do
    @inputs
    |> Path.join(filename)
    |> File.read!()
    |> String.trim()
  end
end
