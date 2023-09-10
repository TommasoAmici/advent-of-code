defmodule Day2 do
  def solve(input, fun) do
    input |> String.split("\n") |> Enum.map(fun) |> Enum.sum()
  end

  def part_1(input) do
    solve(input, &line_checksum_part_1/1)
  end

  def part_2(input) do
    solve(input, &line_checksum_part_2/1)
  end

  defp line_checksum_part_1(line) do
    converted = line |> String.split("\t") |> Enum.map(&String.to_integer/1)
    min_ = Enum.min(converted)
    max_ = Enum.max(converted)
    max_ - min_
  end

  defp line_checksum_part_2(line) do
    converted =
      line
      |> String.split("\t")
      |> Enum.map(&String.to_integer/1)

    converted
    |> Enum.map(fn x -> {x, Enum.find(converted, fn y -> y != x and rem(y, x) == 0 end)} end)
    |> Enum.filter(fn {_, y} -> y != nil end)
    |> Enum.map(fn {x, y} -> div(y, x) end)
    |> Enum.sum()
  end
end
