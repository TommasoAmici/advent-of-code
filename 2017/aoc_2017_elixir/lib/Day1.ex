defmodule Day1 do
  def part_1(input) do
    len = String.length(input)
    step = 1
    solve(input, len, step)
  end

  def part_2(input) do
    len = String.length(input)
    step = (len / 2) |> round
    solve(input, len, step)
  end

  def solve(input, len, step) do
    find_match(len, step, 0, 0, input <> input)
  end

  defp find_match(max, step, curr, count, stream) when is_integer(step) do
    if max == curr do
      count
    else
      a = String.at(stream, curr)
      b = String.at(stream, curr + step)

      value =
        if a == b do
          String.to_integer(a)
        else
          0
        end

      find_match(max, step, curr + 1, count + value, stream)
    end
  end
end
