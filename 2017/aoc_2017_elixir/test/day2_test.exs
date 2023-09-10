defmodule Day2Test do
  use ExUnit.Case

  test "part 1" do
    assert Day2.part_1("5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8") == 18

    assert Tests.open_file("02.txt") |> Day2.part_1() == 36766
  end

  test "part 2" do
    assert Day2.part_2("5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5") == 9

    assert Tests.open_file("02.txt") |> Day2.part_2() == 261
  end
end
