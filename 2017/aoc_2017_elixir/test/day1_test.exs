defmodule Day1Test do
  use ExUnit.Case

  test "part 1" do
    assert Day1.part_1("1122") == 3
    assert Day1.part_1("1111") == 4
    assert Day1.part_1("1234") == 0
    assert Day1.part_1("91212129") == 9

    assert Tests.open_file("01.txt")
           |> Day1.part_1() ==
             1069
  end

  test "part 2" do
    assert Day1.part_2("1212") == 6
    assert Day1.part_2("1221") == 0
    assert Day1.part_2("123425") == 4
    assert Day1.part_2("123123") == 12
    assert Day1.part_2("12131415") == 4

    assert Tests.open_file("01.txt") |> Day1.part_2() == 1268
  end
end
