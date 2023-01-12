defmodule PipelineTest do
  use ExUnit.Case, async: true
  doctest Pipeline

  import Pipeline

  test "pipe into ith position" do
    fun = fn a, b, c -> [a, b, c] end

    res =
      :myval
      ~> fun.(1, &1, 3)

    assert res == [1, :myval, 3]
  end

  test "merge/1" do
    fun = fn a, b, c, d, e, f -> [a, b, c, d, e, f] end

    res =
      merge do
        &1 <- 1
        &2 <- 2
        &4 <- :q
        &3 <- [4, :w]
      end
      ~>> fun.(&1, &3, &1, &4, :r, &2)

    assert res == [1, [4, :w], 1, :q, :r, 2]

    fun = fn a, b, c -> [a, b, c] end

    res =
      merge do
        &1 <- 1
        &2 <- 2
      end
      ~>> fun.(3, &2, &1)

    assert res == [3, 2, 1]
  end

  test "merge/2" do
    fun = fn a, b, c, d, e, f -> [a, b, c, d, e, f] end

    res =
      (div(4, 4) / 1)
      |> merge do
        &2 <- 2
        &4 <- :q
        &3 <- [4, :w]
      end
      ~>> fun.(&1, &3, &1, &4, :r, &2)

    assert res == [1, [4, :w], 1, :q, :r, 2]
  end

  test "switch/2" do
    res =
      6
      |> switch do
        1 -> div(2)
        val -> div(3)
      end

    assert res == 2
  end

  test "split/2" do
    res =
      {1, 10}
      |> split do
        {val, _} -> val * 2
        {_, val} -> val * 2
      end

    assert res == [2, 20]
  end

  test "split/3" do
    res =
      split {1, 10}, timeout: 6_000 do
        {val, _} ->
          Process.sleep(5_500)
          val * 2

        {_, val} ->
          val * 2
      end

    assert res == [2, 20]
  end
end
