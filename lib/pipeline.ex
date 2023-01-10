defmodule Pipeline do
  @moduledoc """
  Contains helper macros for smoother pipelining of functions.
  """

  @doc """
  Like `|>`, but allows you to specify a position to feed the value in.

  ## Examples

    iex> import Pipeline
    iex> 2 ~> div(6, &1)
    3
  """
  defmacro val ~> fun do
    quote do
      unquote(val) |> (&unquote(fun)).()
    end
  end

  @doc """
  Pipe the output of `merge` to a function. See `merge/1`.
  """
  defmacro merge_expr ~>> fun do
    merge_expr = Macro.expand_once(merge_expr, __ENV__)

    case merge_expr do
      {:merge, _md, [[do: do_block]]} ->
        merge1(do_block, fun)

      {:merge, md, [arg1, [do: do_block]]} ->
        {:__block__, do_md, vals} = do_block

        {:__block__, do_md, [{:<-, md, [{:&, do_md, [1]}, arg1]} | vals]}
        |> merge1(fun)
    end
  end

  defp merge1(do_block, fun) do
    vals = merge_fun(do_block)

    {fun_name, md, args} = fun

    pipe_in_args =
      args
      |> Enum.with_index()
      |> Enum.filter(fn {arg, _i} -> match?({:&, _md, [at_no]} when is_integer(at_no), arg) end)
      |> Enum.map(fn {{:&, _md, [at_no]}, i} -> {i, Map.fetch!(vals, at_no)} end)

    given_args =
      Enum.filter(args, fn arg -> not match?({:&, _md, [at_no]} when is_integer(at_no), arg) end)

    part_fun = {fun_name, md, given_args}

    pipe_in(pipe_in_args, part_fun)
  end

  # needs the args ordered according to pos
  defp pipe_in([{pos, expr} | args_rest], fun) do
    pipe_in(args_rest, Macro.pipe(expr, fun, pos))
  end

  defp pipe_in([], fun), do: fun

  @doc """
  Pipes multiple values into a single function.

  ## Examples
      fun = fn a, b, c -> [a, b, c] end
      merge do
      &1 <- 1
      &2 <- 2
      ~>> fun.(3, &2, &1) == [3, 2, 1]
  """
  defmacro merge(do: do_block) do
    {:merge, [], [[do: do_block]]}
  end

  @doc """
  Similar to `merge/1`, allowing the first argument to be piped in.

      1 |> merge, do: &2 <- 2

  is equivalent to

      merge do
        &1 <- 1
        &2 <- 2
      end
  """
  defmacro merge(prior, do: do_block) do
    {:merge, [], [prior, [do: do_block]]}
  end

  defp merge_fun(do_block) do
    {:__block__, _md, lines} = do_block

    for {:<-, _md, [left, right]} <- lines do
      {:&, _md, [at_no]} = left
      {at_no, right}
    end
    |> Map.new()
  end

  @doc """
  The given function is applied to the value, and the same value is returned.
  Useful for side effect.

  ## Example
      iex> 1 |> Pipeline.passthrough(& &1 + 1)
      1
  """
  @spec passthrough(val, (val -> any())) :: val when val: any()
  def passthrough(val, fun) do
    fun.(val)
    val
  end

  @doc """
  Like `case`, but the left side of `->` is piped to the right.

  ## Examples
      > switch 6 do
      >  1 -> div(2)
      >  val -> div(3)
      > end
      2
  """
  defmacro switch(expr, do: do_block) do
    lines_piped =
      for {:->, md_line, [[left], right]} <- do_block do
        {:->, md_line, [[left], Macro.pipe(left, right, 0)]}
      end

    quote do
      case unquote(expr) do
        unquote(lines_piped)
      end
    end
  end
end
