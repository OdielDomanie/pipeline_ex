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
        {do_md, vals} =
          case do_block do
            {:__block__, do_md, vals} -> {do_md, vals}
            {:<-, line_md, val} -> {line_md, [{:<-, line_md, val}]}
          end

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
    lines =
      case do_block do
        {:__block__, _md, lines} -> lines
        line -> [line]
      end

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

  @doc """
  Executes multiple expressions in parallel and returns the results.

  Internally, spawns tasks for each corresponding line and awaits them.

  ## Examples
      parallel {1, 10} do
        {val, _} -> val * 2
        {_, val} -> val * 2
      end
      #=> {2, 20}
  """
  defmacro parallel(expr, do: do_block) do
    parallel_gen(expr, do: do_block)
  end

  @doc """
  Executes multiple expressions in parallel and returns the results, with timeout.

  The `parallel/2` macro has a default timeout of `5_000`.
  Use this macro to set a timeout in the format used in `Task` module.

  Example usage:

      parallel {1, 10}, timeout: 500 do
        <expressions...>
      end
  """
  defmacro parallel(expr, [timeout: timeout], do: do_block)
           when is_integer(timeout) or timeout == :infinity do
    parallel_gen(expr, timeout: timeout, do: do_block)
  end

  defp parallel_gen(expr, opts_n_do) do
    timeout = Keyword.get(opts_n_do, :timeout, 5000)
    do_block = Keyword.fetch!(opts_n_do, :do)

    matched_exprs =
      for line <- do_block do
        quote do
          case unquote(expr) do
            unquote([line])
          end
        end
      end

    task_exprs =
      for line <- matched_exprs do
        quote do
          Task.async(fn -> unquote(line) end)
        end
      end

    quote do
      [unquote_splicing(task_exprs)] |> Task.await_many(unquote(timeout))
    end
  end

  @doc """
  Pipes the expression into multiple branches, returns the results in a tuple.

  This is similar to `parallel/2`, but the branches are executes sequentially.

  ## Examples
      {1, 10}
      |> split do
        {val, _} -> val * 2
        {_, val} -> val * 2
      end
      #=> {2, 20}
  """
  defmacro split(expr, do: do_block) do
    anon_funs =
      for line <- do_block do
        quote do
          fn ->
            case unquote(expr) do
              unquote([line])
            end
          end
        end
      end

    quote do
      [unquote_splicing(anon_funs)]
      |> Enum.map(fn branch -> branch.() end)
      |> List.to_tuple()
    end
  end

  @doc """
  Only runs input if the given atom matches the first element.

  If the input expression returns a two element tuple and
  `ok :: atom()` matches the first element,
  the expression given to `do:` is returned,
  otherwise the input expression is returned as is.

  ## Examples

      {:ok, 6}
      |> only(:ok, do: div(2))
      #=> 3

      {:error, 2, 4}
      |> only(:ok, do: div(2))
      #=> {:error, 2, 4}
  """
  defmacro only(expr, ok, do: do_block) when is_atom(ok) do
    quote do
      (fn ->
         case unquote(expr) do
           {unquote(ok), only_val} -> only_val |> unquote(do_block)
           only_fail -> only_fail
         end
       end).()
    end
  end

  @doc """
  Pipe in if the first element is :ok, otherwise return input.

  `>>> expr` is equivalent to `|> only :ok, do: expr`
  """
  defmacro left >>> right do
    quote do
      (fn ->
         case unquote(left) do
           {:ok, only_val} -> only_val |> unquote(right)
           only_fail -> only_fail
         end
       end).()
    end
  end

  @doc """
  Assigns the piped in value to given variable name.

  ## Examples
      6 |> div(2) |> assign(a) |> div(3)
      #=> 1
      a
      #=> 3
  """
  defmacro assign(expr, var) do
    quote do
      var!(unquote(var)) = unquote(expr)
    end
  end
end
