defmodule :zotonic_core do
  def is_testsandbox do
      case :erlang.atom_to_list(node()) do
      'zotonic001_testsandbox@' ++ _ ->
        true

      _ ->
        false
    end
  end

  def is_zotonic_project do
    is_app_available(:zotonic) and is_app_available(:zotonic_core)
  end

  def is_app_available(app) do
    case :code.which(app) do
      :non_existing ->
        false

      path when is_list(path) ->
        true
    end
  end

  def setup do
    :io.setopts(encoding: :unicode)
    assert_schedulers(:erlang.system_info(:schedulers))
    :z_jsxrecord.init()
    ensure_mnesia_schema()
  end

  def assert_schedulers(1) do
    :io.format(
      'FATAL: Not enough schedulers, please start with 2 or more schedulers.~nUse: ERLOPTS="+S 4:4" ./bin/zotonic debug~n~n'
    )

    :erlang.halt()
  end

  def assert_schedulers(_n) do
    :ok
  end

  def ensure_mnesia_schema() do
    case mnesia_dir() do
      {:ok, dir} ->
        case :filelib.is_dir(dir) and :filelib.is_regular(:filename.join(dir, 'schema.DAT')) do
          true ->
            :ok

          false ->
            :ok = :mnesia.create_schema([node()])
        end

      :undefined ->
        :lager.info(
          'No mnesia directory defined, running without persistent email queue and filezcache. To enable persistency, add to erlang.config: {mnesia,[{dir,"priv/mnesia"}]}'
        )

        :ok
    end
  end

  def mnesia_dir do
    :application.load(:mnesia)

    case is_testsandbox() do
      true ->
        :application.unset_env(:mnesia, :dir)
        :undefined

      false ->
        mnesia_dir_config()
    end
  end

  def mnesia_dir_config do
    case :application.get_env(:mnesia, :dir) do
      {:ok, :none} ->
        :undefined

      {:ok, []} ->
        :undefined

      {:ok, 'priv/mnesia'} ->
        mnesia_priv_dir()

      {:ok, dir} ->
        {:ok, dir}

      :undefined ->
        mnesia_priv_dir()
    end
  end

  def mnesia_priv_dir do
    priv_dir =
      case :code.priv_dir(:zotonic) do
        {:error, :bad_name} ->
          :code.priv_dir(:zotonic_core)

        zotonic_priv_dir when is_list(zotonic_priv_dir) ->
          zotonic_priv_dir
      end

    mnesia_dir_append_node(:filename.join([priv_dir, 'mnesia']))
  end

  def mnesia_dir_append_node(dir) do
    mnesia_dir = :filename.join([dir, :erlang.atom_to_list(node())])

    case :z_filelib.ensure_dir(mnesia_dir)  do
      :ok ->
        :application.set_env(:mnesia, :dir, mnesia_dir)
        {:ok, mnesia_dir}

      {:error, _} = error ->
        :lager.error('Could not create mnesia dir "~s": ~p', [mnesia_dir, error])
        :undefined
    end
  end
end
