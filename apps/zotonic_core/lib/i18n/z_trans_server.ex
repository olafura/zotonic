defmodule :z_trans_server do
  _untranslated = "** 36: can't find include lib \"zotonic.hrl\" **"

  def start_tests do
    :io.format('Starting trans server.~n')
    :gen_server.start_link({:local, :"z_trans_server$test"}, __MODULE__, :test, [])
  end

  def start_link(site) do
    name = :z_utils.name_for_site(__MODULE__, site)
    :gen_server.start_link({:local, name}, __MODULE__, {site, name}, [])
  end

  def load_translations(context) do
    ts = :z_trans.parse_translations(context)
    load_translations(ts, context)
  end

  def load_translations(trans, context) do
    name = :z_utils.name_for_site(__MODULE__, :z_context.site(context))
    :gen_server.cast(name, {:load_translations, trans})
  end

  def table(site) when is_atom(site) do
    :z_utils.name_for_site(__MODULE__, site)
  end

  def table(record(:context, []) = context) do
    record(:context, :translation_table)
  end

  def set_context_table(record(:context, []) = context) do
    record(:context, :translation_table, table(:z_context.site(context)))
  end

  def observe_module_ready(:module_ready, context) do
    load_translations(context)
  end

  def init({site, name}) do
    :lager.md([{:site, site}, {:module, __MODULE__}])
    process_flag(:trap_exit, true)
    :ok = :z_notifier.observe(:module_ready, {__MODULE__, observe_module_ready}, site)
    table = :ets.new(name, [:named_table, :set, :protected, {:read_concurrency, true} | []])
    {:ok, record(:state, table: table, site: site)}
  end

  def handle_call(message, __from, state) do
    {:stop, {:unknown_call, message}, state}
  end

  def handle_cast({:load_translations, trans}, state) do
    f = fn key, value, acc ->
      value1 =
        case :proplists.get_value(:en, value) do
          :undefined ->
            [{:en, key} | value]

          _ ->
            value
        end

      [{key, value1} | acc]
    end

    list = :maps.fold(f, [], trans)
    sync_to_table(list, record(:state, :table))
    :z_template.reset(record(:state, :site))
    {:noreply, state}
  end

  def handle_cast(message, state) do
    {:stop, {:unknown_cast, message}, state}
  end

  def handle_info(__info, state) do
    {:noreply, state}
  end

  def terminate(__reason, state) do
    :z_notifier.detach(:module_ready, record(:state, :site))
    :ok
  end

  def code_change(__old_vsn, state, __extra) do
    case state do
      {:state, table, __old_table} ->
        {:ok, record(:state, table: table)}

      _ ->
        {:ok, state}
    end
  end

  def sync_to_table(list, table) do
    lt = :lists.sort(:ets.tab2list(table))
    list1 = :lists.sort(list)
    sync(list1, lt, table)
  end

  def sync([], [], __table) do
    :ok
  end

  def sync(l, [], table) do
    :ets.insert(table, l)
  end

  def sync([], l, table) do
    :lists.map(fn {key, _} -> :ets.delete(table, key) end, l)
  end

  def sync([h | new_list], [h | old_list], table) do
    sync(new_list, old_list, table)
  end

  def sync([{k, v} | new_list], [{k, _} | old_list], table) do
    :ets.insert(table, [{k, v} | []])
    sync(new_list, old_list, table)
  end

  def sync([{k1, v1} | new_list], [{k2, _} | _] = old_list, table) when k1 < k2 do
    :ets.insert(table, [{k1, v1} | []])
    sync(new_list, old_list, table)
  end

  def sync([{k1, _} | _] = new_list, [{k2, _} | old_list], table) when k1 > k2 do
    :ets.delete(table, k2)
    sync(new_list, old_list, table)
  end
end
