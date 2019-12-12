defmodule :z_db_pgsql do
  _untranslated = "** 27: can't find include file \"zotonic.hrl\" **"
  _untranslated = "** 28: can't find include lib \"epgsql/include/epgsql.hrl\" **"

  def start_link(args) when is_list(args) do
    :gen_server.start_link(:z_db_pgsql, args, [])
  end

  def test_connection(args) do
    case try_connect_tcp(args) do
      :ok ->
        test_connection_1(args)

      {:error, _} = error ->
        error
    end
  end

  def test_connection_1(args) do
    case connect(args) do
      {:ok, conn} ->
        case :z_db.schema_exists_conn(conn, :proplists.get_value(:dbschema, args, 'public')) do
          true ->
            :epgsql.close(conn)
            :ok

          false ->
            :epgsql.close(conn)
            {:error, noschema}
        end

      {:error, _} = E ->
        E
    end
  end

  def squery(worker, sql, timeout) do
    :gen_server.call(worker, {:squery, sql}, timeout)
  end

  def equery(worker, sql, parameters, timeout) do
    :gen_server.call(worker, {:equery, sql, parameters}, timeout)
  end

  def get_raw_connection(record(:context, dbc: worker)) when worker !== :undefined do
    :gen_server.call(worker, :get_raw_connection)
  end

  def init(args) do
    process_flag(:trap_exit, true)
    {:ok, record(:state, conn: :undefined, conn_args: args), 60000}
  end

  def handle_call(cmd, from, record(:state, conn: :undefined, conn_args: args) = state) do
    case connect(args, from) do
      {:ok, conn} ->
        handle_call(cmd, from, record(:state, conn: conn))

      {:error, _} = e ->
        {:reply, e, state}
    end
  end

  def handle_call({:squery, sql}, _from, record(:state, conn: conn) = state) do
    {:reply, decode_reply(:epgsql.squery(conn, sql)), state, 60000}
  end

  def handle_call({:equery, sql, params}, _from, record(:state, conn: conn) = state) do
    {:reply, decode_reply(:epgsql.equery(conn, sql, encode_values(params))), state, 60000}
  end

  def handle_call(:get_raw_connection, _from, record(:state, conn: conn) = state) do
    {:reply, conn, state, 60000}
  end

  def handle_call(_request, _from, state) do
    {:reply, :unknown_call, state, 60000}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state, 60000}
  end

  def handle_info(:disconnect, record(:state, conn: :undefined) = state) do
    {:noreply, state}
  end

  def handle_info(:disconnect, state) do
    database = get_arg(:dbdatabase, record(:state, :conn_args))
    schema = get_arg(:dbschema, record(:state, :conn_args))
    :lager.debug('Closing connection to ~s/~s (~p)', [database, schema, self()])
    {:noreply, disconnect(state)}
  end

  def handle_info(:timeout, state) do
    {:noreply, disconnect(state)}
  end

  def handle_info({:EXIT, pid, _reason}, record(:state, conn: pid) = state) do
    {:noreply, record(:state, conn: :undefined)}
  end

  def handle_info({:EXIT, _pid, _reason}, record(:state, conn: :undefined) = state) do
    {:noreply, state, :hibernate}
  end

  def handle_info(_info, state) do
    {:noreply, state, 60000}
  end

  def terminate(_reason, record(:state, conn: :undefined)) do
    :ok
  end

  def terminate(_reason, record(:state, conn: conn)) do
    _ = :epgsql.close(conn)
    :ok
  end

  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  def try_connect_tcp(args) do
    addr = get_arg(:dbhost, args)
    port = get_arg(:dbport, args)
    sock_opts = [{:active, false}, {:packet, raw}, :binary]

    case :gen_tcp.connect(addr, port, sock_opts, 5000) do
      {:ok, sock} ->
        :gen_tcp.close(sock)
        :ok

      {:error, _} = error ->
        error
    end
  end

  def connect(args) when is_list(args) do
    connect(args, 0, :undefined)
  end

  def connect(args, {pid, _ref}) when is_list(args) do
    m_ref = monitor(:process, pid)
    result = connect(args, 0, m_ref)
    demonitor(m_ref)
    result
  end

  def connect(_args, retry_ct, _m_ref) when retry_ct >= 50 do
    {:error, econnrefused}
  end

  def connect(args, retry_ct, :undefined) do
    connect_1(args, retry_ct, :undefined)
  end

  def connect(args, retry_ct, m_ref) do
    receive do
      {:DOWN, m_ref, :process, _pid, _reason} ->
        {:error, caller_down}
    after
      0 ->
        connect_1(args, retry_ct, m_ref)
    end
  end

  def connect_1(args, retry_ct, m_ref) do
    hostname = get_arg(:dbhost, args)
    port = get_arg(:dbport, args)
    database = get_arg(:dbdatabase, args)
    username = get_arg(:dbuser, args)
    password = get_arg(:dbpassword, args)
    schema = get_arg(:dbschema, args)

    try do
      [
        case :epgsql.connect(hostname, username, password, database: database, port: port) do
          {:ok, conn} ->
            set_schema(conn, schema)

          {:error, record(:error, codename: :too_many_connections)} ->
            retry(args, :too_many_connections, retry_ct, m_ref)

          {:error, record(:error, codename: :out_of_memory)} ->
            retry(args, :out_of_memory, retry_ct, m_ref)

          {:error, record(:error, codename: :admin_shutdown)} ->
            retry(args, :admin_shutdown, retry_ct, m_ref)

          {:error, record(:error, codename: :crash_shutdown)} ->
            retry(args, :crash_shutdown, retry_ct, m_ref)

          {:error, record(:error, codename: :cannot_connect_now)} ->
            retry(args, :cannot_connect_now, retry_ct, m_ref)

          {:error, econnrefused} ->
            retry(args, :econnrefused, retry_ct, m_ref)

          {:error, _} = e ->
            :lager.warning('psql connection to ~p:~p returned error ~p', [hostname, port, e])
            e
        end
      ]
    catch
      {a, b, _} ->
        retry(args, {a, b}, retry_ct, m_ref)
    else
      []
    after
      []
    end
  end

  def set_schema(conn, schema) do
    case :epgsql.squery(conn, 'SET TIME ZONE \'UTC\'; SET search_path TO "' ++ schema ++ '"') do
      [{:ok, [], []}, {:ok, [], []}] ->
        {:ok, conn}

      error ->
        try do
          :epgsql.close(conn)
        rescue
          _ ->
            nil
        end

        {:error, error}
    end
  end

  def retry(args, reason, retry_ct, m_ref) do
    hostname = get_arg(:dbhost, args)
    port = get_arg(:dbport, args)
    delay = retry_delay(reason, retry_ct)

    :lager.warning('psql connection to ~p:~p failed: ~p, retrying in ~p ms (~p)', [
      hostname,
      port,
      reason,
      delay,
      self()
    ])

    maybe_close_connections(reason)
    :timer.sleep(delay)
    connect(args, retry_ct + 1, m_ref)
  end

  def maybe_close_connections(:out_of_memory) do
    :z_db_pool.close_connections()
  end

  def maybe_close_connections(:too_many_connections) do
    :z_db_pool.close_connections()
  end

  def maybe_close_connections(_) do
    :nop
  end

  def retry_delay(_, retry_count) when retry_count < 2 do
    10
  end

  def retry_delay(:too_many_connections, _) do
    10
  end

  def retry_delay(_, _retry_count) do
    10000
  end

  def disconnect(record(:state, conn: :undefined) = state) do
    state
  end

  def disconnect(record(:state, conn: conn) = state) do
    _ = :epgsql.close(conn)
    record(:state, :conn, :undefined)
  end

  def get_arg(k, args) do
    maybe_default(k, :proplists.get_value(k, args))
  end

  def maybe_default(:dbport, 0) do
    :z_config.get(:dbport)
  end

  def maybe_default(k, :undefined) do
    :z_config.get(k)
  end

  def maybe_default(k, []) do
    :z_config.get(k)
  end

  def maybe_default(k, "") do
    :z_config.get(k)
  end

  def maybe_default(_k, v) do
    v
  end

  def encode_values(l) when is_list(l) do
    :lists.map(&encode_value/1, l)
  end

  def encode_value(:undefined) do
    :null
  end

  def encode_value({:term, undefined}) do
    :null
  end

  def encode_value({:term, term}) do
    b = term_to_binary(term)
    # -define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).
    <<20_081_210::1>> <> b
  end

  def encode_value(value) do
    value
  end

  def decode_reply({:ok, columns, rows}) do
    {:ok, columns, :lists.map(&decode_values/1, rows)}
  end

  def decode_reply({:ok, nr, columns, rows}) do
    {:ok, nr, columns, :lists.map(&decode_values/1, rows)}
  end

  def decode_reply(r) do
    r
  end

  def decode_values(t) when is_tuple(t) do
    list_to_tuple(decode_values(tuple_to_list(t)))
  end

  def decode_values(l) when is_list(l) do
    :lists.map(&decode_value/1, l)
  end

  def decode_value({v}) do
    {decode_value(v)}
  end

  def decode_value(:null) do
    :undefined
  end

  # -define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).
  def decode_value((20_081_210 :: 1) <> b) do
    binary_to_term(b)
  end

  def decode_value({h, m, s}) when is_float(s) do
    {h, m, trunc(s)}
  end

  def decode_value({{y, mm, d}, {h, m, s}}) when is_float(s) do
    {{y, mm, d}, {h, m, trunc(s)}}
  end

  def decode_value(v) do
    v
  end
end
