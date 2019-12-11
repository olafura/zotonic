defmodule :zotonic_listen_http do
  def start_link() do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def await() do
    case :gen_server.call(__MODULE__, :status, :infinity) do
      :started ->
        :ok

      :init ->
        :timer.sleep(10)
        await()
    end
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    send(self(), :start)
    {:ok, :init}
  end

  def handle_call(:status, _srom, state) do
    {:reply, state, state}
  end

  def handle_call(_msg, _from, state) do
    {:reply, {:error, :unknown_call}, state}
  end

  def handle_cast(_ssg, state) do
    {:noreply, state}
  end

  def handle_info(:start, :init) do
    :ok = start_http_listeners()
    {:noreply, :started}
  end

  def code_change(_version, state, _extra) do
    {:ok, state}
  end

  def terminate(_why, :started) do
    stop_http_listeners()
  end

  def terminate(_why, :init) do
    :ok
  end

  def stop_http_listeners do
    :lists.foreach(&:cowboy.stop_listener/1, [
      :zotonic_http_listener_ipv4,
      :zotonic_https_listener_ipv4,
      :zotonic_http_listener_ipv6,
      :zotonic_https_listener_ipv6
    ])
  end

  def start_http_listeners do
    :z_ssl_certs.wait_for_dhfile()
    :application.set_env(:cowmachine, :server_header, :z_config.get(:server_header))
    start_http_listeners_ip4(:z_config.get(:listen_ip), :z_config.get(:listen_port))
    start_https_listeners_ip4(:z_config.get(:listen_ip), :z_config.get(:ssl_listen_port))

    case ipv6_supported() do
      true ->
        start_http_listeners_ip6(:z_config.get(:listen_ip6), :z_config.get(:listen_port))
        start_https_listeners_ip6(:z_config.get(:listen_ip6), :z_config.get(:ssl_listen_port))
        :ok

      false ->
        :ok
    end
  end

  def start_http_listeners_ip4(:none, _Port) do
    :lager.warning('HTTP server disabled: \'listen_ip\' is set to \'none\'')
    :ignore
  end

  def start_http_listeners_ip4(_WebIp, :none) do
    :lager.warning('HTTP server disabled: listen_port is set to \'none\'')
    :ignore
  end

  def start_http_listeners_ip4(web_ip, web_port) do
    :lager.info('HTTP server listening on IPv4 ~s:~p', [ip_to_string(web_ip), web_port])

    web_opt =
      case web_ip do
        :any ->
          []

        _ ->
          [ip: web_ip]
      end

    case(
      :cowboy.start_clear(
        :zotonic_http_listener_ipv4,
        %{
          max_connections: :z_config.get(:max_connections),
          num_acceptors: :z_config.get(:inet_acceptor_pool_size),
          socket_opts: [
            :inet,
            {:port, web_port},
            {:backlog, :z_config.get(:inet_backlog)} | web_opt
          ]
        },
        cowboy_options()
      )
    ) do
      {:ok, _} = ok ->
        ok

      {:error, {:already_started, pid}} ->
        {:ok, pid}
    end
  end

  def start_https_listeners_ip4(:none, _ssl_port) do
    :ignore
  end

  def start_https_listeners_ip4(_web_ip, :none) do
    :lager.info('HTTPS server disabled: \'ssl_listen_port\' is set to \'none\'')
    :ignore
  end

  def start_https_listeners_ip4(web_ip, ssl_port) do
    :lager.info('HTTPS server listening on IPv4 ~s:~p', [ip_to_string(web_ip), ssl_port])

    web_opt =
      case web_ip do
        :any ->
          []

        _ ->
          [ip: web_ip]
      end

    case(
      :cowboy.start_tls(
        :zotonic_https_listener_ipv4,
        %{
          max_connections: :z_config.get(:ssl_max_connections),
          num_acceptors: :z_config.get(:ssl_acceptor_pool_size),
          socket_opts:
            [:inet, {:port, ssl_port}, {:backlog, :z_config.get(:ssl_backlog)}] ++
              :z_ssl_certs.ssl_listener_options() ++ web_opt
        },
        cowboy_options()
      )
    ) do
      {:ok, _} = ok ->
        ok

      {:error, {:already_started, pid}} ->
        {:ok, pid}
    end
  end

  def start_http_listeners_ip6(:none, _web_port) do
    :ignore
  end

  def start_http_listeners_ip6(_web_ip, :none) do
    :ignore
  end

  def start_http_listeners_ip6(web_ip, web_port) do
    :lager.info('HTTP server listening on IPv6 ~s:~p', [ip_to_string(web_ip), web_port])

    web_opt =
      case web_ip do
        :any ->
          []

        _ ->
          [ip: web_ip]
      end

    {:ok, _} =
      :cowboy.start_clear(
        :zotonic_http_listener_ipv6,
        %{
          max_connections: :z_config.get(:max_connections),
          num_acceptors: :z_config.get(:inet_acceptor_pool_size),
          socket_opts:
            [
              :inet6,
              {:ipv6_v6only, true},
              {:port, web_port},
              {:backlog, :z_config.get(:inet_backlog)}
            ] ++ web_opt
        },
        cowboy_options()
      )
  end

  def start_https_listeners_ip6(:none, _ssl_port) do
    :ignore
  end

  def start_https_listeners_ip6(_web_ip, :none) do
    :ignore
  end

  def start_https_listeners_ip6(web_ip, ssl_port) do
    :lager.info('HTTPS server listening on IPv6 ~s:~p', [ip_to_string(web_ip), ssl_port])

    web_opt =
      case web_ip do
        :any ->
          []

        _ ->
          [ip: web_ip]
      end

    {:ok, _} =
      :cowboy.start_tls(
        :zotonic_https_listener_ipv6,
        %{
          max_connections: :z_config.get(:ssl_max_connections),
          num_acceptors: :z_config.get(:ssl_acceptor_pool_size),
          socket_opts:
            [
              :inet6,
              {:ipv6_v6only, true},
              {:port, ssl_port},
              {:backlog, :z_config.get(:ssl_backlog)}
            ] ++ :z_ssl_certs.ssl_listener_options() ++ web_opt
        },
        cowboy_options()
      )
  end

  def ip_to_string(:any) do
    'any'
  end

  def ip_to_string(ip) do
    :inet.ntoa(ip)
  end

  def cowboy_options do
    %{
      middlewares: [:cowmachine_proxy, :z_sites_dispatcher, :z_cowmachine_middleware],
      request_timeout: 60000,
      env: %{}
    }
  end

  def ipv6_supported do
    {:ok, _addr} = :inet.getaddr('localhost', :inet6)

    true
  rescue
    _ ->
      false
  end
end
