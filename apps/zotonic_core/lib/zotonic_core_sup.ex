defmodule :zotonic_core_sup do
  def start_link(options) do
    :lists.foreach(fn {k, v} -> :application.set_env(:zotonic_core, k, v) end, options)
    :z_config.init_app_env()
    :zotonic_core.setup()
    :z_stats.init()
    :z_tempfile_cleanup.start()
    ensure_job_queues()
    ensure_sidejobs()
    :z_ssl_certs.ensure_dhfile()
    mqtt_sessions_runtime()
    :inets.start(:httpc, profile: zotonic)
    :supervisor.start_link({:local, zotonic_core_sup}, :zotonic_core_sup, [])
  end

  def init([]) do
    spawn_delayed_status()
    :z_filehandler.start_observers()

    processes = [
      {:z_access_syslog, {:z_access_syslog, :start_link, []}, :permanent, 5000, :worker,
       [:z_access_syslog, :z_buffered_worker]},
      {:z_email_server, {:z_email_server, :start_link, []}, :permanent, 5000, :worker,
       [:z_email_server]},
      {:z_file_sup, {:z_file_sup, :start_link, []}, :permanent, 5000, :supervisor, :dynamic},
      {:z_sites_manager_sup, {:z_sites_manager_sup, :start_link, []}, :permanent, 10100,
       :supervisor, :dynamic},
      {:z_file_mtime, {:z_file_mtime, :start_link, []}, :permanent, 10100, :worker, :dynamic}
    ]

    {:ok, {{:one_for_one, 1000, 10}, processes}}
  end

  def spawn_delayed_status() do
    spawn(fn ->
      :timer.sleep(4000)
      :lager.info('================')
      :lager.info('Sites Status')
      :lager.info('================')
      sites_status = :maps.to_list(:z_sites_manager.get_sites())

      {running, other} =
        :lists.partition(fn {_site, status} -> status === :running end, sites_status)

      :lists.map(
        fn
          {site, :running} when site !== :zotonic_site_status ->
            ctx = :z_context.new(site)
            :lager.info('~p ~s ~-40s~n', [site, :running, :z_context.abs_url("/", ctx)])

          {site, status} ->
            :lager.info('~p - ~s~n', [site, status])
        end,
        running ++ other
      )

      :lager.info('================')
    end)
  end

  def ensure_job_queues() do
    ensure_job_queue(:media_preview_jobs, regulators: [counter: [limit: 3, modifiers: [cpu: 1]]])
    ensure_job_queue(:manage_module_jobs, regulators: [counter: [limit: 1]])
    :ok
  end

  def ensure_job_queue(name, options) do
    case(:jobs.queue_info(name)) do
      :undefined ->
        :jobs.add_queue(name, options)

      {:queue, _props} ->
        :ok
    end
  end

  def ensure_sidejobs() do
    :sidejob.new_resource(
      :zotonic_sessionjobs,
      :sidejob_supervisor,
      :z_config.get(:sessionjobs_limit)
    )

    :sidejob.new_resource(:zotonic_sidejobs, :sidejob_supervisor, :z_config.get(:sidejobs_limit))
  end

  def mqtt_sessions_runtime() do
    case :mqtt_sessions.runtime() do
      :mqtt_sessions_runtime ->
        :mqtt_sessions.set_runtime(:z_mqtt_sessions_runtime)

      _ ->
        :ok
    end
  end
end
