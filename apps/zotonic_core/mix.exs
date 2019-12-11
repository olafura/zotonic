# File: mix.exs
# This file was generated from rebar.config
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Zotonic.Core.Mixfile do
  use Mix.Project

  def project do
    [
      app: :zotonic_core,
      version: "0.0.1",
      elixir: "~> 1.2",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      applications: [
        :crypto,
        :public_key,
        :ssl,
        :inets,
        :lager,
        :mimetypes,
        :mnesia,
        :gproc,
        :jobs,
        :sidejob,
        :bert,
        :dh_date,
        :eiconv,
        :exometer_core,
        :epgsql,
        :depcache,
        :zotonic_stdlib,
        :cowboy,
        :cowmachine,
        :poolboy,
        :filezcache,
        :s3filez,
        :template_compiler,
        :qdate,
        :syslog,
        :bcrypt,
        :erlpass,
        :letsencrypt,
        :keyserver,
        :jsxrecord,
        :yamerl,
        :mqtt_sessions,
        :zotonic_notifier
      ],
      env: []
    ]
  end

  defp deps do
    [
      {:lager, "3.6.10", override: true},
      {:depcache, "1.5.0"},
      {:exometer_core, "1.5.7"},
      {:bert, "0.1.0"},
      {:dh_date, "1.0.0"},
      {:poolboy, "1.5.1"},
      {:epgsql, "3.4.0"},
      {:erlware_commons, "1.3.1", override: true},
      {:erlang_localtime, "1.0.0"},
      {:gproc, "0.8.0"},
      {:parse_trans, "3.3.0"},
      {:proper, "1.2.0"},
      {:recon, "2.4.0"},
      {:meck, "0.8.13"},
      {:edown, "0.8.1"},
      {:shotgun, "0.4.0"},
      {:bcrypt, "1.0.2", override: true},
      {:diffy, "1.0.0"},
      {:eiconv, "1.0.0"},
      {:erlpass, "1.0.4"},
      {:gen_smtp, git: "https://github.com/gen-smtp/gen_smtp.git", branch: "0.x"},
      {:mimetypes, "1.1.0"},
      {:mochiweb, "2.18.0"},
      {:jsx, "2.9.0", override: true},
      {:jsxrecord, "1.0.2"},
      {:sidejob, "2.1.0"},
      {:jobs, "0.9.0", override: true},
      {:filezcache, "1.0.0"},
      {:yamerl, "0.7.0"},
      {:zotonic_stdlib, "1.0.2", override: true},
      {:cowmachine, "1.2.2"},
      {:cowlib, "2.7.3", override: true},
      {:dispatch_compiler,
       git: "https://github.com/zotonic/dispatch_compiler.git", branch: "master"},
      {:template_compiler,
       git: "https://github.com/zotonic/template_compiler.git", branch: "master"},
      {:mqtt_sessions, git: "https://github.com/zotonic/mqtt_sessions.git", branch: "master"},
      {:s3filez, git: "https://github.com/mworrell/s3filez.git", branch: "master"},
      {:qdate, git: "https://github.com/choptastic/qdate.git", branch: "master"},
      {:exif, git: "https://github.com/nlfiedler/erlang-exif.git", branch: "master"},
      {:syslog, git: "https://github.com/Vagabond/erlang-syslog.git", branch: "master"},
      {:oauth, ~r/.*/, git: "https://github.com/tim/erlang-oauth.git", tag: "v1.6.0"},
      {:keyserver, ~r/.*/, git: "https://github.com/channelme/keyserver.git", branch: "master"}
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd([])
  end

  defp post_compile_hooks() do
    run_hook_cmd([])
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type()

    for command <- commands,
        do:
          (fn
             {regex, cmd} ->
               if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
                 Mix.Shell.cmd(cmd, [], fn x -> Mix.Shell.IO.info(trim(x)) end)
               end

             cmd ->
               Mix.Shell.cmd(cmd, [], fn x -> Mix.Shell.IO.info(trim(x)) end)
           end).(command)
  end

  defp trim(x) do
    if Version.compare(System.version(), "1.5.0") == :lt do
      Kernel.apply(String, :strip, [x])
    else
      Kernel.apply(String, :trim, [x])
    end
  end
end
