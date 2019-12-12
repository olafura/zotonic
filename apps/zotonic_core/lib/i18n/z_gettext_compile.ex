defmodule :z_gettext_compile do
  _untranslated = "** 55: can't find include lib \"zotonic.hrl\" **"

  def generate(filename, labels) do
    {:ok, fd} = :file.open(filename, [:write])
    write_header(fd)
    write_entries(fd, labels)
    :ok = :file.close(fd)
  end

  def write_entries(fd, labels) do
    lib_dir = :z_utils.lib_dir()

    f = fn {id, trans, finfo} ->
      :io.format(fd, '~n#: ~s~n', [fmt_fileinfo(finfo, lib_dir) | []])
      :file.write(fd, 'msgid ""\n')
      write_pretty(:unicode.characters_to_binary(id), fd)
      :file.write(fd, 'msgstr ""\n')
      write_pretty(:unicode.characters_to_binary(trans), fd)
    end

    :lists.foreach(f, labels)
  end

  def write_pretty(binary, fd) do
    :file.write(fd, wrap(escape_chars(binary)))
  end

  def wrap(binary) when byte_size(binary) <= 72 do
    <<"\"", binary, "\"\n">>
  end

  def wrap(binary) do
    "\"" <> wrap(:binary.split(binary, " ", [:global]), 72)
  end

  def wrap(parts, length) do
    {line, acc} =
      :lists.foldl(
        fn
          [part, {"", acc}] when byte_size(part) <= length ->
            {part, acc}

          [part, {line, acc}] when byte_size(line) + byte_size(part) <= length ->
            {<<line, " ", part>>, acc}

          part, {line, ""} ->
            {<<part>>, <<line>>}

          part, {line, acc} ->
            {<<part>>, <<acc, " \"\n\"", line>>}
        end,
        {"", ""},
        parts
      )

    <<acc, " \"\n\"", line, "\"\n">>
  end

  def fmt_fileinfo(finfo, lib_dir) do
    f = fn {fname0, line_no}, acc ->
      fname = :z_convert.to_list(fname0)

      fname1 =
        case :lists.prefix(lib_dir, fname) do
          true ->
            [46 | :lists.nthtail(length(lib_dir), fname)]

          false ->
            fname
        end

      iolist_to_binary([fname1 | [':', :z_convert.to_binary(line_no) | [acc | []]]])
    end

    :lists.foldr(f, "", finfo)
  end

  def write_header(fd) do
    :file.write(
      fd,
      '# SOME DESCRIPTIVE TITLE.\n# Copyright (C) YEAR THE PACKAGE\'S COPYRIGHT HOLDER\n# This file is distributed under the same license as the PACKAGE package.\n# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n#\n# NB: Consider using poEdit <http://poedit.sourceforge.net>\n#\n#\n#, fuzzy\nmsgid ""\nmsgstr ""\n"Project-Id-Version: PACKAGE VERSION\\n"\n"POT-Creation-Date: YEAR-MO-DA HO:MI+ZONE\\n"\n"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"\n"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"\n"Language-Team: LANGUAGE <LL@li.org>\\n"\n"MIME-Version: 1.0\\n"\n"Content-Type: text/plain; charset=utf-8\\n"\n"Content-Transfer-Encoding: 8bit\\n"\n'
    )
  end

  def parse_transform(form, opts) do
    case :lists.member(:gettext, opts) do
      true ->
        {__gettext__app__name, __gtxt_dir, _} = get_env()
        true
        true
        pt(form, opts)
        form

      _ ->
        form
    end
  end

  def get_env do
    {:os.getenv('gettext_tmp_name'), :os.getenv('gettext_dir'), :os.getenv('gettext_def_lang')}
  end

  def pt(form, opts) do
    put(:fname, [])
    pt(form, opts, :undefined)
  end

  def pt([h | t], opts, func) when is_list(h) do
    true
    f = fn x -> pt(x, opts, func) end
    [:lists.map(f, h) | pt(t, opts, func)]
  end

  def pt(
        {:call, l1, {:remote, l2, {:atom, l3, :gettext}, {:atom, l4, :key2str}},
         [{:string, l5, string} | []]},
        __opts,
        __func
      ) do
    true
    dump(string, l5)

    {:call, l1, {:remote, l2, {:atom, l3, :gettext}, {:atom, l4, :key2str}},
     [{:string, l5, string} | []]}
  end

  def pt(
        [
          {:call, _, {:remote, _, {:atom, _, :gettext}, {:atom, _, :key2str}},
           [{:string, l5, string} | []]} = h
          | t
        ],
        opts,
        func
      ) do
    true
    dump(string, l5)
    [h | pt(t, opts, func)]
  end

  def pt([{:attribute, _l, :module, mod} = h | t], opts, func) do
    put(:fname, :z_convert.to_binary(mod) <> ".erl")
    true
    [h | pt(t, opts, func)]
  end

  def pt([{:attribute, _l, :yawsfile, fname} = h | t], opts, func) do
    put(:fname, :z_convert.to_binary(fname))
    true
    [h | pt(t, opts, func)]
  end

  def pt([{:block, n, b} | t], opts, func) do
    true
    block = {:block, n, pt(b, opts, func)}
    [block | pt(t, opts, func)]
  end

  def pt([h | t], opts, func) when is_tuple(h) do
    true
    [while(size(h), h, opts, func) | pt(t, opts, func)]
  end

  def pt([h | t], opts, func) do
    true
    [h | pt(t, opts, func)]
  end

  def pt(t, opts, func) when is_tuple(t) do
    true
    while(size(t), t, opts, func)
  end

  def pt(x, _, _) do
    true
    x
  end

  def while(_, {:block, n, b}, opts, func) do
    {:block, n, pt(b, opts, func)}
  end

  def while(n, t, opts, func) when n > 0 do
    nt = setelement(n, t, pt(element(n, t), opts, func))
    while(n - 1, nt, opts, func)
  end

  def while(0, t, _, _) do
    t
  end

  def dump(str, l) do
    fname = get(:fname)
    finfo = get_file_info(str)
    :dets.insert(:gettext_table, {escape_chars(str), [{fname, l} | finfo]})
  end

  def get_file_info(key) do
    case :dets.lookup(:gettext_table, key) do
      [] ->
        []

      [{_, finfo} | _] ->
        finfo
    end
  end

  def escape_chars(binary) do
    escape_chars(binary, "")
  end

  def escape_chars("\"" <> t, acc) do
    escape_chars(t, acc <> "\\\"")
  end

  def escape_chars("\\" <> t, acc) do
    escape_chars(t, acc <> "\\\\")
  end

  def escape_chars("\n" <> t, acc) do
    escape_chars(t, acc <> "\\n")
  end

  def escape_chars(h <> t, acc) do
    escape_chars(t, acc <> h)
  end

  def escape_chars("", acc) do
    acc
  end
end
