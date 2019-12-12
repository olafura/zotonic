defmodule :z_gettext do
  def parse_pot(fname) do
    parse_po_1(fname, false)
  end

  def parse_po(fname) do
    parse_po_1(fname, true)
  end

  def parse_po_1(fname, drop_empty) do
    case :file.read_file(fname) do
      {:ok, bin} ->
        parse_po_bin(bin, drop_empty)

      {:error, _} = error ->
        :lager.error('Error reading po file ~p: ~p', [fname, error])
        []
    end
  end

  def parse_po_bin(bin) do
    parse_po_bin(bin, true)
  end

  def parse_po_bin(bin, drop_empty) do
    :lists.reverse(
      :lists.foldl(
        fn
          {"", r}, acc_in ->
            [{:header, r} | acc_in]

          [{_, ""}, acc_in] when drop_empty ->
            acc_in

          r, acc_in ->
            [r | acc_in]
        end,
        [],
        parse_po_part(bin)
      )
    )
  end

  def parse_po_part("msgid" <> t) do
    {key, r0} = get_po_string(t)
    {val, rest} = get_msgstr(r0)
    [{key, val} | parse_po_part(rest)]
  end

  def parse_po_part("#" <> t) do
    parse_po_part(skip_to_eol(t))
  end

  def parse_po_part(<<13, 10, t>>) do
    parse_po_part(t)
  end

  def parse_po_part(10 <> t) do
    parse_po_part(t)
  end

  def parse_po_part(13 <> t) do
    parse_po_part(t)
  end

  def parse_po_part(<<_::utf8, t>>) do
    parse_po_part(skip_to_eol(t))
  end

  def parse_po_part("") do
    []
  end

  def get_msgstr("msgstr" <> t) do
    get_po_string(t)
  end

  def get_msgstr(<<_::utf8, t>>) do
    get_msgstr(t)
  end

  def skip_to_eol("") do
    ""
  end

  def skip_to_eol("\r\n" <> t) do
    t
  end

  def skip_to_eol("\n" <> t) do
    t
  end

  def skip_to_eol("\r" <> t) do
    t
  end

  def skip_to_eol(<<_::utf8, t>>) do
    skip_to_eol(t)
  end

  def get_po_string(32 <> t) do
    get_po_string(t)
  end

  def get_po_string(13 <> t) do
    get_po_string(t)
  end

  def get_po_string(10 <> t) do
    get_po_string(t)
  end

  def get_po_string(9 <> t) do
    get_po_string(t)
  end

  def get_po_string(34 <> t) do
    eat_string(t)
  end

  def eat_string(s) do
    eat_string(s, "")
  end

  def eat_string(<<92, 34, t>>, acc) do
    eat_string(t, acc <> 34)
  end

  def eat_string(<<92, 92, t>>, acc) do
    eat_string(t, acc <> 92)
  end

  def eat_string(<<92, 110, t>>, acc) do
    eat_string(t, acc <> 10)
  end

  def eat_string(34 <> t, acc) do
    eat_more(t, acc)
  end

  def eat_string(h <> t, acc) do
    eat_string(t, acc <> h)
  end

  def eat_more(32 <> t, acc) do
    eat_more(t, acc)
  end

  def eat_more(10 <> t, acc) do
    eat_more(t, acc)
  end

  def eat_more(13 <> t, acc) do
    eat_more(t, acc)
  end

  def eat_more(9 <> t, acc) do
    eat_more(t, acc)
  end

  def eat_more(34 <> t, acc) do
    eat_string(t, acc)
  end

  def eat_more(t, acc) do
    {acc, t}
  end
end
