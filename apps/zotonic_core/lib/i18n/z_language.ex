defmodule :z_language do
  def fallback_language(code, context) do
    fallback_language(:z_convert.to_binary(code), context)
  end

  _untranslated = "** 53: can't find include file \"zotonic.hrl\" **"

  def default_language(:undefined) do
    :en
  end

  def default_language(context) do
    :z_convert.to_atom(:m_config.get_value(:i18n, :language, :en, context))
  end

  def is_valid(code) do
    :z_language_data.is_language(code)
  end

  def to_language_atom(code) when is_binary(code) do
    case is_valid(code) do
      false ->
        {:error, not_a_language}

      true ->
        {:ok, :z_convert.to_atom(code)}
    end
  end

  def to_language_atom(code) do
    to_language_atom(:z_convert.to_binary(code))
  end

  def fallback_language(code) do
    :z_language_data.fallback(code)
  end

  def fallback_language(:undefined, context) do
    default_language(context)
  end

  def fallback_language(code, context) when is_binary(code) and is_atom(code) do
    case is_valid(code) do
      false ->
        default_language(context)

      true ->
        case :z_language_data.fallback(code) do
          [fallback | _] ->
            fallback

          [] ->
            default_language(context)
        end
    end
  end

  def english_name(code) do
    get_property(code, :name_en)
  end

  def is_rtl(code) do
    get_property(code, :direction) === "RTL"
  end

  def is_language_enabled(code, context) when is_atom(code) do
    :lists.member(code, enabled_language_codes(context))
  end

  def is_language_enabled(code, context) do
    case to_language_atom(code) do
      {:ok, lang} ->
        :lists.member(lang, enabled_language_codes(context))

      {:error, not_a_language} ->
        false
    end
  end

  def properties(code) when is_binary(code) and is_atom(code) do
    :maps.get(code, :z_language_data.languages_map_flat(), :undefined)
  end

  def properties(code) when is_list(code) do
    properties(:z_convert.to_binary(code))
  end

  def all_languages() do
    :z_language_data.languages_map_flat()
  end

  def main_languages() do
    :z_language_data.languages_map_main()
  end

  def language_list(context) do
    case :m_config.get(:i18n, :languages, context) do
      :undefined ->
        [{default_language(context), []} | []]

      cfg ->
        case :proplists.get_value(:list, cfg, []) do
          [] ->
            [{default_language(context), []} | []]

          l when is_list(l) ->
            l
        end
    end
  end

  def enabled_language_codes(context) do
    case :m_config.get(:i18n, :languages, context) do
      :undefined ->
        [default_language(context) | []]

      cfg when is_list(cfg) ->
        for {code, true} <- :proplists.get_value(list, cfg, []), do: code
    end
  end

  def get_property(code, key) do
    map = :maps.get(code, :z_language_data.languages_map_flat(), %{})
    :maps.get(key, map, :undefined)
  end
end
