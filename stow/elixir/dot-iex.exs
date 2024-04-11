# ~/.iex.exs - Config file for IEx (Elixir interactive shell)

if System.get_env("TERM") == "dumb" do
  IEx.configure(colors: [enabled: false])
end
