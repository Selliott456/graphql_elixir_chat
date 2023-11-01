defmodule GraphqlWeb.Schema.Resolvers.UserResolver do
  def register_user(_, args, _) do
    IO.puts("args =>")
    IO.inspect(args)
    {:ok, true}
  end
end
