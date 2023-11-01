defmodule GraphqlWeb.Schema.Resolvers.UserResolver do

  alias Graphql.Auth

  def register_user(_, %{input: input}, _) do
    res = Auth.create_user(input)
    IO.inspect(res)
    {:ok, true}
  end
end
