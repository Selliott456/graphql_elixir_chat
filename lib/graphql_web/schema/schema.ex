defmodule GraphqlWeb.Schema do
  use Absinthe.Schema
  alias GraphqlWeb.Schema.Resolvers

  import_types(GraphqlWeb.Schema.Types.Types)

  @desc "greet"
  query do
    field :hello, :string do
      resolve(fn _, _, _ -> {:ok, "worlds"} end)
    end
  @dec "get all users"
    field :users, list_of(:user_type) do
      resolve(&Resolvers.UserResolver.get_all_users/3)
    end

    @desc "Get Me"
     field :get_me, :user_type do
      resolve(&Resolvers.UserResolver.get_me/3)
    end
  end
end
